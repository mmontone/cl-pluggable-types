(in-package :gradual)

(defparameter *var-types* (make-hash-table :test #'equalp))
(defparameter *fun-types* (make-hash-table :test #'equalp))
(defparameter *fun-sources* (make-hash-table :test #'equalp))
(defparameter *type-environment* (make-hash-table :test #'equalp))
(defparameter *walker-environment* (make-walk-environment))

(defvar *typechecking-enabled* nil "When true, typecheck after function definition")
(defvar *runtime-type-assertions-enabled* t "When true, insert runtime type checks")

(defun call-with-runtype-type-assertions (enabled-p function)
  (let ((*runtime-type-assertions-enabled* enabled-p))
    (funcall function)))

(defmacro without-runtime-type-assertions (&body body)
  `(call-with-runtime-type-assertions nil (lambda () ,@body)))

(defmacro with-runtime-type-assertions (&body body)
  `(call-with-runtime-type-assertions t (lambda () ,@body)))

(defun call-with-typechecking (enabled-p function)
  (let ((*typechecking-enabled* enabled-p))
    (funcall function)))

(defmacro without-typechecking (&body body)
  `(call-with-typechecking nil (lambda () ,@body)))

(defmacro with-typechecking (&body body)
  `(call-with-typechecking t (lambda () ,@body)))

(defun enable-typechecking (&optional (enable-p t))
  (setf *typechecking-enabled* enable-p))

;;(defmacro get-walker-template-internal (x)
;;  `(get ,x 'walker-template))

(defun set-var-type (var type)
  (setf (gethash var *var-types*) type))

(defun var-type (var)
  (or
   (gethash var *var-types*)
   t))

(defun set-fun-type (fun-name type)
  (setf (gethash fun-name *fun-types*) type))

(defmacro defun-type (fun-name type)
  `(set-fun-type ',fun-name ,type))

(defun fun-type (fun-name)
  (or
   (gethash fun-name *fun-types*)
   ;(make-function-type :rest-arg-type t :return-type t)
   nil))

(defun set-fun-source (fun-name source)
  (setf (gethash fun-name *fun-sources*) source))

(defun fun-source (fun-name)
  (gethash fun-name *fun-sources*))

(defmacro typed-defparameter (var val &optional doc (type 't))
  `(prog1
     (defparameter ,var ,val ,doc)
     (when (not (check-gradual-type ,var ',type))
       (gradual-type-error "~A is not of type ~A" ,var ',type))
     (set-var-type ',var ',type)))

(defun check-gradual-type (object type &key (throw-error-p t))
  (let ((type-check-result (%check-gradual-type (type-of object) type)))
    (when (and (not type-check-result) throw-error-p)
      (gradual-type-error "~A is not of type ~A" object type))
    type-check-result))

(defmethod %check-gradual-type (object-type type)
  (subtypep object-type type))





#+nil(defmacro typed-defun (name args return-type &body body)
  (multiple-value-bind (required-vars-spec)
      (parse-typed-lambda-list args)
    (let ((required-types (mapcar #'second required-vars-spec))
	  (required-vars (mapcar #'first required-vars-spec)))
      `(progn
	 (set-fun-type ',name
		       (list :required-vars ',required-types
			     :return ',return-type))
	 (defun ,name ,required-vars ,@body)))))

(defmacro typed-defun (name args &body body)
  (multiple-value-bind (remaining-forms declarations doc-string)
      (parse-body body)
    (declare (ignore remaining-forms))
    (multiple-value-bind (function-type-declarations
			  var-type-declarations
			  return-type-declaration
			  other-declarations)
	(extract-type-declarations declarations)
      (declare (ignore other-declarations))
      (multiple-value-bind (required-args
			    optional-args
			    rest-arg key-args)
	  (parse-typed-lambda-list args)
	(flet ((read-arg-type (arg)
		 (let ((declared-type
			(or
			 (aand
			  (find arg var-type-declarations
				:key (lambda (declaration)
				       (destructuring-bind (declare (var-type var type)) declaration
					 (declare (ignore declare var-type type))
					 var)))
			  (destructuring-bind (declare (var-type var type))
			      it
			    (declare (ignore declare var-type var))
			    type))
			 t))
		       (lambda-list-type (or (aand (or (find arg (append required-args
									 optional-args
									 (list rest-arg))
							     :key #'first)
						       (find arg key-args
							     :key (compose #'second #'first)))
						   (if (equalp (length it) 2)
						       (second it)
						       (third it)))
					     t)))
		   (when (not (or (and (equalp lambda-list-type t)
				       (equalp declared-type t))
				  (or (equalp lambda-list-type t)
				      (equalp declared-type t))))
		     (error "Duplicate type declaration for ~A. Declaration type: ~A lambda list type: ~A"
			    arg declared-type lambda-list-type))
		   (or (and (equalp lambda-list-type t)
			    declared-type)
		       (and (equalp declared-type t)
			    lambda-list-type)))))
	  (when function-type-declarations
	    (warn "~A doesn't make sense here" function-type-declarations))
	  (let* ((return-type (or (and return-type-declaration
				       (destructuring-bind (DECLARE (RETURN-TYPE type))
					   return-type-declaration
					 (declare (ignore declare return-type))
					 type))
				  t))
		 (required-args-types (mapcar #'read-arg-type (mapcar #'first required-args)))
		 (optional-args-types (mapcar #'read-arg-type (mapcar #'first optional-args)))
		 (keyword-args-types (mapcar (lambda (arg)
					       (cons arg
						     (read-arg-type arg)))
					     (mapcar (compose #'second #'first) key-args)))
		 (rest-arg-type (when rest-arg (read-arg-type (first rest-arg))))
		 (function-type `(make-function-type :required-args-types ',required-args-types
						     :optional-args-types ',optional-args-types
						     :keyword-args-types ',keyword-args-types
						     :rest-arg-type ',rest-arg-type
						     :return-type ',return-type))
		 (source (walk-form `(defun ,name ;; ,args  -- Use this eventually
					 ,(typed-lambda-list-to-normal args)
				       ,@body)))
		 (fbody (if *runtime-type-assertions-enabled*
			    `(progn
			       ,@(loop for arg in (append (mapcar #'first required-args)
							  (mapcar #'first optional-args)
							  (mapcar (compose #'second #'first) key-args))
				    for arg-type = (read-arg-type arg)
				    when (not (equalp arg-type t))
				    collect `(check-gradual-type ,arg ',arg-type))
			       ,@(when rest-arg
				       (with-unique-names (rest-elem)
					 (let ((arg-type (read-arg-type (first rest-arg))))
					   `((loop for ,rest-elem in ,(first rest-arg)
						do (check-gradual-type ,rest-elem ',arg-type))))))
			       ,@(if (not (equalp return-type t))
				     (with-unique-names (result)
				       `((let ((,result (progn ,@(mapcar #'unwalk-form (body-of source)))))
					   (check-gradual-type ,result ',return-type)
					   ,result)))
				     (mapcar #'unwalk-form (body-of source))))
			    `(progn ,@(mapcar #'unwalk-form (body-of source))))))
	    `(progn
	       (set-fun-type ',name ,function-type)
	       (set-fun-source ',name (walk-form '(defun ,name ;; ,args  -- Use this eventually
						   ,(typed-lambda-list-to-normal args)
						   ,@body)))
	       (defun ,name ,(typed-lambda-list-to-normal args)
		 ,@(when doc-string (list doc-string))
		 ,@(remove-if (lambda (declaration)
				(member declaration (list 'function-type 'return-type 'var-type)))
			      declarations :key #'caadr)
		 ,fbody)
	       (when *typechecking-enabled*
		 (typecheck-everything))
	       ',name)))))))
