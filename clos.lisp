(in-package :gradual)

;; Ideas for polymorphism:

;; (typed-defclass (queue <a>) ()
;;    ((elems :initform nil
;; 	   :accessor elems
;; 	   :type (list <a>))))

;; ;; assume push has been shadowed from cl package
;; (typed-defmethod push ((elem <a>) (queue (queue <a>)))
;;   (declare (return-type (queue <a>)))
;;   (setf (elems queue) (cl:push elem (elems queue)))
;;   queue)

;; (typed-defmethod pop ((queue (queue <a>)))
;;    (declare (return-type <a>))
;;    (cl:pop (elems queue)))

(defmacro typed-defclass (name direct-superclasses direct-slots &rest options)
  `(progn
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)
     ,@(loop for slot in direct-slots
	    for props = (rest slot)
	    when (and (or (getf props :accessor)
			  (getf props :writer)
			  (getf props :reader))
		      (getf props :type))
	    appending (append
		       (when (getf props :accessor)
			 (list `(typed-defmethod ,(getf props :accessor) ((object ,name))
						 (declare (return-type ,(getf props :type)))
						 (slot-value object ',(first slot)))
			       `(typed-defmethod (setf ,(getf props :accessor))
						 ((value ,(getf props :type)) (object ,name))
						 (declare (return-type ,(getf props :type)))
						 (setf (slot-value object ',(first slot)) value)
						 value)))
		       (when (getf props :reader)
			 (list `(typed-defmethod ,(getf props :reader) ((object ,name))
						 (declare (return-type ,(getf props :type)))
						 (slot-value object ',(first slot)))))
		       (when (getf props :writer)
			 (list `(typed-defmethod (setf ,(getf props :writer))
						 ((value ,(getf props :type)) (object ,name))
						 (declare (return-type ,(getf props :type)))
						 (setf (slot-value object ',(first slot)) value)
						 value)))))))

(defmacro typed-defmethod (name args &body body)
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
	       (defmethod ,name ,(typed-lambda-list-to-normal args)
		 ,@(when doc-string (list doc-string))
		 ,@(remove-if (lambda (declaration)
				(member declaration (list 'function-type 'return-type 'var-type)))
			      declarations :key #'caadr)
		 ,fbody)
	       (when *typechecking-enabled*
		 (typecheck))
	       ',name)))))))
