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

(defun enable-debugging (&optional (enable-p t))
  (setf *debug* enable-p))

(defstruct (function-type
	     (:print-function
	      (lambda (struct stream depth)
		(declare (ignore depth))
		(format stream "(FUN (~A~A~A~A) ~A)"
			(format nil "~{~a~^ ~}" (function-type-required-args-types struct))
			(or (aand (function-type-optional-args-types struct)
				  (format nil " &optional ~{~a~^ ~}" it))
			    "")
			(or (aand (function-type-keyword-args-types struct)
				  (with-output-to-string (s)
				    (format s " &key ")
				    (loop for (var . type) in it
				       do (format s "(~A ~A)" var type))))
			    "")
			(or (aand (function-type-rest-arg-type struct)
				  (format nil " &rest ~A" it))
			    "")
			(function-type-return-type struct)))))
  required-args-types
  optional-args-types
  keyword-args-types
  rest-arg-type
  return-type)
   

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
  `(set-fun-type ',fun-name ',type))

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

(defun parse-typed-lambda-list (lambda-list &key (normalize t)
					      allow-specializers
					      (normalize-optional normalize)
					      (normalize-keyword normalize)
					      (normalize-auxilary normalize))
  "Parses a gradual lambda-list, returning as multiple values:

1. Required parameters.

2. Optional parameter specifications, normalized into form:

   (name init suppliedp)

3. Name of the rest parameter, or NIL.

4. Keyword parameter specifications, normalized into form:

   ((keyword-name name) init suppliedp)

5. Boolean indicating &ALLOW-OTHER-KEYS presence.

6. &AUX parameter specifications, normalized into form

   (name init).

7. Existence of &KEY in the lambda-list.

Signals a PROGRAM-ERROR is the lambda-list is malformed."
  (let ((state :required)
        (allow-other-keys nil)
        (auxp nil)
        (required nil)
        (optional nil)
        (rest nil)
        (keys nil)
        (keyp nil)
        (aux nil))
    (labels ((fail (elt)
               (simple-program-error "Misplaced ~S in ordinary lambda-list:~%  ~S"
                                     elt lambda-list))
             (check-variable (elt what &optional (allow-specializers allow-specializers))
               (unless (and (or (symbolp elt)
                                (and allow-specializers
                                     (consp elt) (= 2 (length elt)) (symbolp (first elt))))
                            (not (constantp elt)))
                 (simple-program-error "Invalid ~A ~S in gradual lambda-list:~%  ~S"
                                       what elt lambda-list)))
	     (check-type-spec (type what)
	       (unless (symbolp type)
		 (simple-program-error "Invalid ~A type spec ~A in typed lambda list:~% ~S"
				       what
				       type
				       lambda-list)))
             (check-spec (spec what)
               (destructuring-bind (init &optional type suppliedp) spec
                 (declare (ignore init))
		 (when type
		   (check-type-spec type what))
		 (when suppliedp
		   (check-variable suppliedp what nil)))))
      (dolist (elt lambda-list)
        (case elt
          (&optional
           (if (eq state :required)
               (setf state elt)
               (fail elt)))
          (&rest
           (if (member state '(:required &optional))
               (setf state elt)
               (fail elt)))
          (&key
           (if (member state '(:required &optional :after-rest))
               (setf state elt)
               (fail elt))
           (setf keyp t))
          (&allow-other-keys
           (if (eq state '&key)
               (setf allow-other-keys t
                     state elt)
               (fail elt)))
          (&aux
           (cond ((eq state '&rest)
                  (fail elt))
                 (auxp
                  (simple-program-error "Multiple ~S in ordinary lambda-list:~%  ~S"
                                        elt lambda-list))
                 (t
                  (setf auxp t
                        state elt))
                 ))
          (otherwise
           (when (member elt '#.(set-difference lambda-list-keywords
                                                '(&optional &rest &key &allow-other-keys &aux)))
             (simple-program-error
              "Bad lambda-list keyword ~S in ordinary lambda-list:~%  ~S"
              elt lambda-list))
           (case state
             (:required
	      (if (listp elt)
		  (progn
		    (check-variable (first elt) "required parameter")
		    (check-type-spec (second elt) "required parameter")
		    (push elt required))
		  (progn
		    (check-variable elt "required parameter")
		    (push (list elt t) required))))
             (&optional
              (cond ((consp elt)
                     (destructuring-bind (name &rest tail) elt
                       (check-variable name "optional parameter")
                       (cond ((cdr tail)
                              (check-spec tail "optional-supplied-p parameter"))
                             (normalize-optional
                              (setf elt (append elt '(t)))))))
                    (t
                     (check-variable elt "optional parameter")
                     (when normalize-optional
                       (setf elt (cons elt '(nil t))))))
              (push (ensure-list elt) optional))
             (&rest
	      (if (consp elt)
		  (destructuring-bind (var type) elt
		    (check-variable var "rest parameter")
		    (check-type-spec type "rest parameter")
		    (setf rest elt))
		  ;; else
		  (progn
		    (check-variable elt "rest parameter")
		    (setf rest (list elt t))))
	      (setf state :after-rest))
             (&key
              (cond ((consp elt)
                     (destructuring-bind (var-or-kv &rest tail) elt
                       (cond ((consp var-or-kv)
                              (destructuring-bind (keyword var) var-or-kv
                                (unless (symbolp keyword)
                                  (simple-program-error "Invalid keyword name ~S in ordinary ~
                                                         lambda-list:~%  ~S"
                                                        keyword lambda-list))
                                (check-variable var "keyword parameter")))
                             (t
                              (check-variable var-or-kv "keyword parameter")
                              (when normalize-keyword
                                (setf var-or-kv (list (make-keyword var-or-kv) var-or-kv))
				)))
                       (if (cdr tail)
                           (check-spec tail "keyword-supplied-p parameter")
                           (when normalize-keyword
                             (setf tail (append tail '(t)))))
                       (setf elt (cons var-or-kv tail))))
                    (t
                     (check-variable elt "keyword parameter")
                     (setf elt (if normalize-keyword
                                   (list (list (make-keyword elt) elt) nil t)
                                   elt))))
              (push elt keys))
             (&aux
              (if (consp elt)
                  (destructuring-bind (var &optional init) elt
                    (declare (ignore init))
                    (check-variable var "&aux parameter"))
                  (progn
                    (check-variable elt "&aux parameter")
                    (setf elt (list* elt (when normalize-auxilary
                                           '(nil))))))
              (push elt aux))
             (t
              (simple-program-error "Invalid typed lambda-list:~%  ~S" lambda-list)))))))
    (values (nreverse required) (nreverse optional) rest (nreverse keys)
            allow-other-keys (nreverse aux) keyp)))

(defun typed-lambda-list-to-normal (typed-lambda-list)
  (multiple-value-bind (required optional rest keys allow-other-keys aux)
      (parse-typed-lambda-list typed-lambda-list)
    (append (mapcar #'first required)
	    (when optional
	      (cons '&optional
		    (mapcar (lambda (arg)
			      (list (first arg)
				    (second arg)))
			    optional)))
	    (when rest
	      (cons '&rest rest))
	    (when keys
	      (cons '&key
		    (mapcar (lambda (arg)
			      (list (first arg)
				    (second arg)))
			    keys)))
	    (when allow-other-keys
	      (cons '&allow-other-keys
		    allow-other-keys))
	    (when aux
	      (cons '&aux aux)))))

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

(defun extract-type-declarations (declarations)
  (let* ((function-type-declarations 
	  (remove-if-not (lambda (x)
			   (equalp x 'fun-type))
			 declarations
			 :key #'caadr))
	 (var-type-declarations
	  (remove-if-not (lambda (x)
			   (equalp x 'var-type))
			 declarations
			 :key #'caadr))
	 (return-type-declarations
	  (remove-if-not (lambda (x)
			   (equalp x 'return-type))
			 declarations
			 :key #'caadr))
	 (other-declarations (set-difference declarations
					     (append function-type-declarations
						     var-type-declarations
						     return-type-declarations))))
    (values function-type-declarations
	    var-type-declarations
	    (first return-type-declarations)
	    other-declarations)))

#+test(extract-type-declarations '((declare (ignore x))
			     (declare (var-type x integer))
			     (declare (fun-type f (integer -> integer)))
			     (declare (return-type integer))))

;; Comment: maybe we should consider replacing the :any type by t ? (the supertype of every type)

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
									 optional-args)
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
		 (rest-arg-type (when rest-arg (read-arg-type (mapcar #'first rest-arg))))
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
							  (mapcar (compose #'second #'first) key-args)
							  (when rest-arg (list rest-arg)))
				    for arg-type = (read-arg-type arg)
				    when (not (equalp arg-type t))
				    collect `(check-gradual-type ,arg ',arg-type))
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
		 (typecheck))
	       ',name)))))))

(defun parse-types-lambda-list (lambda-list &key (normalize t)
					      allow-specializers
					      (normalize-optional normalize)
					      (normalize-keyword normalize)
					      (normalize-auxilary normalize))
  "Parses a types lambda-list, returning as multiple values:

1. Required parameters.

2. Optional parameter specifications, normalized into form:

   (name init suppliedp)

3. Name of the rest parameter, or NIL.

4. Keyword parameter specifications, normalized into form:

   ((keyword-name name) init suppliedp)

5. Boolean indicating &ALLOW-OTHER-KEYS presence.

6. &AUX parameter specifications, normalized into form

   (name init).

7. Existence of &KEY in the lambda-list.

Signals a PROGRAM-ERROR is the lambda-list is malformed."
  (let ((state :required)
        (allow-other-keys nil)
        (auxp nil)
        (required nil)
        (optional nil)
        (rest nil)
        (keys nil)
        (keyp nil)
        (aux nil))
    (labels ((fail (elt)
               (simple-program-error "Misplaced ~S in types lambda-list:~%  ~S"
                                     elt lambda-list))
             (check-variable (elt what &optional (allow-specializers allow-specializers))
               (unless (and (or (symbolp elt)
                                (and allow-specializers
                                     (consp elt) (= 2 (length elt)) (symbolp (first elt))))
                            (not (constantp elt)))
                 (simple-program-error "Invalid ~A ~S in ordinary lambda-list:~%  ~S"
                                       what elt lambda-list)))
             (check-spec (spec what)
               (destructuring-bind (init suppliedp) spec
                 (declare (ignore init))
                 (check-variable suppliedp what nil)))
	     (check-type-spec (type what)
	       (unless (symbolp type)
		 (simple-program-error "Invalid ~A type spec ~A in types lambda list:~% ~S"
				       what
				       type
				       lambda-list))))
      (dolist (elt lambda-list)
        (case elt
          (&optional
           (if (eq state :required)
               (setf state elt)
               (fail elt)))
          (&rest
           (if (member state '(:required &optional))
               (setf state elt)
               (fail elt)))
          (&key
           (if (member state '(:required &optional :after-rest))
               (setf state elt)
               (fail elt))
           (setf keyp t))
          (&allow-other-keys
           (if (eq state '&key)
               (setf allow-other-keys t
                     state elt)
               (fail elt)))
          (&aux
           (cond ((eq state '&rest)
                  (fail elt))
                 (auxp
                  (simple-program-error "Multiple ~S in types lambda-list:~%  ~S"
                                        elt lambda-list))
                 (t
                  (setf auxp t
                        state elt))
                 ))
          (otherwise
           (when (member elt '#.(set-difference lambda-list-keywords
                                                '(&optional &rest &key &allow-other-keys &aux)))
             (simple-program-error
              "Bad lambda-list keyword ~S in types lambda-list:~%  ~S"
              elt lambda-list))
           (case state
             (:required
              (check-type-spec elt "required parameter")
              (push elt required))
             (&optional
	      (check-type-spec elt "optional parameter")
	      (push elt optional))
             (&rest
              (check-type-spec elt "rest parameter")
              (setf rest elt
                    state :after-rest))
             (&key
	      (when (not (and (consp elt)
			      (equalp (length elt) 2)))
		(simple-program-error "Invalid keyword type spec ~A in lambda-list:~% ~S"
				      elt
				      lambda-list))
	      (destructuring-bind (var type) elt
		(check-variable var "keyword parameter")
		(check-type-spec type "keyword parameter")
		(push (cons (car elt) (cadr elt)) keys)))
             (&aux
              (if (consp elt)
                  (destructuring-bind (var &optional init) elt
                    (declare (ignore init))
                    (check-variable var "&aux parameter"))
                  (progn
                    (check-variable elt "&aux parameter")
                    (setf elt (list* elt (when normalize-auxilary
                                           '(nil))))))
              (push elt aux))
             (t
              (simple-program-error "Invalid types lambda-list:~%  ~S" lambda-list)))))))
    (values (nreverse required) (nreverse optional) rest (nreverse keys)
            allow-other-keys (nreverse aux) keyp)))

(defun parse-function-type-spec (spec)
  (when (not (and (consp spec)
		  (equalp (first spec) 'fun)
		  (equalp (length spec) 3)))
    (simple-program-error "Invalid function type spec ~S" spec))
  (destructuring-bind (function args return-type) spec
    (declare (ignore function))
    (when (not (symbolp return-type))
      (simple-program-error "Invalid return type ~A in ~S" return-type spec))
    (multiple-value-bind (required-args-types
			  optional-args-types
			  rest-arg-type
			  keyword-args-types)
	(parse-types-lambda-list args)
      (make-function-type :required-args-types required-args-types
			  :optional-args-types optional-args-types
			  :keyword-args-types keyword-args-types
			  :rest-arg-type rest-arg-type
			  :return-type return-type))))

(defmacro fun (args-types return-type)
  `(parse-function-type-spec '(fun ,args-types ,return-type)))

(defmacro @ (&rest args)
    `(declare ,@args))
