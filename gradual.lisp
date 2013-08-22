(in-package :gradual)

(defparameter *var-types* (make-hash-table :test #'equalp))
(defparameter *fun-types* (make-hash-table :test #'equalp))
(defparameter *fun-sources* (make-hash-table :test #'equalp))
(defparameter *type-environment* (make-hash-table :test #'equalp))
(defparameter *walker-environment* (make-walk-environment))

(defstruct (function-type
	     (:print-function
	      (lambda (struct stream depth)
		(declare (ignore depth))
		(format stream "(FUNCTION (~A~A~A~A) ~A)"
			(format nil "~{~a~^ ~}" (function-type-required-args-types struct))
			(or (aand (function-type-optional-args-types struct)
				  (format nil " &optional ~{~a~^ ~}" it))
			    "")
			(or (aand (function-type-keyword-args-types struct)
				  (format nil " &key ~{~a~^ ~}" it))
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
   (make-function-type :rest-arg-type t :return-type t)))

(defun set-fun-source (fun-name source)
  (setf (gethash fun-name *fun-sources*) source))

(defun fun-source (fun-name)
  (gethash fun-name *fun-sources*))

(define-condition gradual-type-error (simple-error)
  ())

(defun gradual-type-error (message &rest args)
  (error 'gradual-type-error
	:format-control message
	:format-arguments args))

(defmacro $defparameter (var val &optional doc (type 't))
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
             (check-spec (spec what)
               (destructuring-bind (init suppliedp) spec
                 (declare (ignore init))
                 (check-variable suppliedp what nil)))
	     (check-type-spec (type)
	       (unless (symbolp type)
		 (simple-program-error "Invalid type spec ~A" type))))
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
		    (check-type-spec (second elt))
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
                              (setf elt (append elt '(,t)))))))
                    (t
                     (check-variable elt "optional parameter")
                     (when normalize-optional
                       (setf elt (cons elt '(nil ,t))))))
              (push (ensure-list elt) optional))
             (&rest
              (check-variable elt "rest parameter")
              (setf rest elt
                    state :after-rest))
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
                                ;(setf var-or-kv (list (make-keyword var-or-kv) var-or-kv))
				)))
                       (if (cdr tail)
                           (check-spec tail "keyword-supplied-p parameter")
                           (when normalize-keyword
                             (setf tail (append tail '(,t)))))
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
              (simple-program-error "Invalid ordinary lambda-list:~%  ~S" lambda-list)))))))
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

#+nil(defmacro $defun (name args return-type &body body)
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

(defmacro $defun (name args &body body)
  (multiple-value-bind (remaining-forms declarations doc-string)
      (parse-body body)
    (multiple-value-bind (function-type-declarations
			  var-type-declarations
			  return-type-declaration
			  other-declarations)
	(extract-type-declarations declarations)
      (declare (ignore other-declarations))
      (when function-type-declarations
	(warn "~A doesn't make sense here" function-type-declarations))
      (let* ((return-type (or (and return-type-declaration
				   (destructuring-bind (DECLARE (RETURN-TYPE type))
				       return-type-declaration
				     (declare (ignore declare return-type))
				     type))
			      t))
	     (args-types (multiple-value-bind (required-args optional-args
							     rest-arg key-args)
			     (parse-typed-lambda-list args)
			   (declare (ignore rest-arg))
			   (mapcar (lambda (arg)
				     (let ((declared-type (or
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
					   (lambda-list-type (aand (find arg (append required-args
										     optional-args
										     key-args)
									 :key #'first)
								   (if (equalp (length it) 2)
								       (second it)
								       (third it)))))
				       (when (not (or (and (equalp lambda-list-type t)
							   (equalp declared-type t))
						      (or (equalp lambda-list-type t)
							  (equalp declared-type t))))
					 (error "Duplicate type declaration for ~A" arg))
				       (or (and (equalp lambda-list-type t)
						declared-type)
					   (and (equalp declared-type t)
						lambda-list-type))))
				   (mapcar #'first (append required-args
							   optional-args
							   key-args)))))
	     ;; TODO: fix the function type
	     (function-type `(make-function-type :required-args-types ',args-types
						 :return-type ',return-type))
	     (fbody (if *runtime-type-assertions-enabled*
			`(progn
			   ,@(multiple-value-bind (required-args optional-args
								 rest-arg key-args)
						  (parse-typed-lambda-list args)
						  (declare (ignore rest-arg))
						  (loop for arg in (mapcar #'first (append required-args
											   optional-args
											   key-args))
						     for arg-type in args-types
						     when (not (equalp arg-type t))
						     collect `(check-gradual-type ,arg ',arg-type)))
			   ,(if (not (equalp return-type t))
				(with-unique-names (result)
				  `(let ((,result (progn ,@remaining-forms)))
				     (check-gradual-type ,result ',return-type)
				     ,result))
				`(progn ,@remaining-forms)))
			`(progn ,@remaining-forms))))
	`(progn
	   (set-fun-type ',name ,function-type)
	   (set-fun-source ',name (walk-form '(defun ,name ;; ,args  -- Use this eventually
					       ,(typed-lambda-list-to-normal args)
					       ,@body)))
	   (defun ,name ,(typed-lambda-list-to-normal args)
	     ,doc-string
	     ,@(remove-if (lambda (declaration)
			    (member declaration (list 'function-type 'return-type 'var-type)))
			  declarations :key #'caadr)
	     ,fbody))))))

(defvar *typechecking-enabled* nil)
(defvar *runtime-type-assertions-enabled* t)

(defun call-with-runtype-type-assertions (enabled-p function)
  (let ((*runtime-type-assertions-enabled* enabled-p))
    (funcall function)))

(defmacro without-runtime-type-assertions (&body body)
  `(call-with-runtime-type-assertions nil (lambda () ,@body)))

(defmacro with-runtime-type-assertions (&body body)
  `(call-with-runtime-type-assertions t (lambda () ,@body)))

(defun typecheck ())

(defun call-with-typechecking (enabled-p function)
  (let ((*typechecking-enabled* enabled-p))
    (funcall function)))

(defmacro without-typechecking (&body body)
  `(call-with-typechecking nil (lambda () ,@body)))

(defmacro with-typechecking (&body body)
  `(call-with-typechecking t (lambda () ,@body)))

(defstruct (typing-environment
	     (:print-function
	      (lambda (struct stream depth)
		(declare (ignore depth))
		(format stream "#S<TYPING-ENVIRONMENT VARS: ~A FUNS: ~A>"
			(typing-environment-var-types struct)
			(typing-environment-fun-types struct)))))
  var-types
  fun-types)

(declaim (ftype (function (typing-environment) typing-environment) copy-typing-environment))
(defun copy-typing-environment (env)
  (let ((env-copy (make-typing-environment)))
    (setf (typing-environment-var-types env-copy)
	  (copy-tree (typing-environment-var-types env)))
    (setf (typing-environment-fun-types env-copy)
	  (copy-tree (typing-environment-fun-types env)))
    env-copy))	  

(defun env-fun-type (env fun-name)
  (cdr (assoc fun-name (typing-environment-fun-types env))))

(defun set-env-fun-type (env fun-name type)
  (let ((new-env (copy-typing-environment env)))
    (let ((old-type (env-fun-type new-env fun-name)))
      (when old-type
	(warn "~A already has type ~A in env ~A" fun-name old-type new-env))
      (push (cons fun-name type) (typing-environment-fun-types new-env))
      new-env)))

(defun env-var-type (env var-name)
  (cdr
   (assoc var-name (typing-environment-var-types env))))

(defun set-env-var-type (env var-name type)
  (let ((new-env (copy-typing-environment env)))
    (let ((old-type (env-fun-type new-env var-name)))
      (when old-type
	(warn "~A already has type ~A in env ~A" var-name old-type new-env))
      (push (cons var-name type) (typing-environment-var-types new-env))
      new-env)))

(defun typecheck-form (form &optional (typing-environment (make-typing-environment)))
  (let ((walked-form (walk-form form)))
    (%typecheck-form walked-form typing-environment)))

(defmethod %typecheck-form ((form progn-form) typing-environment)
  (loop for body-form in (body-of form)
       do
       (%typecheck-form body-form typing-environment)))

(defmethod %typecheck-form ((form let-form) typing-environment)
  (let ((bindings (bindings-of form)))
    (loop for binding in bindings
	 do (let ((initial-value (initial-value-of binding)))))))

(defun infer-type (form)
  (%infer-type form
	       (make-typing-environment)))

(defgeneric %infer-type (form typing-environment)
  )

(defmethod %infer-type ((form constant-form) typing-environment)
  (type-of (value-of form)))

(defmethod %infer-type ((form free-application-form) typing-environment)
  (let ((function-type (fun-type (operator-of form)))
	(args-types (mapcar #'infer-type (arguments-of form))))
    (function-type-return-type function-type)))

(defmethod %infer-type ((form let-form) typing-environment)
  (let ((fresh-typing-environment typing-environment))
    (loop for binding in (bindings-of form)
	 do
	 (setf fresh-typing-environment
	       (set-env-var-type fresh-typing-environment
				 (name-of binding)
				 (if (not (equalp (type-of binding) t))
				     (%infer-type (value-of binding) typing-environment)
				     (type-of binding)))))
    (%infer-type (car (last (body-of form))) fresh-typing-environment)))

(defmethod %infer-type ((form walked-lexical-variable-reference-form) typing-environment)
  (env-var-type typing-environment (name-of form)))
