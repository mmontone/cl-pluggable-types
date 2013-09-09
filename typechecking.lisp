(in-package :gradual)

(defvar *debug* nil)

(defun enable-debugging (&optional (enable-p t))
  (setf *debug* enable-p))

(defun typecheck-everything (&optional (output *standard-output*))
  (when *debug*
    (format output "Typechecking started.~%"))
  (handler-bind ((gradual-type-error (lambda (type-error)
				       (format output "TYPE ERROR: ~A ~@{in ~A~}~%" type-error (source type-error))
				       (continue))))
    (let ((env (make-typing-environment)))
      (loop for key being the hash-keys of *fun-sources*
	 using (hash-value value)
	 do
	   (progn
	     (when *debug*
	       (format output "Typechecking ~A...~%" key))
	     (%typecheck-form value env))))
    (when *debug* (format output "Done.~%"))))

(defgeneric typecheck (thing &rest args)
  (:method ((form walked-form) &rest args)
    (apply #'typecheck-form form
	   args))
  (:method ((method typed-standard-method) &rest args)
    (declare (ignore args))
    (typecheck-typed-method method))
  (:method ((gf typed-standard-generic-function) &rest args)
    (declare (ignore args))
    (typecheck-typed-generic-function gf))
  (:method (thing &rest args)
    (declare (ignore args))
    (error "~A can not be type checked" thing)))

(defun typecheck-form (form &optional (typing-environment (make-typing-environment)))
  (let ((walked-form (walk-form form)))
    (%typecheck-form walked-form typing-environment)))

(defmethod %typecheck-form ((form progn-form) typing-environment)
  (loop
     with type = t
     for body-form in (body-of form)
     do
       (setf type (%typecheck-form body-form typing-environment))
       finally (return type)))

(defmethod %typecheck-form ((form let-form) typing-environment)
  (let ((bindings (bindings-of form))
	(let-env typing-environment))
    (loop for binding in bindings
	 do (let ((value (value-of binding))
		  (type (or (cl-walker::type-spec binding)
			    t)))
	      (let ((value-type (%typecheck-form value typing-environment)))
		(if (not (or (equalp value-type t)
			     (gradual-subtypep value-type type)))
		    (gradual-type-error (source-of form)
					"~A should have type ~A but is ~A"
					(name-of binding)
					type
					value-type))
		(setf let-env (set-env-var-type let-env
						(name-of binding)
						(or (cl-walker::type-spec binding)
						    value-type)
						)))))
    (%typecheck-form (body-of form) let-env)))

(defmethod %typecheck-form ((form let*-form) typing-environment)
  (let ((bindings (bindings-of form))
	(let*-env typing-environment))
    (loop for binding in bindings
	 do (let ((value (value-of binding))
		  (type (or (cl-walker::type-spec binding)
			    t)))
	      (let ((value-type (%typecheck-form value let*-env)))
		(if (not (or (equalp value-type t)
			     (gradual-subtypep value-type type)))
		    (gradual-type-error (source-of form)
					"~A should have type ~A but is ~A"
					(name-of binding)
					type
					value-type))
		(setf let*-env (set-env-var-type let*-env
						 (name-of binding)
						 (or (cl-walker::type-spec binding)
						     value-type)
						)))))
    (%typecheck-form (body-of form) let*-env)))

(defun check-return-type (form value-type type)
  (let ((type (or (and (listp type)
		       (equalp (first type) 'values)
		       (second type))
		  type)))
    (when (not (or (equalp value-type t)
		   (gradual-subtypep value-type type)))
      (gradual-type-error nil "~A should return ~A but ~A found."
			  (name-of form)
			  type
			  value-type))))


(defmethod %typecheck-form ((form function-definition-form) typing-environment)
  (let* ((declarations (declares-of form))
	 (fun-env typing-environment)
	 (args-declarations
	  (remove-if-not
	   (lambda (declaration)
	     (typep declaration 'cl-walker::var-type-declaration-form))
	   declarations))
	 (fun-type (fun-type (name-of form))))
    ;; We could just traverse the types in the (fun-type (name-of form))
    (loop for arg in (arguments-of form)
	 do (let ((arg-type (or (cl-walker::type-spec arg)
				(aand (find (name-of arg) args-declarations :key #'name-of :test #'equalp)
				      (cl-walker::type-of it))
				t)))
	      (setf fun-env (set-env-var-type fun-env (name-of arg) arg-type))))
    (let ((body-type
	   (or
	    (aand (body-of form)
		  (%typecheck-form it fun-env))
	    'null)))
      (check-return-type form body-type (return-type fun-type))
      fun-type)))

(defmethod %typecheck-form ((form cons) typing-environment)
  ;; We assume an implicit progn here
  (loop with type = t
     for f in form
     do (setf type
	  (%typecheck-form f typing-environment))
     finally (return type)))

(defmethod %typecheck-form ((form free-application-form) typing-environment)
  (flet ((check-argument-types (arg actual-arg-type formal-arg-type)
	   (when (not (or (equalp actual-arg-type t)
			  (gradual-subtypep actual-arg-type formal-arg-type)))
	     (gradual-type-error (source-of form)
				 "~A has type ~A but ~A expected"
				 (source-of arg)
				 actual-arg-type
				 formal-arg-type))))
  (let ((operator (operator-of form))
	(args (arguments-of form)))
    (let ((operator-type (fun-type operator)))
      (if (null operator-type)
	  ;; No type declared for operator, we are ok then
	  (progn
	    (when *debug* (format t "Warning: function ~A type has not been declared.~%" operator))
	    (return-from %typecheck-form t))
	  ;; else, check the operator type signature matches the arguments types
	  (let ((args (copy-list args)))
	    ;; required parameters
	    (loop for formal-arg-type in (required-args-types operator-type)
		
	       do (let* ((arg (pop args))
			 (actual-arg-type (%typecheck-form arg typing-environment)))
		    (check-argument-types arg actual-arg-type formal-arg-type)))
	    ;; optional parameters
	    (loop for formal-arg-type in (optional-args-types operator-type)
		
	       do (let ((arg (pop args)))
		    (when (null arg)
		      ;; Stop if the optional parameter is not being passed
		      (return nil))
		    (let ((actual-arg-type (%typecheck-form arg typing-environment)))
		      (check-argument-types arg actual-arg-type formal-arg-type))))
	    ;; keyword parameters
	    (when (keyword-args-types operator-type)
	      (loop while args
		   do
		   (let ((key (pop args))
			 (arg (pop args)))
		     (when (not key)
		       (return))
		     (assert (and (typep key 'constant-form)
				  (keywordp (value-of key)))
			     nil
			     "~A is not a keyword argument" key)
		     (let ((formal-arg-type (cdr (assoc (symbol-name (value-of key))
							(keyword-args-types operator-type) :key #'symbol-name :test #'equalp))))
		       (assert formal-arg-type nil "Keyword type for ~A not found" key)
		       (let ((actual-arg-type (%typecheck-form arg typing-environment)))
			 (check-argument-types arg actual-arg-type formal-arg-type)))
		     )))
	    ;; rest parameters
	    (awhen (rest-arg-type operator-type)
	      ;; rest args = args
	      (loop for arg in args
		   do (let ((actual-arg-type (%typecheck-form arg typing-environment)))
			(check-argument-types arg actual-arg-type it))))
	    
	    ;; return return-type
	    (return-type operator-type)))))))

(defmethod %typecheck-form ((form lexical-variable-reference-form) typing-environment)
  (or (env-var-type typing-environment (name-of form))
      t))

(defmethod %typecheck-form ((form constant-form) typing-environment)
  (type-of (value-of form)))

(defmethod %typecheck-form ((form if-form) typing-environment)
  (%typecheck-form (condition-of form) typing-environment)
  (let ((then-type (%typecheck-form (then-of form) typing-environment))
	(else-type (%typecheck-form (else-of form) typing-environment)))
    `(or ,then-type ,else-type)))

(defmethod %typecheck-form ((form the-form) typing-environment)
  (let ((value-type (%typecheck-form (value-of form) typing-environment))
	(declared-type (cl-walker::type-of form)))
    (when (not (or (equalp value-type t)
		   (equalp declared-type t)
		   (gradual-subtypep value-type declared-type)))
      (gradual-type-error (source-of form)
			  "~A has type ~A but ~A expected"
			  (value-of form)
			  value-type
			  declared-type))))

(defmethod %typecheck-form ((form flet-form) typing-environment)
  (let ((bindings (bindings-of form))
	(flet-env typing-environment))
    (loop for binding in bindings
       do
	 (let* ((declarations (declares-of form))
		(args-declarations
		 (remove-if-not
		  (lambda (declaration)
		    (typep declaration 'cl-walker::var-type-declaration-form))
		  declarations)))
	   (flet ((arg-type (arg)
		    (or (cl-walker::type-spec arg)
			(aand (find (name-of arg) args-declarations
				    :key #'name-of
				    :test #'equalp)
			      (cl-walker::type-of it))
			t)))
	     ;; Construct the local function type first
	     (let* ((required-args-types
		     (mapcar #'arg-type
			     (remove-if-not
			      (lambda (arg)
				(typep arg 'cl-walker::required-function-argument-form))
			      (arguments-of binding))))
		    (optional-args-types
		     (mapcar #'arg-type
			     (remove-if-not (lambda (arg)
					      (typep arg 'cl-walker::optional-function-argument-form))
					    (arguments-of binding))))
		    (keyword-args-types
		     (mapcar #'arg-type
			    (remove-if-not (lambda (arg)
					     (typep arg 'cl-walker::keyword-function-argument-form))
					   (arguments-of binding))))
		   (rest-arg-type
		    (aand
		     (find-if (lambda (arg)
				(typep arg 'cl-walker::rest-function-argument-form))
			      (arguments-of binding))
		     (arg-type it)))
		   (return-type (or (aand
				     (find-if
				      (lambda (declaration)
					(typep declaration 'cl-walker::return-type-declaration-form))
				      declarations)
				     (cl-walker::type-of it))
				    t))
		   (function-type (make-function-type :required-args-types required-args-types
						      :optional-args-types optional-args-types
						      :keyword-args-types keyword-args-types
						      :rest-arg-type rest-arg-type
						      :return-type return-type)))
  
	       ;; Then typecheck the local function

	       (let ((body-type
		      (or
		       (aand (body-of binding)
			     (%typecheck-form it flet-env))
		       'null)))
		 (check-return-type form body-type return-type)
		 
		 ;; Put the type of the local function in the typing environment
		 (setf flet-env (set-env-fun-type flet-env
						  (name-of binding)
						  function-type)))))))

    ;; Typecheck the function body in the new functions environment
    (%typecheck-form (body-of form) flet-env)))

(defmethod %typecheck-form ((form free-function-object-form) typing-environment)
  (fun-type (name-of form)))

(defmethod %typecheck-form ((form setq-form) typing-environment)
  (let ((value-type (%typecheck-form (value-of form) typing-environment))
	(var-type (or (env-var-type typing-environment (variable-of form))
		      t)))
    (when (not (or (equalp value-type t)
		   (equalp var-type t)
		   (gradual-subtypep value-type var-type)))
      (gradual-type-error (source-of form)
			  "~A has type ~A but ~A expected"
			  (value-of form)
			  value-type
			  var-type))))

(defmethod %typecheck-form ((form walked-lexical-function-object-form) typing-environment)
  (env-fun-type typing-environment (name-of form)))

(defmethod %typecheck-form ((form special-variable-reference-form) typing-environment)
  (var-type (name-of form)))

(defmethod %typecheck-form ((form lambda-function-form) typing-environment)
  ;; TODO: the following is wrong, until we have typed lambdas
  (fun (&rest t) t))

(defun typecheck-typed-generic-function (gf)
  )

(defun typecheck-typed-method (method)
  )
