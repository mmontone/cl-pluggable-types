(in-package :gradual)

(defvar *debug* nil)

(defun typecheck (&optional (output *standard-output*))
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

(defun more-specific-type (type1 type2)
  (if (not (or (subtypep type1 type2)
	       (subtypep type2 type1)))
	   (error "Cant decide between ~A and ~A" type1 type2))
  (if (subtypep type1 type2)
      type1
      type2))

(defmethod %typecheck-form ((form let-form) typing-environment)
  (let ((bindings (bindings-of form))
	(let-env typing-environment))
    (loop for binding in bindings
	 do (let ((value (value-of binding))
		  (type (or (cl-walker::type-spec binding)
			    t)))
	      (let ((value-type (%typecheck-form value typing-environment)))
		(if (not (or (equalp value-type t)
			     (subtypep value-type type)))
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
	   (%typecheck-form (body-of form) fun-env)))
      (when (not (or (equalp body-type t)
		     (subtypep body-type (function-type-return-type fun-type))))
	(gradual-type-error nil "~A should return ~A but ~A found."
			    (name-of form)
			    (function-type-return-type fun-type)
			    body-type))
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
			  (subtypep actual-arg-type formal-arg-type)))
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
	    (loop for formal-arg-type in (function-type-required-args-types operator-type)
		
	       do (let* ((arg (pop args))
			 (actual-arg-type (%typecheck-form arg typing-environment)))
		    (check-argument-types arg actual-arg-type formal-arg-type)))
	    ;; optional parameters
	    (loop for formal-arg-type in (function-type-optional-args-types operator-type)
		
	       do (let ((arg (pop args)))
		    (when (null arg)
		      ;; Stop if the optional parameter is not being passed
		      (return nil))
		    (let ((actual-arg-type (%typecheck-form arg typing-environment)))
		      (check-argument-types arg actual-arg-type formal-arg-type))))
	    ;; keyword parameters
	    (when (function-type-keyword-args-types operator-type)
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
		     (let ((formal-arg-type (cdr (assoc (value-of key)
							(function-type-keyword-args-types operator-type)))))
		       (assert formal-arg-type nil "Keyword type for ~A not found" key)
		       (let ((actual-arg-type (%typecheck-form arg typing-environment)))
			 (check-argument-types arg actual-arg-type formal-arg-type)))
		     )))
	    ;; rest parameters
	    (awhen (function-type-rest-arg-type operator-type)
	      ;; rest args = args
	      (loop for arg in args
		   do (let ((actual-arg-type (%typecheck-form arg typing-environment)))
			(check-argument-types arg actual-arg-type it))))
	    
	    ;; return return-type
	    (function-type-return-type operator-type)))))))

(defmethod %typecheck-form ((form lexical-variable-reference-form) typing-environment)
  (or (env-var-type typing-environment (name-of form))
      t))

(defmethod %typecheck-form ((form constant-form) typing-environment)
  (type-of (value-of form)))
