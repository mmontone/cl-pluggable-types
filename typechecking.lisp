(in-package :gradual)

(defun typecheck ()
  (let ((env (make-typing-environment)))
    (loop for key being the hash-keys of *fun-sources*
       using (hash-value value)
       do
	 (progn
	   (format t "Typechecking ~A" key)
	   (%typecheck-form value env)))))

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
  (let ((bindings (bindings-of form)))
    (loop for binding in bindings
	 do (let ((initial-value (initial-value-of binding)))))))

(defmethod %typecheck-form ((form function-definition-form) typing-environment)
  (let* ((declarations (declares-of form))
	 (fun-env typing-environment)
	 (args-declarations
	  (remove-if-not
	   (lambda (declaration)
	     (typep declaration 'cl-walker::var-type-declaration-form))
	   declarations)))
    ;; We could just traverse the types in the (fun-type (name-of form))
    (loop for arg in (arguments-of form)
	 do (let ((arg-type (or (cl-walker::type-spec arg)
				(aand (find (name-of arg) args-declarations :key #'name-of :test #'equalp)
				      (cl-walker::type-of it))
				t)))
	      (setf fun-env (set-env-var-type fun-env (name-of arg) arg-type))))
    (%typecheck-form (body-of form) fun-env)
    (fun-type (name-of form))))

(defmethod %typecheck-form ((form cons) typing-environment)
  ;; We assume an implicit progn here
  (loop with type = t
     for f in form
     do (setf type
	  (%typecheck-form f typing-environment))
     finally (return type)))

(defmethod %typecheck-form ((form free-application-form) typing-environment)
  (let ((operator (operator-of form))
	(args (arguments-of form)))
    (let ((operator-type (fun-type operator)))
      (if (null operator-type)
	  ;; No type declared for operator, we are ok then
	  (progn
	    (format t "~%Warning: function ~A type has not been declared." operator)
	    (return-from %typecheck-form t))
	  ;; else, check the operator type signature matches the arguments types
	  (progn
	    (loop for arg in args
	       for formal-arg-type in (function-type-required-args-types operator-type)
	       do
		 (let ((actual-arg-type (%typecheck-form arg typing-environment)))
		   (when (not (or (equalp actual-arg-type t)
				  (subtypep actual-arg-type formal-arg-type)))
		     (format t "~%Type error in ~A: ~A has type ~A but ~A expected"
			     operator
			     (name-of arg)
			     actual-arg-type
			     formal-arg-type))))
	    (function-type-return-type operator-type))))))

(defmethod %typecheck-form ((form lexical-variable-reference-form) typing-environment)
  (or (env-var-type typing-environment (name-of form))
      t))
