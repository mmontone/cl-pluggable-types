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
  (loop for body-form in (body-of form)
       do
       (%typecheck-form body-form typing-environment)))

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
    (loop for arg in (arguments-of form)
       do (let ((arg-type (or (cl-walker::type-spec arg)
			      (aand (find (name-of arg) args-declarations :key #'name-of :test #'equalp)
				    (cl-walker::type-of it))
			      t)))
	    (setf fun-env (set-env-var-type fun-env (name-of arg) arg-type))))
    (%typecheck-form (body-of form) fun-env)))

(defmethod %typecheck-form ((form cons) typing-environment)
  ;; We assume an implicit progn here
  (loop for f in form
       do (%typecheck-form f typing-environment)))

(defmethod %typecheck-form ((form free-application-form) typing-environment)
  (let ((operator (operator-of form))
	(args (arguments-of form)))
    (let ((operator-type (fun-type operator)))
      (if (null operator-type)
	  ;; No type declared for operator, we are ok then
	  (return-from %typecheck-form)
	  ;; else, check the operator type signature matches the arguments types
	  (loop for arg in args
	     for arg-type in (function-type-required-args-types operator-type)
	     do (let ((env-arg-type (env-var-type typing-environment (name-of arg))))
		  (when env-arg-type 
		    (when (not (equalp env-arg-type t))
		      (when (not (subtypep env-arg-type arg-type))
			(format t "~%Type error in ~A: ~S has type ~A but ~A expected"
				operator
				(name-of arg)
				env-arg-type
				arg-type))))))))))


(defun infer-type (form)
  (%infer-type form
	       (make-typing-environment)))


