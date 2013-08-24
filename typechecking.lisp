(in-package :gradual)

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


