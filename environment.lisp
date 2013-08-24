(in-package :gradual)

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

