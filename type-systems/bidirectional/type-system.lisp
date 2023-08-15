(in-package :pluggable-types/bid)

(defclass bid-type-system (pluggable-types::type-system)
  ())

(defclass bid-typing-environment (pluggable-types::typing-environment)
  ())

(defmethod pluggable-types::make-typing-environment ((type-system bid-type-system))
  (make-instance 'bid-typing-environment))

(defmethod copy-typing-environment ((env bid-typing-environment))
  (let ((env-copy (make-instance 'bid-typing-environment)))
    env-copy))

(defmethod type-of-function (fname (env bid-typing-environment))
  (or (cdr (assoc fname (typing-environment-fun-types env)))
      'function))

(defmethod (setf type-of-function) (type fname (env bid-typing-environment))
  (let ((old-type (type-of-function fname env)))
    (when old-type
      (warn "~A already has type ~A in env ~A" fname old-type env))
    (push (cons fname type)
          (typing-environment-fun-types env))
    env))

(defmethod type-of-var (varname (env bid-typing-environment))
  (cdr
   (assoc varname (typing-environment-var-types env))))

(defmethod (setf type-of-var) (type varname (env bid-typing-environment))
  (let ((old-type (type-of-var varname env)))
    (when old-type
      (warn "~A already has type ~A in env ~A" varname old-type env))
    (push (cons varname type)
          (typing-environment-var-types env))
    env))
