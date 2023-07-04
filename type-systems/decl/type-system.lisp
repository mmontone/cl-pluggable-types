(in-package :pluggable-types/decl)

(defclass decl-type-system (pluggable-types::type-system)
  ())

(defclass decl-typing-environment (pluggable-types::typing-environment)
  ())

(defmethod pluggable-types::make-typing-environment ((type-system decl-type-system))
  (make-instance 'decl-typing-environment))

(defmethod copy-typing-environment ((env decl-typing-environment))
  (let ((env-copy (make-instance 'decl-typing-environment)))
    env-copy))

(defmethod type-of-function (fname (env decl-typing-environment))
  (or (cdr (assoc fname (typing-environment-fun-types env)))
      'function))

(defmethod (setf type-of-function) (type fname (env decl-typing-environment))
  (let ((old-type (type-of-function fname env)))
    (when old-type
      (warn "~A already has type ~A in env ~A" fname old-type env))
    (push (cons fname type)
          (typing-environment-fun-types env))
    env))

(defmethod type-of-var (varname (env decl-typing-environment))
  (cdr
   (assoc varname (typing-environment-var-types env))))

(defmethod (setf type-of-var) (type varname (env decl-typing-environment))
  (let ((old-type (type-of-var varname env)))
    (when old-type
      (warn "~A already has type ~A in env ~A" varname old-type env))
    (push (cons varname type)
          (typing-environment-var-types env))
    env))
