(in-package :gradual)

(defclass gradual-type-system (pluggable-types::type-system)
  ())

(defclass gradual-typing-environment (pluggable-types::typing-environment)
  ((var-types :accessor typing-environment-var-types
              :initform nil)
   (fun-types :accessor typing-environment-fun-types
              :initform nil)))

(defmethod gradual::make-typing-environment ((type-system gradual-type-system))
  (make-instance 'gradual-typing-environment))

(defmethod copy-typing-environment ((env gradual-typing-environment))
  (let ((env-copy (make-typing-environment)))
    (setf (typing-environment-var-types env-copy)
          (copy-tree (typing-environment-var-types env)))
    (setf (typing-environment-fun-types env-copy)
          (copy-tree (typing-environment-fun-types env)))
    env-copy))

(defmethod type-of-function (fname (env gradual-typing-environment))
  (or (cdr (assoc fname (typing-environment-fun-types env)))
      'function))

(defmethod (setf type-of-function) (type fname (env gradual-typing-environment))
  (let ((old-type (type-of-function fname env)))
    (when old-type
      (warn "~A already has type ~A in env ~A" fname old-type env))
    (push (cons fname type)
          (typing-environment-fun-types env))
    env))

(defmethod type-of-var (varname (env gradual-typing-environment))
  (cdr
   (assoc varname (typing-environment-var-types env))))

(defmethod (setf type-of-var) (type varname (env gradual-typing-environment))
  (let ((old-type (type-of-var varname env)))
    (when old-type
      (warn "~A already has type ~A in env ~A" varname old-type env))
    (push (cons varname type)
          (typing-environment-var-types env))
    env))
