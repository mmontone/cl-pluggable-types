(in-package :pluggable-types)

(defgeneric infer-type (form type-system &rest args))

(defgeneric type-system-infer-type (type-system form typing-environment)
  (:documentation "Infer the type of FORM under TYPING-ENVIRONMENT."))

(defmethod type-system-infer-type :around (type-system form env)
  (let ((type (call-next-method)))
    (setf (inferred-type form env) type)
    type))
