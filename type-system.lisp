(in-package :gradual)

(defclass type-system ()
  ())

(defclass gradual-type-system (type-system)
  ())

(defgeneric make-typing-environment (type-system))

(defmethod make-typing-environment ((type-system gradual-type-system))
  (make-instance 'gradual-typing-environment))
