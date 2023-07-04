(in-package :gradual)

(defclass typing-environment ()
  ((declared-types :type hash-table
                   :documentation "A mapping of WALKED-FORM to their declared types."
                   :initform (make-hash-table)
                   :accessor declared-types)
   (inferred-types :type hash-table
                   :documentation "A mapping of WALKED-FORM to their inferred types."
                   :initform (make-hash-table)
                   :accessor inferred-types))
  (:documentation "A typing environment."))

(defmethod declared-type (walked-form (env typing-environment))
  (gethash walked-form (declared-types env)))

(defmethod (setf declared-type) (type walked-form (env typing-environment))
  (setf (gethash walked-form (declared-types env)) type))

(defmethod inferred-type (walked-form (env typing-environment))
  (gethash walked-form (inferred-types env)))

(defmethod (setf inferred-type) (type walked-form (env typing-environment))
  (setf (gethash walked-form (inferred-types env)) type))

(declaim (ftype (function (typing-environment) typing-environment) copy-typing-environment))

(defgeneric copy-typing-environment (env))
(defgeneric type-of-function (fname env))
(defgeneric (setf type-of-function) (value fname env))
(defgeneric type-of-var (varname env))
(defgeneric (setf type-of-var) (value varname env))
