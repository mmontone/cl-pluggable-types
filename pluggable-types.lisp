(in-package :pluggable-types)

(defvar *type-system* nil
  "The default type system to use.")

(defclass type-system ()
  ())

(defgeneric type-system-check-form (type-system form &optional env))

(defun check-form (form &key env (type-system *type-system*))
  (unless type-system
    (error "No type system selected. Set *TYPE-SYSTEM*"))
  (type-system-check-form type-system form env))

(define-condition type-checking-error (simple-error)
  ((form :initarg :form
         :accessor error-form)
   (source :initarg :source
           :accessor error-source)
   (position :initarg :position
             :accessor error-position)))

(define-condition type-inconsistency-error (type-checking-error)
  ())
