(in-package :pluggable-types)

(defvar *type-checker* nil
  "The default type system to use.")

(defclass type-checker ()
  ())

(defgeneric type-checker-check-form (type-checker form &optional env))

(defun check-form (form &key env (type-checker *type-checker*))
  (unless type-checker
    (error "No type system selected. Set *TYPE-CHECKER*"))
  (type-checker-check-form type-checker form env))

(define-condition type-checking-error (simple-error)
  ((form :initarg :form
         :accessor error-form)
   (source :initarg :source
           :accessor error-source)
   (position :initarg :position
             :accessor error-position)))

(define-condition type-inconsistency-error (type-checking-error)
  ())
