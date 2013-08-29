(in-package :gradual-common-lisp)

(defmacro defun (name args &body body)
  `(gradual::typed-defun ,name ,args ,@body))

(defmacro defparameter (name &optional value type)
  `(gradual::typed-defparameter ,name ,value ,type))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-external-symbols (s :common-lisp)
    (export (find-symbol (symbol-name s)) :gradual-common-lisp)))
