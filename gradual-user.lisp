(in-package :gradual-user)

(defmacro defun (name args &body body)
  `(gradual::$defun ,name ,args ,@body))

(defmacro defparameter (name &optional value type)
  `(gradual::$defparameter ,name ,value ,type))

(do-external-symbols (s :common-lisp)
  (export (find-symbol (symbol-name s)) :gradual-user))

