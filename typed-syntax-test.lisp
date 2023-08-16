(defpackage :typed-syntax-test
  (:use :cl))

(in-package :typed-syntax-test)

(<t>:defparameter *my-param* <string> 22
   "This is my string param")

(<t>:defvar *my-var* <integer>
   "This is my var")

(<t>:defun sum (x <integer> y <integer>) <integer>
  (+ x y))

(<t>:defun first-num (xs <list-of number>) <number>
  (first xs))

(<t>:defun foo (x <list> &optional (y <integer> 22)
                                   (out <stream> *standard-output*)) <stream>
     *standard-output*)

(<t>:defun my-foo (x <integer> &key (error-p <boolean> t)) <t>
     t)
