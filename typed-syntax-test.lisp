(defpackage :typed-syntax-test
  (:use :cl))

(in-package :typed-syntax-test)

(<t>:defun sum (x <integer> y <integer>) <integer>
  (+ x y))

(<t>:defun first-num (xs <list-of number>) <number>
  (first xs))
