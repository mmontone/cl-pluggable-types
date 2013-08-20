(defpackage gradual
  (:use :cl :cl-walker :anaphora :alexandria)
  (:export :fun-type
	   :var-type
	   :return-type
	   :defun-type
	   :typecheck))

(DEFPACKAGE :gradual-user
  (:shadow :defun :defparameter)
  (:USE :CL :gradual))


