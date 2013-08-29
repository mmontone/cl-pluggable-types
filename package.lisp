(defpackage gradual
  (:use :cl :cl-walker :anaphora :alexandria)
  (:export :fun-type
	   :var-type
	   :return-type
	   :defun-type
	   :typecheck
	   :infer-type
	   :fun
	   :gradual-type-error

	   ;; Types
	   :alist
	   :plist))

(defpackage :gradual-user
  (:shadow :defun :defparameter)
  (:use :cl :gradual))
