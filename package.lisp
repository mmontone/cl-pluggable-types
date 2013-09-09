(defpackage gradual
  (:use :cl :cl-walker :anaphora :alexandria :mw-equiv)
  (:export :fun-type
	   :var-type
	   :return-type
	   :defun-type
	   :typecheck
	   :typecheck-everything
	   :infer-type
	   :fun
	   :gradual-type-error
	   
	   ;; Types
	   :alist
	   :plist))
