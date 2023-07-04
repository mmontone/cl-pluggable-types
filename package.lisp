(defpackage gradual
  ;; TODO: use 'pluggable-types' as package name, not nickname
  (:nicknames :pluggable-types)
  (:use :cl :cl-walker :anaphora :alexandria :mw-equiv)
  (:export #:fun-type
	   #:var-type
	   #:return-type
	   #:defun-type
	   #:typecheck
	   #:typecheck-everything
	   #:infer-type
	   #:fun
	   #:gradual-type-error
           #:type-system
	   #:gradual-type-system
	   ;; Types
	   #:alist
	   #:plist))
