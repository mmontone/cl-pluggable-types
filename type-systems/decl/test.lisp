(in-package :pluggable-types/decl)

(pluggable-types:infer-type '(defun foo (x)
                              (+ x 22))
                            'decl-type-system)

(compiler-info:function-type '+) ;; => (function (&rest number) (values number &optional))

(compiler-info:function-type 'foo)
;; Evaluates to: (function (t) (values number &optional))
;; The type of the argument is T, but could be inferred to NUMBER.

;; Plan to infer variables types:
;; Constrain the type variables from the argument types,
;; adding the argument-type to an AND type.
;; An AND type is valid if the common super-type of all its members is not T.
;; If the common supertype is T, then the AND type cannot be satisfied.
;; Use Common Lisp type hierarchy tree (https://sellout.github.io/2012/03/03/common-lisp-type-hierarchy) plus "Lower common ancestor" algorithm to find the common parent type.


