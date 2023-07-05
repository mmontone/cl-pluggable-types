(in-package :pluggable-types/decl)

(pluggable-types:infer-type '(defun foo (x)
                              (+ x 22))
                            'decl-type-system)

(compiler-info:function-type '+) ;; => (function (&rest number) (values number &optional))

(compiler-info:function-type 'foo)

#|

Evaluates to: (function (t) (values number &optional))
The type of the argument is T, but could be inferred to NUMBER.

Plan to infer variables types:
Constrain the type variables from the argument types,
adding the argument-type to an AND type.
An AND type is valid if the common super-type of all its members is not T.
If the common supertype is T, then the AND type cannot be satisfied.
Use Common Lisp type hierarchy tree (https://sellout.github.io/2012/03/03/common-lisp-type-hierarchy) plus "Lower common ancestor" algorithm to find the common parent type.
But, problem with constraining with AND, is that it may be bad, consider the following:

(defun foo (x)
  (if (stringp x)
      (concatenate 'string "hello " x)
      (+ x 20)))

Would signal a type problem because X would be assigned type (AND STRING NUMBER), that is not satisfiable. So, we would need some kind of static flow analysis. Or, use a worse type, use OR, (OR NUMBER STRING).

|#

#| Parametric types
----------------

(deftype list-of (what)
    'list)  ;; fallback

|#

#| Polymorphic types
-----------------

(all (a) (function ((list-of a)) a))

(deftype all (typevars type)
  ,(replace-atoms typevars type)) ;; fallback

|#

(defun replace-atom (atom replacement exp)
  (cond
    ((and (atom exp) (eql atom exp))
     replacement)
    ((listp exp)
     (mapcar (lambda (subexp) (replace-atom atom replacement subexp)) exp))
    (t exp)))

;; (replace-atom 'x  't '((list-of x)))

(defun replace-atoms (atoms replacement exp)
  (let ((result exp))
    (dolist (atom atoms)
      (setq result (replace-atom atom replacement result)))
    result))

;; (replace-atoms '(a b) 't '(function (a b) a))
