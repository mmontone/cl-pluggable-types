(in-package :pluggable-types/decl)

;; https://www.cs.cornell.edu/courses/cs3110/2011sp/Lectures/lec26-type-inference/type-inference.htm

(defun unify-one (term1 term2)
  
  )

(defun unify (constraints)
  "Unify CONSTRAINTS."
  )

#|
(unify '((var x) . (var y))) => '((x . (var y)))
(unify '((var x) . integer)) => '((x . integer))
(unify '(integer . (var x))) => '((x . integer))
(unify '((var x) . (list-of integer)) => '((x . (list-of integer)))
(unify '((all (a) (list-of a)) . integer)) => '((a . integer))
(unify '(integer . (all (a) (list-of a)))) => '((a . integer))

(unify '(integer . boolean)) => error
(unify '((var x) . integer)  ((var x) . boolean)) => error

The steps:
(unify '((var x) . integer) ((var x) . boolean)) =>
(unify '(integer . boolean)) => error

mapcar :: (all (a b) (function ((function (a) b) (list-of a)) (list-of b)))

+ :: (function (&rest number) number)

(mapcar + (list 1 2 3))

inference of (mapcar + (list 1 2 3 x)):

(unify '((all (a b) (function (a) b)) (function (&rest number) number))
       '((all (a b) (list-of b)) (list-of number)))
=>
(unify '((a . (&rest number)) (b . number))
       '((b . number)))
=> '((a . number) (b . number))

------------------

(unify '(integer . (list-of (var-type a)))) => 

|#


;; Problem with inferencing to or:
(defun or-test (x)
  (concatenate 'string x "hello"))

(compiler-info:function-type 'concatenate)
(compiler-info:function-type 'or-test)

(defun or-test (x)
  (concatenate 'string (1+ x) "hello"))

(defun or-test (x)
  (declare (type integer x))
  (concatenate 'string x "hello"))

(defun or-test (x)
  (concatenate 'string (the integer x) "hello"))

(describe #'or-test)

;; Like this??:
;; Unify type variables first?
;; Then unify bindings -> inference
;; Then unify with declared types -> type checking

(defun my-func (x list)
  (concatenate 'list (mapcar 'some-func list) x))

;; some-func :: a -> b
;; mapcar :: (c -> d) -> list-of c -> list-of d

;; unify (c -> d) with (a -> b) = a is c, d is b
;; (mapcar 'some-func list) :: (apply substitution (type-of mapcar)) =
;; (a -> b) -> list-of a -> list-of b

;; unify list-of a with sequence => list-of a (keep the more specific type)?

#|

Generation of type constraints for an expression e

In a type environment, assign a unique type variable to each variable x ocurring in e
Assign a unique type variable to each subexpression of e

Constraints:

 Call the type variable assigned to x,  u(x), and call the type variable assigned to occurrence of a subexpression e', v(e').

Now we take the following constraints:

* u(x) = v(x) for each occurrence of a variable x.
* v(e1) = v(e2) -> v((e1 e2)) for each occurrence of a subexpression (e1 e2).
* v(fun x -> e) = v(x) -> v(e) for each occurrence of a subexpression fun x -> e.

* For each occurrence of a variable x, u(x) = v(x) .
* For each occurrence of function application (fn e), v(fn) = (function (v(e)) v(fn e))
* For each occurrence of a subexpression (lambda (x) e), v(lambda (x) e) = (function (v(x)) (v(e))).


|#
