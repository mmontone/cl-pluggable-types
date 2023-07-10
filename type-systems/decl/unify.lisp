(in-package :pluggable-types/decl)

;; https://www.cs.cornell.edu/courses/cs3110/2011sp/Lectures/lec26-type-inference/type-inference.htm

(defun unify-one (term1 term2)
  (trivia:match (list term1 term2)
    ((list (list 'var x) (list 'var y))
     (cons x (list 'var y)))
    ((list type (list 'var x))
     (cons x type))
    ((list (list 'var x) type)
     (cons x type))
    ((list type1 type2)
     (error "no unify: ~s ~s" type1 type2))))

;; (defun subst (sub term)
;;   (trivia:match (list sub term)
;;     ((list (cons (list 'var x) s)
;;            (list 'var y))
;;      (if (eql x y) s term))
;;     ((list (cons (list 'var x) s)
;;            term)
;;      (if (listp term)
;;          (mapcar (lambda (subterm)
;;                    (subst sub subterm))
;;                  term)
;;          term))))

(defun subst-term (assignment term)
  (or (trivia:match (list assignment term)
          ((list (cons varname val) (list 'var x))
           (if (eql varname x)
               val
               term)))
       (cond
         ((listp term)
          (cons (car term) (mapcar (curry 'subst-term assignment) (cdr term))))
         (t
          term))))

;; A substituion is a list of assignments
(defun apply-substitution (assignments term)
  (let ((new-term term))
    (dolist (assignment assignments)
      (setq new-term (subst-term assignment new-term)))
    new-term))

(defun unify (constraints)
  "Unify CONSTRAINTS."
  (when constraints
    (let* ((substitution (unify (rest constraints)))
           (constraint (first constraints))
           (sub2
             (unify-one (apply-substitution substitution (car constraint))
                        (apply-substitution substitution (cdr constraint)))))
      (append substitution (list sub2)))))

(trace unify)
(trace unify-one)
      
#|      
(unify '(((var x) . (var y)))) => '((x . (var y)))
(unify '(((var x) . integer))) => '((x . integer))
(unify '((integer . (var x)))) => '((x . integer))
(unify '(((var x) . (list-of integer)))) => '((x . (list-of integer)))
(unify '((all (a) (list-of a)) . integer)) => '((a . integer))
(unify '(integer . (all (a) (list-of a)))) => '((a . integer))

(unify '(((var x) . integer) ((var x) . boolean))) => error
(unify '(((var x) . integer) ((var y) . boolean) ((var x) . (var y))))

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

#|

(generate '(+ 12 x))

Env: + -> g1, x -> g2, 12 -> g3, (+ 12 x) -> g4, (g1, .., gN) type variables.

g2 = g2
g2 = integer
g3 = g3
g4 = integer
g1 = (function (g2 g3) g4)

Unify:
g1 = (function (integer g3) integer)

|#

(defstruct type-env
  (symbol-nr 0)
  (vars nil)
  (constraints nil)
  (unified nil))

(defun new-var (form env)
  (let* ((varname (intern (format nil "VAR~a" (incf (type-env-symbol-nr env)))))
         (var
          (list 'var varname)))
    (push (cons varname form) (type-env-vars env))
    var))

(defun add-constraint (x y env)
  (push (cons x y) (type-env-constraints env)))

(defgeneric generate-type-constraints (form env locals))

(defmethod generate-type-constraints ((form constant-form) env locals)
  (let ((var (new-var form env)))
    (add-constraint var (type-of (value-of form)) env)
    var))

(defmethod generate-type-constraints ((form let-form) env locals)
  (let ((let-locals locals))
    (dolist (binding (bindings-of form))
      (let ((binding-value-var
              (generate-type-constraints (initial-value-of binding) env locals)))
        (let ((binding-var (new-var binding env)))
          (add-constraint binding-var binding-value-var env)
          (push (cons (name-of binding) binding-var) let-locals))))
    ;; The type of the let is the type of the last expression in body, so return that
    (let ((body-var nil)
          (let-var (new-var form env)))
      (dolist (body-form (body-of form))
        (setf body-var (generate-type-constraints body-form env let-locals)))
      (add-constraint let-var body-var env)
      let-var)))

(defmethod generate-type-constraints ((form lexical-variable-reference-form) env locals)
  (let ((var (new-var form env))
        (local-var (or (cdr (find (name-of form) locals :key #'car))
                       (error "Badly done"))))
    (add-constraint var local-var env)
    var))

(defparameter *type-declarations* '())
(push '(ftype* (all (a) (function (a) a)) identity) *type-declarations*)

(defun get-func-type (fname)
  ;; First try to get from the read declarations
  (dolist (decl *type-declarations*)
    (destructuring-bind (declaration-type &rest declaration-body) decl
      (when (member (symbol-name declaration-type) '("FTYPE" "FTYPE*")
                    :test #'string=)
        (when (eql (lastcar declaration-body) fname)
          (return-from get-func-type (car declaration-body))))))
  ;; If none found, use compiler information
  (compiler-info:function-type fname))

;; (get-func-type 'identity)
;; (get-func-type 'concatenate)

(defmethod generate-type-constraints ((form application-form) env locals)
  (let* ((func-type (or (compiler-info:function-type (operator-of form))
                        '(function (&rest t) t)))
         (arg-types (assign-types-from-function-type
                     func-type
                     (arguments-of form)))
         (arg-vars nil))

    ;; Constraint the types of the arguments
    (dolist (arg-type arg-types)
      (let ((var (new-var (first arg-type) env)))
        (add-constraint var (cdr arg-type) env)
        (push var arg-vars)))

    ;; Constraint the type of the application
    (let* ((return-type (last func-type))
           (app-var (new-var form env))
           (func-var (new-var (operator-of form) env)))
      (add-constraint app-var return-type env)
      (add-constraint func-var `(function ,arg-vars ,app-var) env)
      app-var)))

(defparameter *env* (make-type-env))

(generate-type-constraints
 (hu.dwim.walker:walk-form '(let ((x 22)) x))
 *env* nil)

(generate-type-constraints
 (hu.dwim.walker:walk-form '(let ((x 22)) (+ x 45)))
 *env* nil)

(type-env-constraints *env*)

(type-env-vars *env*)

(unify (type-env-constraints *env*))

(defun infer-form (form)
  (let ((type-env (make-type-env))
        (walked-form (hu.dwim.walker:walk-form form)))
    (generate-type-constraints walked-form type-env nil)
    (setf (type-env-unified type-env) (unify (type-env-constraints type-env)))
    (let ((type-assignments nil))
      (dolist (type-assignment (type-env-unified type-env))
        (trivia:match type-assignment
          ((cons x type)
           (let ((expr (cdr (assoc x (type-env-vars type-env)))))
             (push (cons expr type) type-assignments)))))
      (values
       ;; type of the walked form
       (cdr (assoc walked-form type-assignments))
       ;; type-assignments of all subexpressions
       type-assignments
       ;; type environment
       type-env))))

(multiple-value-list (infer-form 22))
(infer-form '(let ((x "lla")) (concatenate 'string x)))

(infer-form '(let ((x "lla")) x))

(infer-form '(let ((x 34)
                   (y 56))
              (+ x (- y x))))
