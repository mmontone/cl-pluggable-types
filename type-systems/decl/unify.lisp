(in-package :pluggable-types/decl)

;; https://www.cs.cornell.edu/courses/cs3110/2011sp/Lectures/lec26-type-inference/type-inference.htm

(trivia-functions:define-match-function unify-types (types))

(trivia-functions:define-match-method unify-types
    ((list (list 'values type) other-type))
  (unify-one type other-type))

(trivia-functions:define-match-method unify-types
    ((list other-type (list 'values type)))
  (unify-one type other-type))

;; Homogeneous lists
(trivia-functions:define-match-method unify-types
    ((list (list 'list-of elem-type-a) (list 'list-of elem-type-b)))
  (unify-one elem-type-a elem-type-b))

;; List fallback/coercion
(trivia-functions:define-match-method unify-types
    ((or (list (list 'list-of elem-type) (or 'cons 'list))
         (list (or 'cons 'list) (list 'list-of elem-type))))
  (unify-one (list 'list-of elem-type) '(list-of t)))

(trivia-functions:define-match-method unify-types
    ((list (list 'function args-1 return-value-1)
           (list 'function args-2 return-value-2)))
  ;; This is dependant on order:
  ;; (unify-one return-value-2 return-value-1) is different
  ;; from (unify-one return-value-1 return-value-2).
  ;; Not sure if that is correct or desired.
  (append (unify-one return-value-1 return-value-2)
          (unify-one return-value-2 return-value-1)
          (apply #'append
                 (mapcar (lambda (args)
                           (apply #'unify-one args))
                         (mapcar #'list args-2 args-1 )))
          (apply #'append
                 (mapcar (lambda (args)
                           (apply #'unify-one args))
                         (mapcar #'list args-1 args-2 )))))

(trivia-functions:define-match-method unify-types
    ((list (list 'function args-1 return-value-1)
           (list 'function (list '&rest rest-type) return-value-2)))
  (append (unify-one return-value-1 return-value-2)
          (apply #'append
                 (mapcar (curry #'unify-one rest-type) args-1))))

(trivia-functions:define-match-method unify-types
    ((list (list 'function (list '&rest rest-type) return-value-1)
           (list 'function args-2 return-value-2)))
  (append (unify-one return-value-1 return-value-2)
          (apply #'append
                 (mapcar (curry #'unify-one rest-type) args-2))))

(defun unify-one (term1 term2)
  (cond
    ((and (listp term1)
          (eql (car term1) 'values))
     (destructuring-bind (_ t1 &rest args) term1
       (declare (ignore _ args))
       (unify-one t1 term2)))
    ((and (listp term2)
          (eql (car term2) 'values))
     (destructuring-bind (_ t2 &rest args) term2
       (declare (ignore _ args))
       (unify-one term1 t2)))
    (t
     (trivia:match (list term1 term2)
       ((list (list 'var x) (list 'var y))
        (list (cons x (list 'var y))))
       ((list (list 'var x) type)
        (list (cons x type)))
       ((list type (list 'var x))
        (list (cons x type)))
       ((list type1 type2)
        (multiple-value-bind (subst unified?)
            (unify-types (list term1 term2))
          (unless unified?
            (unless (or (subtypep type1 type2)
                        (subtypep type2 type1))
              (error "Can't unify: ~s with: ~s" type1 type2)))
          subst))))))

(unify-one '(list-of integer) '(list-of string))

(unify-one '(list-of integer) '(list-of number))
(can-unify? '(list-of integer) '(list-of string))
(can-unify? '(list-of integer) 'number)

(defun subst-term (assignment term)
  (trivia:match (list assignment term)
    ((list (cons varname val) (list 'var x))
     (if (eql varname x)
         val
         term))
    (_
     (cond
       ((listp term)
        (cons (car term) (mapcar (curry 'subst-term assignment) (cdr term))))
       (t term)))))

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
      (append substitution sub2))))

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

(defmethod generate-type-constraints ((form the-form) env locals)
  (let ((var (new-var form env)))
    (add-constraint var (declared-type-of form) env)
    (add-constraint var (generate-type-constraints (value-of form) env locals) env)
    var))

(defparameter *type-declarations* '())
(push '(ftype* (all (a) (function (a) a)) identity) *type-declarations*)
(push '(ftype* (all (a b) (function ((function (a) b) (list-of a)) (list-of b)))
        mapcar)
      *type-declarations*)

(defun get-func-type (fname)
  ;; First try to get from the read declarations
  (dolist (decl *type-declarations*)
    (destructuring-bind (declaration-type &rest declaration-body) decl
      (when (member (symbol-name declaration-type) '("FTYPE" "FTYPE*")
                    :test #'string=)
        (when (eql (lastcar declaration-body) fname)
          (return-from get-func-type (car declaration-body))))))
  ;; If none found, use compiler information
  (or (compiler-info:function-type fname)
      ;; Or a generic function type
      '(function (&rest t) t)))

;; (get-func-type 'identity)
;; (get-func-type 'concatenate)

(defun instantiate-type (type env)
  (trivia:match type
    ((list 'all type-args type)
     (let ((type-instance type))
       (dolist (type-arg type-args)
         (let ((type-var (new-var type-arg env)))
           (setq type-instance (subst type-var type-arg type-instance))))
       type-instance))
    (_ type)))

;; (instantiate-type '(all (a) (list-of a)) (make-type-env))
;; (instantiate-type '(all (a) (function (a) a)) (make-type-env))
;; (instantiate-type 'integer (make-type-env))
;; (instantiate-type '(function (integer) t) (make-type-env))

(defmethod generate-type-constraints ((form application-form) env locals)
  (let* ((func-type (instantiate-type (get-func-type (operator-of form)) env))
         (arg-types (assign-types-from-function-type
                     func-type
                     (arguments-of form)))
         (arg-vars nil))

    ;; Constraint the types of the arguments
    (loop for arg in (arguments-of form)
          for arg-type in arg-types
          do
             (let ((arg-var (generate-type-constraints arg env locals)))
               (add-constraint arg-var (cdr arg-type) env)
               (push arg-var arg-vars)))

    ;; Constraint the type of the application
    (let* ((return-type (lastcar func-type))
           (app-var (new-var form env))
           ;;(func-var (new-var (operator-of form) env))
           )
      (add-constraint app-var return-type env)
      ;;(add-constraint func-var `(function ,arg-vars ,app-var) env)
      app-var)))

(defmethod generate-type-constraints ((form free-function-object-form) env locals)
  (let ((var (new-var form env)))
    (add-constraint var (instantiate-type (get-func-type (name-of form)) env) env)
    var))

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
             (push (cons expr (apply-substitution (type-env-unified type-env) type)) type-assignments)))))
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

(multiple-value-list (infer-form '(+ 22 "lala")))

;; Evaluates to integer! : uses the (all (a) (function (a) a)) type !! :-)
(multiple-value-list (infer-form '(identity 22)))

(let ((env (make-type-env)))
  (generate-type-constraints
   (hu.dwim.walker:walk-form '(+ 22 "lala"))
   env nil)
  env)

(let ((env (make-type-env)))
  (generate-type-constraints
   (hu.dwim.walker:walk-form '(identity 40))
   env nil)
  env)

(multiple-value-list (infer-form ''(1 2 3)))

(multiple-value-list (infer-form '(the (list-of integer) '(1 2 3))))

(multiple-value-list (infer-form '(the (list-of integer) 22)))

(multiple-value-list (infer-form '(the (list-of integer) '("asdf"))))

(infer-form '(mapcar #'+ (the (list-of number) '(1 2 3))))

(infer-form '(mapcar #'+ '(1 2 3)))

(infer-form '(mapcar #'+ (the (list-of string) '("lala" 2 3))))

(infer-form '(mapcar #'identity (the (list-of string) '("lala" 2 3))))

(multiple-value-list (infer-form '(mapcar #'identity (the (list-of string) '("lala" 2 3)))))

(infer-form '(mapcar #'print (the (list-of string) '("lala"))))

(infer-form '(mapcar #'identity (the list '("lala"))))

(infer-form '(mapcar #'identity '("lala")))

(infer-form '(mapcar #'identity (the (list-of t) '("lala"))))


