(in-package :pluggable-types/decl)

;; https://www.cs.cornell.edu/courses/cs3110/2011sp/Lectures/lec26-type-inference/type-inference.htm

(define-condition type-unification-error (simple-error)
  ())

(adt:defdata type-term
  (var symbol t) ;; var-name, info
;;  (literal-type t)
;;  (function-type list t t) ;; arg-types, return-type, info
  (or-subst list))

(defmethod print-object ((var var) stream)
  (adt:match var var
    ((var varname info)
     (write-string "(var " stream)
     (princ varname stream)
     (when info
       (write-string " " stream)
       (prin1 info stream))
     (write-string ")" stream))))

(trivia-functions:define-match-function unify-types (types))

;; Homogeneous lists
(trivia-functions:define-match-method unify-types
    ((list (list 'list-of elem-type-a) (list 'list-of elem-type-b)))
  (unify-one elem-type-a elem-type-b))

;; List fallback/coercion
(trivia-functions:define-match-method unify-types
    ((or (list (list 'list-of elem-type) (or 'cons 'list))
         (list (or 'cons 'list) (list 'list-of elem-type))))
  (unify-one (list 'list-of elem-type) '(list-of t)))

;; Homogeneous cons
(trivia-functions:define-match-method unify-types
    ((list (list 'cons-of a b) (list 'cons-of c d)))
  (append (unify-one a c)
          (unify-one b d)))

;; Cons fallback/coercion
(trivia-functions:define-match-method unify-types
    ((or (list (list 'cons-of a b) (or 'cons 'list))
         (list (or 'cons 'list) (list 'cons-of a b))))
  (unify-one (list 'cons-of a b) '(cons-of t t)))

(defun unify-one (term1 term2)
  (format t "Unify: ~a ~a " term1 term2)
  (let ((unification
          (trivia:match (list term1 term2)
            ;; multiple values unification
            ((list (cons 'values values-types-1)
                   (cons 'values values-types-2))
             (loop for val-type-1 in values-types-1
                   for val-type-2 in values-types-2
                   appending (unify-one val-type-1 val-type-2)))
            ((list (cons 'values values-types)
                   type)
             (unify-one (first values-types) type))
            ((list type (cons 'values values-types))
             (unify-one type (first values-types)))
            ;; functions with &rest in lambda-list
            ((list (list 'function args-1 return-value-1)
                   (list 'function (list '&rest rest-type) return-value-2))
             (apply #'append
                    (mapcar (curry #'unify-one rest-type) args-1)))
            ((list (list 'function (list '&rest rest-type) return-value-1)
                   (list 'function args-2 return-value-2))
             (append (unify-one return-value-1 return-value-2)
                     (apply #'append
                            (mapcar (curry #'unify-one rest-type) args-2))))
            ;; functions
            ((list (list 'function args-1 return-value-1)
                   (list 'function args-2 return-value-2))
             ;; This is dependant on order:
             ;; (unify-one return-value-2 return-value-1) is different
             ;; from (unify-one return-value-1 return-value-2).
             ;; Not sure if that is correct or desired.
             (append
              ;;(unify-one return-value-1 return-value-2)
              (unify-one return-value-2 return-value-1)
              #+nil(apply #'append
                     (mapcar (lambda (args)
                               (apply #'unify-one args))
                             (mapcar #'list args-2 args-1 )))
              (apply #'append
                     (mapcar (lambda (args)
                               (apply #'unify-one args))
                             (mapcar #'list args-1 args-2 )))
              ))
            ;; type variables
            ((list (var (%0 x) (%1 x-info)) (var (%0 y) (%1 y-info)))
             (list (cons x (var y y-info))))
            ((list (var (%0 x) (%1 info)) type)
             (list (cons x type)))
            ((list type (var (%0 x) (%1 info)))
             (list (cons x type)))
            ;; rest of types
            ((list type1 type2)
             (multiple-value-bind (subst unified?)
                 (unify-types (list term1 term2))
               (unless unified?
                 ;; unify iff type1 and type2 can be coerced
                 (unless (or (subtypep type1 type2)
                             (subtypep type2 type1))
                   (error 'type-unification-error
                          :format-control "Can't unify: ~s with: ~s"
                          :format-arguments (list type1 type2))))
               subst)))))
    (format t " => ~a ~%" unification)
    unification))

;;(unify-one '(list-of integer) '(list-of string))
;;(unify-one '(list-of integer) '(list-of number))

(declaim (ftype (function (cons t) t) subst-term))
(defun subst-term (assignment term)
  (trivia:match (list assignment term)
    ((list (cons varname val) (var (%0 x)))
     (if (eql varname x)
         val
         term))
    ((list assignment (or-subst (%0 or-cases)))
     (or-subst (mapcar (curry 'subst-term assignment) or-cases)))
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

#|
(unify `((,(var 'x 'x) . ,(var 'y 'y)))) => '((x . (var y)))
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
  (symbol-nr 0 :type integer)
  (vars nil :type list)
  (constraints nil :type list)
  (unified nil :type list)
  (declared-ftypes nil :type list)
  (declared-vartypes nil :type list)
  (debugp nil :type boolean))

(declaim (ftype (function (walked-form type-env) var)))
(defun new-var (form env)
  "Create a new type variable for FORM in ENV."
  (let* ((varname (intern (format nil "VAR~a" (incf (type-env-symbol-nr env)))))
         (var (var varname
                   (when (type-env-debugp env)
                     form))))
    (push (cons varname form) (type-env-vars env))
    var))

(declaim (ftype (function (t t type-env) t) add-constraint))
(defun add-constraint (x y env)
  "Add constraint from type term X to type term Y, in ENV"
  (push (cons x y) (type-env-constraints env)))

(declaim (ftype (function (walked-form type-env list) list) generate-type-constraints))
(defgeneric generate-type-constraints (form env locals)
  (:documentation "Generate type constraints for FORM in ENV under LOCALS."))

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
(push '(ftype* (all (a) (function (unsigned-byte (list-of a)) a))
        nth)
      *type-declarations*)
(push '(ftype* (all (a) (function ((list-of a)) a)) first) *type-declarations*)
(push '(ftype* (all (a) (function ((list-of a)) (list-of a))) rest) *type-declarations*)

(push '(ftype*
        (or (all (a b) (function ((cons-of a b)) a))
         (all (a) (function ((list-of a)) a)))
        car)
      *type-declarations*)

(push '(ftype* (or
                (all (a b) (function ((cons-of a b)) b))
                (all (a) (function ((list-of a)) (list-of a))))
        cdr)
      *type-declarations*)

(declaim (ftype (function (symbol type-env) t) get-func-type))
(defun get-func-type (fname env)
  "Get the type of function with FNAME in ENV."
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

;; (get-func-type 'identity (make-type-env))
;; (get-func-type 'concatenate (make-type-env))

(declaim (ftype (function (t type-env) t) instantiate-type))
(defun instantiate-type (type env)
  "Create an instance of TYPE in ENV.
Type parameters are substituted by type variables."
  (trivia:match type
    ((list 'all type-args type)
     (let ((type-instance type))
       (dolist (type-arg type-args)
         (let ((type-var (new-var type-arg env)))
           (setq type-instance (subst type-var type-arg type-instance))))
       type-instance))
    ((cons type-name args)
     (cons type-name (mapcar (rcurry #'instantiate-type env) args)))
    (_ type)))

;; (instantiate-type '(all (a) (list-of a)) (make-type-env))
;; (instantiate-type '(all (a) (function (a) a)) (make-type-env))
;; (instantiate-type 'integer (make-type-env))
;; (instantiate-type '(function (integer) t) (make-type-env))
;; (instantiate-type '(or (all (a) (function (a) boolean))
;;                     (all (a b) (function (a b) b)))
;;                   (make-type-env))

(declaim (ftype (function (t walked-form type-env list) t)
                generate-function-application-constraints))
(defun generate-function-application-constraints (func-type form env locals)
  "Generate type constraints for function application."
  (let ((arg-types (assign-types-from-function-type
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

(defmethod generate-type-constraints ((form application-form) env locals)
  (let ((func-type (instantiate-type (get-func-type (operator-of form) env) env)))
    (ecase (car func-type)
      ;; OR type. Several function cases.
      (or (let ((func-vars (mapcar (rcurry #'generate-function-application-constraints form env locals)
                                   (cdr func-type)))
                (or-var (new-var form env)))
            (add-constraint or-var (or-subst func-vars) env)))
      ;; A single function type
      (function (generate-function-application-constraints
                 func-type form env locals)))))

(defmethod generate-type-constraints ((form free-function-object-form) env locals)
  (let ((var (new-var form env)))
    (add-constraint var (instantiate-type (get-func-type (name-of form) env) env) env)
    var))

(defun canonize-type (type)
  (trivia:match type
    ((or-subst (%0 subtypes))
     (dolist (subtype subtypes)
       ;; Take the subtype that is fully unified (doesn't have variables).
       (when (not (some-tree (rcurry #'typep 'var)
                             subtype))
         (return-from canonize-type subtype))))
    (_ type)))

(defun infer-form (form &optional env)
  "Infer the type of FORM."
  (let ((type-env (make-type-env))
        (walked-form (hu.dwim.walker:walk-form form)))
    (generate-type-constraints walked-form type-env nil)
    (setf (type-env-unified type-env) (unify (type-env-constraints type-env)))
    (let ((type-assignments nil))
      (dolist (type-assignment (type-env-unified type-env))
        (trivia:match type-assignment
          ((cons x type)
           (let ((expr (cdr (assoc x (type-env-vars type-env)))))
             (push (cons expr
                         (arrows:->> type
                                     (apply-substitution (type-env-unified type-env))
                                     (canonize-type)))
                   type-assignments)))))
      (values
       ;; type of the walked form
       (cdr (assoc walked-form type-assignments))
       ;; type-assignments of all subexpressions
       type-assignments
       ;; type environment
       type-env))))

(defun some-tree (predicate tree)
  (cond
    ((atom tree) (funcall predicate tree))
    ((listp tree)
     (or (funcall predicate tree)
         (some (curry #'some-tree predicate) tree)))))

;; (some-tree (lambda (x)
;;                 (and (listp x)
;;                      (eql (car x) 'var)))
;;               '(or (var x)))

;; (some-tree (lambda (x)
;;                 (and (listp x)
;;                      (eql (car x) 'var)))
;;            '(or x z))

(defun tree-find-if (predicate tree)
  (cond
    ((atom tree)
     (when (funcall predicate tree)
       tree))
    ((listp tree)
     (when (funcall predicate tree)
       (return-from tree-find-if tree))
     (dolist (x tree)
       (when (tree-find-if predicate x)
         (return-from tree-find-if x))))
    (t nil)))

;; (tree-find-if (lambda (x)
;;                 (and (listp x)
;;                      (eql (car x) 'var)))
;;               '(or (var x)))

;; (tree-find-if (lambda (x)
;;                 (and (listp x)
;;                      (eql (car x) 'var)))
;;            '(or x z))

(defun type-equalp (t1 t2)
  (and (subtypep t1 t2)
       (subtypep t2 t1)))

(defun type-coerceablep (t1 t2)
  (or (subtypep t1 t2)
      (subtypep t2 t1)))
