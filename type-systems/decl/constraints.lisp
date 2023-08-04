(defpackage :pluggable-types/const
  (:use :cl :alexandria :hu.dwim.walker)
  (:import-from
   :pluggable-types/decl
   #:*funtypes*
   #:*vartypes*
   #:assign-types-from-function-type))

(in-package :pluggable-types/const)

(defvar *debug-solver* nil
  "When enabled, unification steps are printed to *STANDARD-OUTPUT*")

(define-condition type-inconsistency-error (simple-error)
  ())

(adt:defdata type-var
  (var t t))

(adt:defdata constraint
  (assign t t)
  (subtype t t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivia-functions:define-match-function types-compatible-p (types)))

;; Homogeneous lists
(trivia-functions:define-match-method types-compatible-p
    ((list (list 'list-of elem-type-a) (list 'list-of elem-type-b)))
  (types-compatible-p (list elem-type-a elem-type-b)))

(trivia-functions:define-match-method types-compatible-p
    ((list (list 'list-of list-type)
           (list 'cons-of cons-type-a cons-type-b)))
  (and (types-compatible-p (list list-type cons-type-a))
       (types-compatible-p (list cons-type-b `(list-of ,list-type)))))

(trivia-functions:define-match-method types-compatible-p
    ((list (list 'cons-of cons-type-a cons-type-b)
           (list 'list-of list-type)))
  (and (types-compatible-p (list list-type cons-type-a))
       (types-compatible-p (list cons-type-b `(list-of ,list-type)))))

;; Homogeneous cons
(trivia-functions:define-match-method types-compatible-p
    ((list (list 'cons-of a b) (list 'cons-of c d)))
  (and (types-compatible-p (list a c))
       (types-compatible-p (list b d))))

;; Cons fallback/coercion
(trivia-functions:define-match-method types-compatible-p
    ((or (list (list 'cons-of a b) (or 'cons 'list))
         (list (or 'cons 'list) (list 'cons-of a b))))
  (types-compatible-p (list (list 'cons-of a b) '(cons-of t t))))

(trivia-functions:define-match-method types-compatible-p
    ((list (list 'hash-table-of a b)
           (list 'hash-table-of c d)))
  (and (types-compatible-p (list a c))
       (types-compatible-p (list b d))))

(trivia-functions:define-match-method types-compatible-p
    ((list type1 type2))
  (subtypep type1 type2))

(declaim (ftype (function (cons t) t) subst-term))
(defun subst-term (assignment term)
  "Substitute ASSIGNMENT in TERM.
ASSIGNMENT is CONS of VAR to a TERM."
  (when (null term)
    (return-from subst-term term))
  (trivia:match (list assignment term)
    ((list (cons (var (%0 varname) (%1 _)) val)
           (var (%0 x) (%1 _)))
     (if (eql varname x)
         val
         term))
    ((list _ (subtype (%0 type1) (%1 type2)))
     (subtype (subst-term assignment type1)
              (subst-term assignment type2)))
    ((list _ (assign (%0 var) (%1 what)))
     (assign var (subst-term assignment what)))
    (_
     (cond
       ((listp term)
        (cons (subst-term assignment (car term))
              (mapcar (curry 'subst-term assignment) (cdr term))))
       (t term)))))

;; (subst-term (cons (var 'x nil) 'y) (list (var 'x nil)))

;; A substituion is a list of assignments
(defun apply-solution (assignments term)
  (let ((new-term term))
    (dolist (assignment assignments)
      (setq new-term (subst-term assignment new-term)))
    new-term))

(defun tree-find (what tree)
  (cond
    ((eql what tree)
     t)
    ((atom tree)
     nil)
    ((consp tree)
     (or (tree-find what (car tree))
         (tree-find what (cdr tree))))
    (t nil)))

(tree-find 'a '(b (a)))

(defun tree-find-if (predicate tree)
  (cond
    ((atom tree)
     (funcall predicate tree))
    ((consp tree)
     (or (tree-find-if predicate (car tree))
         (tree-find-if predicate (cdr tree))))))

(defun fully-solved-p (thing)
  (not (tree-find-if (rcurry #'typep 'type-var) thing)))

(fully-solved-p '(lala))
(fully-solved-p (list (var 'a nil)))

(declaim (ftype (function (constraint list) (values list boolean))
                solve-constraint))
(defun solve-constraint (constraint solution)
  (break)
  (adt:match constraint constraint
    ((assign var thing)
     (when (not (fully-solved-p thing))
       (return-from solve-constraint (values solution nil)))
     (let ((current-var (assoc var solution)))
       (cond
         ;; If var already assigned, check type compatibility
         (current-var
          (let ((assigned (cdr current-var)))
            (unless (or (subtypep assigned thing)
                        (subtypep thing assigned))
              (error 'type-inconsistency-error))
            ;;(break "already assigned")
            (if (subtypep thing assigned)
                (progn
                  (setf (cdr (assoc var solution)) thing)
                  (values solution t))
                (values solution t))))
         ;; Not already assigned, assign.
         (t
          ;;(break "assign ~a to ~a" var thing)
          (values (cons (cons var thing) solution) t)))))
    ((subtype type1 type2)
     ;;(break)
     (when (or (not (fully-solved-p type1))
               (not (fully-solved-p type2)))
       (return-from solve-constraint (values solution nil)))
     (unless (types-compatible-p (list type1 type2))
       (error 'type-inconsistency-error
              :format-control "~a is not subtype of ~a"
              :format-arguments (list type1 type2)))
     (values solution t))))     

(defun solve (constraints &optional solution)
  "Solve CONSTRAINTS under current SOLUTIONitution."
  (when (null constraints)
    (return-from solve solution))
  (let ((unsolved nil)
        (current-solution solution))
    (dolist (constraint constraints)
      (format t "Constraint: ~a " constraint)
      (multiple-value-bind (new-solution solved?)
          (solve-constraint (apply-solution current-solution constraint) current-solution)
        (setq current-solution new-solution)
        (when (not solved?)
          (push constraint unsolved))
        (format t " solved: ~a~%" solved?)))
    (if unsolved
        (solve unsolved current-solution)
        current-solution)))
#|
(unify `((,(var 'x 'x) . ,(var 'y 'y)))) => '((x . (var y)))
(unify `((,(var 'x 'x) . integer))) => '((x . integer))
(unify '((integer . (var x)))) => '((x . integer))
(unify '(((var x) . (list-of integer)))) => '((x . (list-of integer)))
(unify '((all (a) (list-of a)) . integer)) => '((a . integer))
(unify '(integer . (all (a) (list-of a)))) => '((a . integer))

(unify '(((var x) . integer) ((var x) . boolean))) => error
(unify '(((var x) . integer) ((var y) . boolean) ((var x) . (var y))))

(unify `((integer . t))) ;; success
(unify `((integer . nil))) ;; success
(unify `((integer . fixnum))) ;; success
(unify `((fixnum . integer))) ;; success
(unify `((integer . string))) ;; fail

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
  (solution nil :type list)
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
    (push (cons var form) (type-env-vars env))
    var))

(declaim (ftype (function (constraint type-env) t) add-constraint))
(defun add-constraint (constraint env)
  "Add constraint from type term X to type term Y, in ENV"
  (push constraint (type-env-constraints env)))

(declaim (ftype (function (walked-form type-env list) t) generate-type-constraints))
(defgeneric generate-type-constraints (form env locals)
  (:documentation "Generate type constraints for FORM in ENV under LOCALS."))

(defmethod generate-type-constraints ((form constant-form) env locals)
  (let ((var (new-var form env))
        (type (if (functionp (value-of form))
                  (get-func-type (value-of form) env)
                  (type-of (value-of form)))))
    (add-constraint (assign var type) env)
    var))

(defmethod generate-type-constraints ((form let-form) env locals)
  (let ((let-locals locals))
    (dolist (binding (bindings-of form))
      (let ((binding-value-var
              (generate-type-constraints (initial-value-of binding) env locals)))
        (let ((binding-var (new-var binding env)))
          (add-constraint (assign binding-var binding-value-var) env)
          (push (cons (name-of binding) binding-var) let-locals))))
    ;; The type of the let is the type of the last expression in body, so return that
    (let ((body-var nil)
          (let-var (new-var form env)))
      (dolist (body-form (body-of form))
        (setf body-var (generate-type-constraints body-form env let-locals)))
      (add-constraint (assign let-var body-var) env)
      let-var)))

(defmethod generate-type-constraints ((form lexical-variable-reference-form) env locals)
  (let ((var (new-var form env))
        (local-var (or (cdr (find (name-of form) locals :key #'car))
                       (error "Badly done"))))
    (add-constraint (assign var local-var) env)
    var))

(defmethod generate-type-constraints ((form the-form) env locals)
  (let ((var (new-var form env)))
    (add-constraint (assign var (declared-type-of form)) env)
    (add-constraint (subtype var (generate-type-constraints (value-of form) env locals)) env)
    var))

(defmethod generate-type-constraints ((form setq-form) env locals)
  ;; Should constrain only if variable has a DECLARED type, not inferred..
  (let ((local-var (or (cdr (find (name-of (variable-of form)) locals :key #'car))
                       (error "Badly done")))
        (value-var (generate-type-constraints (value-of form) env locals)))
    (add-constraint (subtype value-var local-var) env)
    local-var))

(defmethod generate-type-constraints ((form implicit-progn-mixin) env locals)
  (let ((var (new-var form env)))
    (dolist (expr (body-of form))
      (setq var (generate-type-constraints expr env locals)))
    var))

(defmethod generate-type-constraints ((form go-tag-form) env locals)
  ;; what to do?
  (let ((var (new-var form env)))
    (add-constraint var 't env)
    var))

(defmethod generate-type-constraints ((form go-form) env locals)
  ;; what to do?
  (let ((var (new-var form env)))
    (add-constraint var 't env)
    var))

(defparameter *type-declarations* '())

(declaim (ftype (function ((or symbol function) type-env) t) get-func-type))
(defun get-func-type (func env)
  "Get the type of function with FNAME in ENV."
  (let ((fname (typecase func
                 (symbol func)
                 (function (compiler-info:function-name func)))))
    ;; First try to get from the read declarations
    (dolist (funtype *funtypes*)
      (when (eql fname (car funtype))
        (return-from get-func-type (cdr funtype))))
    (dolist (decl *type-declarations*)
      (destructuring-bind (declaration-type &rest declaration-body) decl
        (when (member (symbol-name declaration-type) '("FTYPE" "FTYPE*")
                      :test #'string=)
          (when (eql (lastcar declaration-body) fname)
            (return-from get-func-type (car declaration-body))))))
    ;; If none found, use compiler information
    (or (compiler-info:function-type fname)
        ;; Or a generic function type
        '(function (&rest t) t))))

;; (get-func-type 'identity (make-type-env))
;; (get-func-type 'concatenate (make-type-env))
;; (get-func-type #'identity (make-type-env))
;; (get-func-type #'+ (make-type-env))

(declaim (ftype (function (t type-env) t) instantiate-type))
(defun instantiate-type (type env)
  "Create an instance of TYPE in ENV.
Type parameters are substituted by type variables."
  (trivia:match type
    ((list 'pluggable-types/decl:all type-args type)
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

(declaim (ftype (function (t) t) generalize-type))
(defun generalize-type (type)
  "Generalize TYPE."
  (let ((var-counter 64)
        (vars (list)))
    (labels ((var-for (varname)
               (or (cdr (assoc varname vars))
                   (let ((var (intern (princ-to-string (code-char (incf var-counter))))))
                     (push (cons varname var) vars)
                     var)))
             (generalize-term (term)
               (trivia:match term
                 ((var (%0 varname) (%1 varinfo))
                  (var-for varname))
                 ((cons x xs)
                  (cons (generalize-term x) (mapcar #'generalize-term xs)))
                 (_ term))))
      (let ((term-body (generalize-term type)))
        (if vars
            `(all ,(nreverse (mapcar #'cdr vars)) ,term-body)
            type)))))

;; (generalize-type `(list-of ,(var 'a nil)))
;; (generalize-type `(function (,(var 'a nil) ,(var 'b nil)) ,(var 'b nil)))
;; (generalize-type `(function (,(var 'a nil) ,(var 'b nil)) ,(var 'a nil)))
;; (generalize-type `(function (,(var 'a nil) ,(var 'b nil)) ,(var 'z nil)))

(declaim (ftype (function (t walked-form type-env list) t)
                generate-function-application-constraints))
(defun generate-function-application-constraints (func-type form env locals)
  "Generate type constraints for function application."
  (let ((arg-types (assign-types-from-function-type
                    func-type (arguments-of form)))
        (arg-vars nil))

    ;; Constraint the types of the arguments
    (loop for arg in (arguments-of form)
          for arg-type in arg-types
          do
             (let ((arg-var (new-var arg env))
                   (actual-arg (generate-type-constraints arg env locals)))
               (add-constraint (assign arg-var (cdr arg-type)) env)
               (add-constraint (subtype actual-arg arg-var) env)
               (push arg-var arg-vars)))

    ;; Constraint the type of the application
    (let* ((return-type (lastcar func-type))
           (app-var (new-var form env))
           ;;(func-var (new-var (operator-of form) env))
           )
      (cond
        ;; Treat MAKE-INSTANCE specially
        ((and (eql (operator-of form) 'make-instance)
              (typep (first (arguments-of form)) 'constant-form))
         (add-constraint (assign app-var (value-of (first (arguments-of form)))) env))
        ;; Otherwise, the type of the application is the function return type
        (t
         (add-constraint (assign app-var return-type) env)
         ;;(add-constraint func-var `(function ,arg-vars ,app-var) env)
         ))
      app-var)))

(defmethod generate-type-constraints ((form application-form) env locals)
  (let ((func-type (instantiate-type (get-func-type (operator-of form) env) env)))
    (generate-function-application-constraints
     func-type form env locals)))

(defmethod generate-type-constraints ((form free-function-object-form) env locals)
  (let ((var (new-var form env)))
    (add-constraint (assign var (instantiate-type (get-func-type (name-of form) env) env)) env)
    var))

(defmethod generate-type-constraints ((form walked-lexical-variable-reference-form) env locals)
  (alexandria:if-let (local (assoc (name-of form) locals))
    (let ((var (new-var form env)))
      (add-constraint (assign var (cdr local)) env)
      var)
    (error "Shouldn't happen")))

(defmethod generate-type-constraints-for-declarations (declarations bindings-vars form env locals)
  (dolist (declaration declarations)
    (typecase declaration
      (type-declaration-form
       (let ((binding-var (or (cdr (assoc (name-of declaration)
                                          bindings-vars :key #'name-of))
                              (error "Binding not found: ~s" (name-of declaration)))))
         (add-constraint (subtype binding-var (declared-type-of declaration)) env))))))

(defmethod generate-type-constraints ((form lambda-function-form) env locals)
  (let* ((var (new-var form env))
         (arg-types (mapcar (rcurry #'new-var env) (bindings-of form)))
         (lambda-locals (append (mapcar (lambda (arg arg-type)
                                          (cons (name-of arg)
                                                arg-type))
                                        (bindings-of form)
                                        arg-types)))
         (body-type nil))
    (generate-type-constraints-for-declarations
     (declarations-of form) (mapcar #'cons (bindings-of form) arg-types)
     form env (append locals lambda-locals))
    (dolist (body-form (body-of form))
      (setq body-type (generate-type-constraints body-form env (append locals lambda-locals))))
    (add-constraint var `(function ,arg-types ,body-type) env)
    var))

(defmethod generate-type-constraints ((form if-form) env locals)
  (let ((then-type (generate-type-constraints (then-of form) env locals))
        (else-type (generate-type-constraints (else-of form) env locals))
        (var (new-var form env)))
    (add-constraint var `(or ,then-type ,else-type) env)
    var))

(defun canonize-type (type)
  (trivia:match type
    ;; Clean the (values <type> &optional) types
    ((list 'values type '&optional)
     type)
    ((cons type-name args)
     (cons type-name (mapcar #'canonize-type args)))
    (_ type)))

(defun infer-form (form &optional env)
  "Infer the type of FORM."
  (let ((type-env (or env (make-type-env)))
        (walked-form (hu.dwim.walker:walk-form form)))
    (generate-type-constraints walked-form type-env nil)
    (setf (type-env-solution type-env) (solve (type-env-constraints type-env)))
    (let ((type-assignments nil))
      (dolist (type-assignment (type-env-solution type-env))
        (trivia:match type-assignment
          ((cons x type)
           (let ((expr (cdr (assoc x (type-env-vars type-env)))))
             (assert expr)
             (push (cons expr
                         (arrows:->> type
                                     (apply-substitution (type-env-solution type-env))
                                     (canonize-type)
                                     (generalize-type)))
                   type-assignments)))))
      (values
       ;; type of the walked form
       (cdr (assoc walked-form type-assignments))
       ;; type-assignments of all subexpressions
       type-assignments
       ;; type environment
       type-env))))
