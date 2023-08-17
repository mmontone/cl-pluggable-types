(defpackage :pluggable-types/const
  (:use :cl :alexandria :hu.dwim.walker)
  (:import-from
   :pluggable-types/decl
   #:*funtypes*
   #:*vartypes*
   #:assign-types-from-function-type

   #:list-of
   #:cons-of
   #:all
   #:hash-table-of))

(in-package :pluggable-types/const)

(defvar *debug-solver* nil
  "When enabled, unification steps are printed to *STANDARD-OUTPUT*")

(define-condition type-inconsistency-error (simple-error)
  ())

(adt:defdata type-var
  (var t t))

(adt:defdata constraint
  (assign t t)
  (subtype t t)
  (inst t t))

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
  (or (subtypep type1 type2)
      (subtypep type2 type1)))

(trivia-functions:define-match-method types-compatible-p
    ((list type1 (cons 'values type2)))
  (types-compatible-p (list type1 (first type2))))

(trivia-functions:define-match-method types-compatible-p
    ((list (cons 'values type1) type2))
  (types-compatible-p (list (first type1) type2)))

(trivia-functions:define-match-method types-compatible-p
    ((list (list 'pluggable-types/decl:hash-table-of _ _) 'hash-table))
  t)

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
    ((list _ (inst (%0 type1) (%1 type2)))
     (inst (subst-term assignment type1)
           (subst-term assignment type2)))
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

(defun unify-types (type1 type2 solution)
  (cond
    ((and (typep type1 'var)
          (fully-solved-p type2))
     (values (cons (cons type1 type2) solution) t))
    ((and (typep type2 'var)
          (fully-solved-p type1))
     (values (cons (cons type2 type1) solution) t))
    ((and (fully-solved-p type1)
          (fully-solved-p type2))
     (unless (types-compatible-p (list type1 type2))
       (error 'type-inconsistency-error
              :format-control "~a is not compatible with ~a"
              :format-arguments (list type1 type2)))
     (values solution t))
    ((and (listp type1) (listp type2)
          (eql (car type1) (car type2)))
     (let ((sol solution)
           (solved t))
       (loop for t1 in (cdr type1)
             for t2 in (cdr type2)
             do
                (multiple-value-bind (unif-sol unif-solved)
                    (unify-types t1 t2 sol)
                  (setq solved (and solved unif-solved))
                  (setq sol unif-sol)))
       (values sol solved)))
    ;; not solved
    (t
     (trivia:match (list type1 type2)
       ((list (list 'cons-of a b) (list 'list-of c))
        (unify-types (list 'cons-of a b)
                     (list 'cons-of c (list 'list-of c))
                     solution))
       (_
        (values solution nil))))))

(declaim (ftype (function (constraint list) (values list boolean))
                solve-constraint))
(defun solve-constraint (constraint solution)
  (format t "Solve constraint: ~a~%" constraint)
  ;;(break)
  (adt:match constraint constraint
    ((assign var thing)
     (let ((current-var (assoc var solution)))
       (cond
         ;; Not already assigned, assign.
         ((and (not current-var) (fully-solved-p thing))
          ;;(break "assign ~a to ~a" var thing)
          (values (cons (cons var thing) solution) t))
         ;; If var already assigned, check type compatibility
         ((and current-var (fully-solved-p thing))
          (let ((assigned (cdr current-var)))
            (unless (types-compatible-p (list assigned thing))
              (error 'type-inconsistency-error
                     :format-control "~a is not compatible with ~a"
                     :format-arguments (list assigned thing)))
            ;;(break "already assigned")
            #+todo(if (subtypep thing assigned)
                (progn
                  (setf (cdr (assoc var solution)) thing)
                  (values solution t))
                (values solution t))
            (values solution t)))
         ;; Already assigned and right side is a variable.
         ;; Add as solution for variable at right hand side.
         ((and current-var (typep thing 'var))
          (values (cons (cons thing (cdr current-var))
                        solution)
                  t))
         ;; Cannot solve
         (t
          (values solution nil)))))
    ((subtype type1 type2)
     ;;(break)
     (when (or (not (fully-solved-p type1))
               (not (fully-solved-p type2)))
       (return-from solve-constraint (values solution nil)))
     (unless (types-compatible-p (list type1 type2))
       (cerror "Continue" 'type-inconsistency-error
              :format-control "~a is not subtype of ~a"
              :format-arguments (list type1 type2)))
     (values solution t))
    ((inst type1 type2)
     (cond
       ((equalp type1 type2)
        (values solution t))
       ((and (fully-solved-p type1)
             (fully-solved-p type2))
        (unless (types-compatible-p (list type2 type1))
          (cerror "Continue" 'type-inconsistency-error
              :format-control "~a is not subtype of ~a"
              :format-arguments (list type1 type2)))
        (values solution t))
       (t (unify-types type1 type2 solution))))
    ))     

(defun solve (constraints &optional solution)
  "Solve CONSTRAINTS under current SOLUTIONitution."
  (format t "~%Solving ... ~%")
  (when (null constraints)
    (return-from solve solution))
  (let ((unsolved nil)
        (current-solution solution))
    (dolist (constraint constraints)
      (format t "Constraint: ~a ~%" constraint)
      (multiple-value-bind (new-solution solved?)
          (solve-constraint (apply-solution current-solution constraint) current-solution)
        (assert (or (not solution) new-solution))
        (setq current-solution new-solution)
        (when (not solved?)
          (push constraint unsolved))
        (format t "Solved: ~a~%" solved?)))
    (if unsolved
        (if (= (length constraints) (length unsolved))
            current-solution
            (solve unsolved current-solution))
        current-solution)))

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

(defmethod generate-type-constraints ((form let*-form) env locals)
  (let ((let-locals locals))
    (dolist (binding (bindings-of form))
      (let ((binding-value-var
              (generate-type-constraints (initial-value-of binding) env let-locals)))
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
  (let ((var (new-var form env))
        (value-type (generate-type-constraints (value-of form) env locals)))
    (add-constraint (assign var (declared-type-of form)) env)
    (add-constraint (subtype var value-type) env)
    (add-constraint (inst value-type (declared-type-of form)) env)
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

;; (instantiate-type '(pluggable-types/decl::all (a) (list-of a)) (make-type-env))
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
               (add-constraint (inst (cdr arg-type) actual-arg) env)
               (push arg-var arg-vars)))

    ;; Constraint the type of the application
    (let* ((return-type (lastcar func-type))
           (app-var (new-var form env)))
      (cond
        ;; Treat MAKE-INSTANCE specially
        ((and (eql (operator-of form) 'make-instance)
              (typep (first (arguments-of form)) 'constant-form))
         (add-constraint (assign app-var (value-of (first (arguments-of form)))) env))
        ;; Otherwise, the type of the application is the function return type
        (t
         (add-constraint (assign app-var return-type) env)
         (add-constraint (inst return-type app-var) env)         
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
    (add-constraint (assign var `(function ,arg-types ,body-type)) env)
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

(defun infer-form* (&rest args)
  (first (multiple-value-list (apply #'infer-form args))))