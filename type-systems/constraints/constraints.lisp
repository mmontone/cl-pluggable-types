;; https://dada.cs.washington.edu/research/tr/1998/01/UW-CSE-98-01-01.pdf

(defpackage :pluggable-types/const
  (:use :cl :alexandria :hu.dwim.walker
   :polymorphic-types
        :polymorphic-cl-types)
  (:export #:check-form
           #:type-checking-error
           #:types-compatible-p))

(in-package :pluggable-types/const)

(defvar *debug-solver* nil
  "When enabled, unification steps are printed to *STANDARD-OUTPUT*")

(define-condition type-checking-error (simple-error)
  ((form :initarg :form
         :accessor error-form)
   (source :initarg :source
           :accessor error-source)
   (position :initarg :position
             :accessor error-position)))

(define-condition type-inconsistency-error (type-checking-error)
  ())

(adt:defdata type-var
  (var t t)
  (unknown t))

(adt:defdata constraint
  (unify t t)
  (subtype t t))

(defmethod print-object ((var var) stream)
  (adt:match type-var var
    ((var name form)
     (format stream "~a" name)
     (when (and *debug-solver* form
                (typep form 'walked-form))
       (format stream "[~a]" (source-of form))))
    ((unknown form)
     (call-next-method))))

(defun unknown-p (x)
  (typep x 'unknown))

(defun types-compatible-p (type1 type2)
  "Return T when TYPE1 can be used in a place that expects a TYPE2."
  (trivia:match (list type1 type2)
    ((list 't _)
     t)
    ((list _ 't)
     t)
    ((list _ (list 'cons-of a (list 'list-of b)))
     (types-compatible-p type1 `(list-of (or ,a ,b))))
    ((list (cons 'values vt1) (cons 'values vt2))
     (every (curry #'apply #'types-compatible-p)
            (mapcar #'list vt1 vt2)))
    ((list (cons 'values vt1) type)
     (types-compatible-p (car vt1) type))
    ((list type (cons 'values vt2))
     (types-compatible-p type (car vt2)))
    ((list (list 'list-of type1)
           (list 'list-of type2))
     (types-compatible-p type1 type2))
    ((list (list 'list-of type)
           (list 'cons-of a b))
     (types-compatible-p `(cons-of ,type (list-of ,type))
                         type2))
    ((list (list 'cons-of a b)
           (list 'list-of type))
     (types-compatible-p type1
                         `(cons-of ,type (list-of ,type))))
    ((list (cons 'function _)
           'function)
     t)
    ((list (list 'function f1args f1ret)
           (list 'function f2args f2ret))
     ;; TODO: parse lambda lists and check args
     (types-compatible-p f2ret f1ret))
    ;; Type union
    ((list _ (cons 'or types))
     (some (curry #'types-compatible-p type1) types))
    ((list (cons 'or types) _)
     (some (rcurry #'types-compatible-p type2) types))
    ;; Compatibility of composed types
    ((and (list (cons tname1 args1)
                (cons tname2 args2))
          (satisfies (lambda (_)
                       (declare (ignore _))
                       (and (symbolp tname1)
                            (symbolp tname2)
                            (eql tname1 tname2)))))
     (every #'types-compatible-p args1 args2))
    #+nil((list _ (satisfies unknown-p))
     ;;(adt:set-data type2 (unknown type1))
     
     t)
    #+nil((list (satisfies unknown-p) _)
     ;;(adt:set-data type1 (unknown type2))
     t)
    (_
     (or
      (some (rcurry #'typep 'var) (list type1 type2))
      ;;(some (rcurry #'typep 'unknown) (list type1 type2))
      (subtypep type1 type2)))))

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
     (unless (types-compatible-p type1 type2)
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

(declaim (ftype (function ((list-of cons)) list) resolve-type-vars))

(defun resolve-type-vars-one (term1 term2)
  (when (eql term1 term2)
    (return-from resolve-type-vars-one nil))
  (trivia:match (mapcar #'expanded-type (list term1 term2))
    ((list (cons 'values ts1) (cons 'values ts2))
     (resolve-type-vars (mapcar #'cons ts1 ts2)))
    ((list (cons 'values values) type)
     (resolve-type-vars-one (car values) type))
    ((list type (cons 'values values))
     (resolve-type-vars-one type (car values)))
    ((list (list 'list-of a) (list 'list-of b))
     (resolve-type-vars-one a b))
    ((list (list 'cons-of a (list 'list-of c))
           (list 'cons-of b (list 'list-of d)))
     (resolve-type-vars-one a b)
     (resolve-type-vars-one c d))
    ((list (list 'cons-of a b)
           (list 'list-of type))
     (resolve-type-vars-one term1 `(cons-of ,type (list-of ,type))))
    ((list (list 'list-of type) (list 'cons-of a b))
     (resolve-type-vars-one `(cons-of ,type (list-of ,type)) term2))
    ((list (var (%0 vname) (%1 vinfo))
           t2)
     (list (cons term1 term2)))
    ((list t1 (var (%0 vname) (%1 vinfo)))
     (list (cons term2 term1)))
    ((list (list '&rest t1) (list '&rest t2))
     (resolve-type-vars-one t1 t2))
    ((list (list '&rest rtype) _)
     (apply #'append
            (mapcar (curry #'resolve-type-vars-one rtype)
                    term2)))
    ((list _ (list '&rest rtype))
     (apply #'append
            (mapcar (rcurry #'resolve-type-vars rtype) term1)))
    ((list (cons t1 ts1)
           (cons t2 ts2))
     (append (resolve-type-vars-one t1 t2)
             (resolve-type-vars (mapcar #'cons ts1 ts2))))
    (_
     nil
     )))

(defun resolve-type-vars (constraints)
  "Unify CONSTRAINTS."
  (when constraints
    (let* ((substitution (resolve-type-vars (rest constraints)))
           (constraint (first constraints))
           (sub2
             (resolve-type-vars-one
              (apply-substitution substitution (car constraint))
              (apply-substitution substitution (cdr constraint)))))
      (append sub2 substitution))))

(defun solve-constraint (constraint solution)
  (format t "Solve constraint: ~a~%" constraint)
  ;;(break)
  (adt:match constraint constraint
    ((subtype type1 type2)
     ;;(break)
     #+nil(when (or (not (fully-solved-p type1))
                    (not (fully-solved-p type2)))
            (return-from solve-constraint (values solution nil)))
     (let ((type1 (apply-substitution* solution type1))
           (type2 (apply-substitution* solution type2)))
       (unless (types-compatible-p type1 type2)
         (cerror "Continue" 'type-inconsistency-error
                 :format-control "~a is not subtype of ~a"
                 :format-arguments (list type1 type2)))
       (values solution t)))    
    (_)
    ))

(defun solve (constraints &optional solution)
  "Solve CONSTRAINTS under current SOLUTIONitution."
  (format t "~%Solving ... ~%")
  (format t "Current solution:~%")
  (dolist (assignment solution)
    (format t "~a -> ~a~%" (car assignment) (cdr assignment)))
  (when (null constraints)
    (return-from solve solution))
  (let ((unsolved nil)
        (current-solution solution))
    (dolist (constraint constraints)
      ;;(format t "Constraint: ~a ~%" constraint)
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
  (ftypes nil :type list)
  (vartypes nil :type list)
  (debugp *debug-solver* :type boolean))

(defmacro with-type-env ((var &optional env) &body body)
  (if env
      `(let ((,var (copy-type-env ,env)))
         ,@body)
      `(let ((,var (make-type-env)))
         ,@body)))

(defmacro appendf-front (place &rest lists)
  `(setf ,place
         (append ,@lists ,place)))

(declaim (ftype (function (walked-form type-env) var)))
(defun new-var (form env)
  "Create a new type variable for FORM in ENV."
  (let* ((varname (intern (format nil "V~a" (incf (type-env-symbol-nr env)))))
         (var (var varname
                   (when (type-env-debugp env)
                     form))))
    (push (cons var form) (type-env-vars env))
    var))

(declaim (ftype (function (constraint type-env) t) add-constraint))
(defun add-constraint (constraint env)
  "Add constraint from type term X to type term Y, in ENV"
  (push constraint (type-env-constraints env)))

(declaim (ftype (function ((or symbol function) t type-env) t)
                get-func-type))
(defgeneric get-func-type (function-designator context type-env)
  (:documentation "Get the type for FUNCTION-DESIGNATOR in CONTEXT."))

(defmethod get-func-type ((func function) context env)
  (get-func-type (compiler-info:function-name func) context env))

(defvar *use-compiler-provided-types* t
  "When enabled, use types provided by the Lisp compiler.")

(defmethod get-func-type ((func-name symbol) context env)
  "Get the type of function with FNAME in ENV."
  (or
   ;; Try from locals in ENV first
   (cdr (assoc func-name (type-env-ftypes env)))
   ;; Then try to get from the read declarations
   (cdr (assoc func-name *funtypes*))
   ;; If none found, use compiler information
   (and *use-compiler-provided-types*
        (compiler-info:function-type func-name))
   ;; Or a generic function type
   '(function (&rest t) t)))

;; (get-func-type 'identity t (make-type-env))
;; (get-func-type 'concatenate t (make-type-env))
;; (get-func-type #'identity t (make-type-env))
;; (get-func-type #'+  t (make-type-env))

(defmethod get-func-type ((func (eql 'mapcar)) (form application-form) env)
  (assert (eql (operator-of form) 'mapcar))
  (let* ((mapcar-args-len (length (cdr (arguments-of form))))
         (vars (loop repeat mapcar-args-len
                     collect (gensym)))
         (return-type (gensym)))
    `(all (,@vars ,return-type)
          (function ((function ,vars ,return-type)
                     ,@(mapcar (lambda (type) `(list-of ,type)) vars))
                    (list-of ,return-type)))))

(declaim (ftype (function (walked-form type-env list) t) generate-type-constraints))
(defgeneric generate-type-constraints (form env locals)
  (:documentation "Generate type constraints for FORM in ENV under LOCALS."))

(defmethod generate-type-constraints ((form constant-form) env locals)
  (let ((var (new-var form env))
        (type (if (functionp (value-of form))
                  (get-func-type (value-of form) form env)
                  (type-of (value-of form)))))
    (add-constraint (unify var type) env)
    var))

(defmethod generate-type-constraints ((form let-form) env locals)
  (let ((let-locals locals))
    (dolist (binding (bindings-of form))
      (let ((binding-value-var
              (generate-type-constraints (initial-value-of binding) env locals)))
        (let ((binding-var (new-var binding env)))
          (add-constraint (unify binding-var binding-value-var) env)
          (push (cons (name-of binding) binding-var) let-locals))))
    ;; The type of the let is the type of the last expression in body, so return that
    (let ((body-var nil)
          (let-var (new-var form env)))
      (dolist (body-form (body-of form))
        (setf body-var (generate-type-constraints body-form env let-locals)))
      (add-constraint (unify let-var body-var) env)
      let-var)))

(defmethod generate-type-constraints ((form let*-form) env locals)
  (let ((let-locals locals))
    (dolist (binding (bindings-of form))
      (let ((binding-value-var
              (generate-type-constraints (initial-value-of binding) env let-locals)))
        (let ((binding-var (new-var binding env)))
          (add-constraint (unify binding-var binding-value-var) env)
          (push (cons (name-of binding) binding-var) let-locals))))
    ;; The type of the let is the type of the last expression in body, so return that
    (let ((body-var nil)
          (let-var (new-var form env)))
      (dolist (body-form (body-of form))
        (setf body-var (generate-type-constraints body-form env let-locals)))
      (add-constraint (unify let-var body-var) env)
      let-var)))

(defmethod generate-type-constraints ((form lexical-variable-reference-form) env locals)
  (let ((var (new-var form env))
        (local-var (or (cdr (find (name-of form) locals :key #'car))
                       (error "Badly done"))))
    (add-constraint (unify var local-var) env)
    var))

(defmethod generate-type-constraints ((form the-form) env locals)
  (let ((var (new-var form env))
        (value-type (generate-type-constraints (value-of form) env locals)))
    (add-constraint (unify var (declared-type-of form)) env)
    ;; The declared type is a subtype of the form type (coercion).
    (add-constraint (unify (declared-type-of form) value-type) env)
    ;; This causes trouble. How to check?
    ;;(add-constraint (subtype var value-type) env)    
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
    (add-constraint (unify var 't) env)
    var))

(defmethod generate-type-constraints ((form go-form) env locals)
  ;; what to do?
  (let ((var (new-var form env)))
    (add-constraint (unify var 't) env)
    var))

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
  (let ((arg-types (assign-types-from-function-type-2 func-type form))
        (arg-vars nil))

    ;; Constraint the types of the arguments
    (loop for arg in (arguments-of form)
          for arg-type in arg-types
          do
             (let ((arg-var (new-var arg env))
                   (actual-arg (generate-type-constraints arg env locals)))
               (add-constraint (unify arg-var (cdr arg-type)) env)
               (add-constraint (subtype actual-arg arg-var) env)
               (add-constraint (unify actual-arg (cdr arg-type)) env)
               (push arg-var arg-vars)))

    ;; Constraint the type of the application
    (let* ((return-type (lastcar func-type))
           (app-var (new-var form env)))
      (cond
        ;; Treat MAKE-INSTANCE specially
        ((and (eql (operator-of form) 'make-instance)
              (typep (first (arguments-of form)) 'constant-form))
         (add-constraint (unify app-var (value-of (first (arguments-of form)))) env))
        ;; Otherwise, the type of the application is the function return type
        (t
         (add-constraint (unify app-var return-type) env)
         (add-constraint (unify return-type app-var) env)
         ))
      app-var)))

(defmethod generate-type-constraints ((form application-form) env locals)
  (let ((func-type (instantiate-type (get-func-type (operator-of form) form env) env)))
    (generate-function-application-constraints
     func-type form env locals)))

(defmethod generate-type-constraints ((form free-function-object-form) env locals)
  (let ((var (new-var form env)))
    (add-constraint (unify var (instantiate-type (get-func-type (name-of form) form env) env)) env)
    var))

(defmethod generate-type-constraints ((form walked-lexical-variable-reference-form) env locals)
  (alexandria:if-let (local (assoc (name-of form) locals))
    (let ((var (new-var form env)))
      (add-constraint (unify var (cdr local)) env)
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
    (add-constraint (unify var `(function ,arg-types ,body-type)) env)
    var))

(defmethod generate-type-constraints ((form if-form) env locals)
  (let ((then-type (generate-type-constraints (then-of form) env locals))
        (else-type (generate-type-constraints (else-of form) env locals))
        (var (new-var form env)))
    (add-constraint (unify var `(or ,then-type ,else-type)) env)
    var))

(defun canonize-type (type)
  (trivia:match type
    ;; Clean the (values <type> &optional) types
    ((list 'values type '&optional)
     type)
    ((cons type-name args)
     (cons type-name (mapcar #'canonize-type args)))
    (_ type)))

(defun expanded-type (type)
  (trivia:match type
    ('list '(list-of t))
    ('cons '(cons-of t t))
    ('hash-table '(hash-table-of t t))
    ('function '(function (&rest t) t))
    ((list 'list-of type)
     `(cons-of ,type (list-of ,type)))
    (_ type)))

;; A substituion is a list of assignments
(defun apply-substitution (assignments term)
  (let ((new-term term))
    (dolist (assignment assignments)
      (setq new-term (subst-term assignment new-term)))
    new-term))

(defun apply-substitution* (assignments term)
  "Apply as much substitutions as possible."
  (let ((subst-term term)
        (last-subst-term nil))
    (loop while (not (equalp subst-term last-subst-term))
          do (setq last-subst-term subst-term)
             (setq subst-term (apply-substitution assignments subst-term)))
    subst-term))

(defun check-form (form &optional env)
  "Infer the type of FORM."
  (let ((type-env (or env (make-type-env)))
        (walked-form (hu.dwim.walker:walk-form form)))
    (generate-type-constraints walked-form type-env nil)
    ;; First resolve type variables
    (setf (type-env-solution type-env)
          (resolve-type-vars
           (mapcar (lambda (unify)
                     (adt:match constraint unify
                       ((unify t1 t2) (cons t1 t2))
                       (_)))
                   (remove-if-not (rcurry #'typep 'unify)
                                  (type-env-constraints type-env)))))
    ;; Then check type constraints
    (setf (type-env-solution type-env)
          (solve (remove-if (rcurry #'typep 'unify)
                            (type-env-constraints type-env))
                 (type-env-solution type-env)))    
    (let ((type-assignments nil))
      (dolist (type-assignment (type-env-solution type-env))
        (trivia:match type-assignment
          ((cons x type)
           (let ((expr (cdr (assoc x (type-env-vars type-env)))))
             (assert expr)
             (push (cons expr
                         (arrows:->> type
                                     (apply-substitution* (type-env-solution type-env))
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

(defun check-form* (&rest args)
  (first (multiple-value-list (apply #'check-form args))))
