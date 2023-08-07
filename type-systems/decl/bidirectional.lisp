;; https://davidchristiansen.dk/tutorials/bidirectional.pdf
;; https://www.youtube.com/watch?v=utyBNDj7s2w
;; https://jaked.org/blog/2021-09-07-Reconstructing-TypeScript-part-0

(defpackage :pluggable-types/bid
  (:use :cl :alexandria :hu.dwim.walker)
  (:export #:check-form
           #:type-checking-error
           #:types-compatible-p)
  (:import-from
   :pluggable-types/decl
   #:*funtypes*
   #:*vartypes*
   #:assign-types-from-function-type

   #:list-of
   #:cons-of
   #:all
   #:hash-table-of))

(in-package :pluggable-types/bid)

(define-condition type-checking-error (simple-error)
  ())

(defstruct type-env
  (symbol-nr 0 :type integer)
  (vars nil :type list)
  (declared-ftypes nil :type list)
  (declared-vartypes nil :type list)
  (debugp nil :type boolean))

(adt:defdata bid-type
  (unknown t)
  (var t t)
  (literal t))

(defun bid-type-error (message &rest args)
  (error 'type-checking-error
         :format-control message
         :format-arguments args))

(defun types-compatible-p (type1 type2)
  (trivia:match (list type1 type2)
    ((list (cons 'values vt1) (cons 'values vt2))
     (every (curry #'apply #'types-compatible-p)
            (mapcar #'cons vt1 vt2)))
    ((list (cons 'values vt1) type)
     (types-compatible-p (car vt1) type))
    ((list type (cons 'values vt2))
     (types-compatible-p type (car vt2)))
    ((list (list 'list-of type)
           (list 'cons-of a b))
     (types-compatible-p `(cons-of ,type (list-of ,type))
                         type2))
    ((list (list 'cons-of a b)
           (list 'list-of type))
     (types-compatible-p type1
                         `(cons-of ,type (list-of ,type))))
    ((list (list 'function f1args f1ret)
           (list 'function f2args f2ret))
     ;; TODO: parse lambda lists and check args
     (types-compatible-p f2ret f1ret))
    (_
     (or
      (some (rcurry #'typep 'var) (list type1 type2))
      (some (rcurry #'typep 'unknown) (list type1 type2))
      (subtypep type1 type2)))))

(defun ensure-types-compatible (type1 type2)
  (unless (types-compatible-p type1 type2)
    (bid-type-error "Types not compatible: ~a and ~a" type1 type2)))

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
    ;; If none found, use compiler information
    (or (compiler-info:function-type fname)
        ;; Or a generic function type
        '(function (&rest t) t))))

;; (get-func-type 'identity (make-type-env))
;; (get-func-type 'concatenate (make-type-env))
;; (get-func-type #'identity (make-type-env))
;; (get-func-type #'+ (make-type-env))

(declaim (ftype (function (walked-form type-env) var)))
(defun new-var (form env)
  "Create a new type variable for FORM in ENV."
  (let* ((varname (intern (format nil "VAR~a" (incf (type-env-symbol-nr env)))))
         (var (var varname
                   (when (type-env-debugp env)
                     form))))
    (push (cons var form) (type-env-vars env))
    var))

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


(defun more-informative-type (type1 type2)
  ;;(break)
  (cond
    ((or (typep type1 'unknown)
         (typep type1 'var))
     type2)
    ((or (typep type2 'unknown)
         (typep type2 'var))
     type1)
    ((and (pluggable-types/decl::tree-find-if (rcurry #'typep 'var)
                                              type1)
          (not (pluggable-types/decl::tree-find-if (rcurry #'typep 'var)
                                                   type2)))
     type2)
    ((and (pluggable-types/decl::tree-find-if (rcurry #'typep 'unknown)
                                              type2)
          (not (pluggable-types/decl::tree-find-if (rcurry #'typep 'unknown)
                                                   type1)))
     type1)
    (t (if (types-compatible-p type1 type2)
           type1
           type2))))

(defgeneric infer-type (form env locals))
(defgeneric bid-check-type (form type env locals))

(defmethod infer-type ((form constant-form) env locals)
  (if (functionp (value-of form))
      (get-func-type (value-of form) env)
      (type-of (value-of form))))

(defmethod infer-type ((form free-function-object-form) env locals)
  (instantiate-type (get-func-type (name-of form) env) env))

(defmethod infer-type ((form the-form) env locals)
  (bid-check-type (value-of form) (declared-type-of form) env locals)
  (declared-type-of form))

(defmethod bid-check-type ((form constant-form) type env locals)
  (let ((itype (infer-type form env locals)))
    (if (types-compatible-p itype type)
        (more-informative-type type itype)
        (bid-type-error "Types not compatible: ~a and ~a" type itype))))

(defmethod bid-check-type ((form the-form) type env locals)
  (ensure-types-compatible
   (infer-type (value-of form) env locals)
   (declared-type-of form))
  (ensure-types-compatible (declared-type-of form) type)
  (declared-type-of form))

(defmethod infer-type ((form walked-lexical-variable-reference-form) env locals)
  (alexandria:if-let (local (assoc (name-of form) locals))
    (cdr local)
    (error "Shouldn't happen")))

(defmethod infer-form ((form lambda-function-form) env locals)
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

(defmethod bid-check-type ((form walked-form) type env locals)
  (let ((inferred-type (infer-type form env locals)))
    (ensure-types-compatible inferred-type type)
    ;;(more-informative-type type inferred-type)
    inferred-type
    ))

(defmethod bid-check-type ((form let-form) type env locals)
  (let ((let-locals locals))
    (dolist (binding (bindings-of form))
      (let ((binding-type
              (infer-type (initial-value-of binding) env locals)))
        (push (cons (name-of binding) binding-type) let-locals)))
    ;; The type of the let is the type of the last expression in body, so return that
    (let ((body-type (unknown nil)))
      (dolist (body-form (body-of form))
        (setf body-type (infer-type body-form env let-locals)))
      body-type)))

(defmethod bid-check-type ((form let*-form) type env locals)
  (let ((let-locals locals))
    (dolist (binding (bindings-of form))
      (let ((binding-type
              (infer-type (initial-value-of binding) env let-locals)))
        (push (cons (name-of binding) binding-type) let-locals)))
    ;; The type of the let is the type of the last expression in body, so return that
    (let ((body-type (unknown nil)))
      (dolist (body-form (body-of form))
        (setf body-type (infer-type body-form env let-locals)))
      body-type)))

(defun subst-all (pairs tree &key key test test-not)
  "Substitute all PAIRS of things in TREE.
PAIRS is a list of CONSes, with (old . new)."
  (if (null pairs)
      tree
      (let ((pair (first pairs)))
        (apply #'subst
               (cdr pair)
               (car pair)
               (subst-all (rest pairs) tree
                          :key key :test test :test-not test-not)
               (append
                (when key
                  (list :key key))
                 (when test
                   (list :test test))
                 (when test-not
                   (list :test-not test-not)))))))

(defmethod bid-check-type ((form application-form) type env locals)
  (let ((inferred-type (infer-type form env locals)))
    (ensure-types-compatible inferred-type type)
    (more-informative-type inferred-type type)))

(defun extract-var-assignments (assignment)
  (destructuring-bind (t1 . t2) assignment
    (cond
      ((typep t1 'var)
       (list (cons t1 t2)))
      ((typep t2 'var)
       (list (cons t2 t1)))
      ((and (listp t1) (listp t2)
            (eql (car t1) (car t2)))
       (apply #'append
              (mapcar #'extract-var-assignments
                      (mapcar #'cons (rest t1) (rest t2)))))
      (t nil))))

(defun extract-var-assignments* (assignments)
  (apply #'append (mapcar #'extract-var-assignments assignments)))

(defmethod infer-type ((form free-application-form) env locals)
  (call-with-type-combinations
   (get-func-type (operator-of form) env)
   (lambda (abstract-func-type)                            
     (let* ((args (arguments-of form))
            (func-type (instantiate-type abstract-func-type env))
            (formal-arg-types (assign-types-from-function-type func-type args)))

       (call-with-types-combinations
        formal-arg-types
        (lambda (formal-arg-types)                             

          ;; Check the types of the arguments
          (let ((checked-arg-types
                  (loop for arg in args
                        for arg-type in formal-arg-types
                        collect (bid-check-type arg (cdr arg-type) env locals))))
            ;; Unify terms with type variables
            (let ((subst (unify
                          (remove-if-not (curry #'pluggable-types/decl::tree-find-if (rcurry #'typep 'var))
                                         (mapcar #'cons
                                                 (mapcar #'cdr formal-arg-types)
                                                 checked-arg-types
                                                 )))))
              (apply-substitution* subst (lastcar func-type))))))))))

(defun check-form (form &optional env)
  (let ((env (or env (make-type-env)))
        (form (if (typep form 'walked-form)
                  form
                  (walk-form form))))
    (bid-check-type form (unknown form) env nil)))

(define-condition type-unification-error (type-checking-error)
  ())

(defun type-unification-error (args &optional message)
  (cerror "Continue"
          'type-unification-error
         :format-control (or message "Can't unify: ~{~a~^, ~}")
         :format-arguments (list args)))

(defun unify-one (term1 term2)
  (when (eql term1 term2)
    (return-from unify-one nil))
  (trivia:match (list term1 term2)
    ((list (cons 'values ts1) (cons 'values ts2))
     (unify (mapcar #'cons ts1 ts2)))
    ((list (cons 'values values) type)
     (unify-one (car values) type))
    ((list type (cons 'values values))
     (unify-one type (car values)))
    ((list (list 'cons-of a b)
           (list 'list-of type))
     (unify-one term1 `(cons-of ,type (list-of ,type))))
    ((list (var (%0 vname) (%1 vinfo))
           t2)
     (list (cons term1 term2)))
    ((list t1 (var (%0 vname) (%1 vinfo)))
     (list (cons term2 term1)))
    ((list (list '&rest t1) (list '&rest t2))
     (unify-one t1 t2))
    ((list (list '&rest rtype) _)
     (unify-one rtype term2))
    ((list _ (list '&rest rtype))
     (unify (mapcar (rcurry #'cons rtype) term1)))
    ((list (cons t1 ts1)
           (cons t2 ts2))
     (unify (mapcar #'cons term1 term2)))
    (_
     (unless (types-compatible-p term1 term2)
       (type-unification-error (list term1 term2))))))

(unify-one 'integer 'integer)
(unify-one (var 'x nil) 'integer)
(unify-one 'integer (var 'x nil))
(unify-one (list 'lala (var 'x nil))
           (list 'lala (var 'y nil)))
(unify-one (list 'integer)
           (list (var 'x nil)))

(declaim (ftype (function (cons t) t) subst-term))
(defun subst-term (assignment term)
  "Substitute ASSIGNMENT in TERM.
ASSIGNMENT is CONS of VAR to a TERM."
  (when (null term)
    (return-from subst-term term))
  (trivia:match (list assignment term)
    ((list (cons (var (%0 x)) val)
           (var (%0 y)))
     (if (eql x y)
         val
         term))
    (_
     (cond
       ((listp term)
        (cons (subst-term assignment (car term))
              (mapcar (curry 'subst-term assignment) (cdr term))))
       (t term)))))

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

(declaim (ftype (function ((list-of cons)) list) unify))
(defun unify (constraints)
  "Unify CONSTRAINTS."
  (when constraints
    (let* ((substitution (unify (rest constraints)))
           (constraint (first constraints))
           (sub2
             (unify-one (apply-substitution substitution (car constraint))
                        (apply-substitution substitution (cdr constraint)))))
      (append sub2 substitution))))

;; Type cases

(defun case-type-p (type)
  (and (listp type)
       (eql (car type) 'case)))

(defun type-cases (type)
  (if (case-type-p type)
      (rest type)
      (list type)))

(defun %call-with-types-combinations (type-cases types func)
  (when (null types)
    (return-from %call-with-types-combinations (funcall func (reverse type-cases))))
    
  (destructuring-bind (type . rest-types) types
    (if (case-type-p type)
        (loop with type-type-cases = (type-cases type)
              for type-case = (pop type-type-cases) then (pop type-type-cases)
              while type-case
              do
                 (if (null type-cases)
                     ;; If this is the last case, then don't handle error and fail potentially
                     (%call-with-types-combinations (cons type-case type-cases) rest-types func)
                     ;; otherwise, handle error and try with another case
                     (handler-case
                         (%call-with-types-combinations (cons type-case type-cases) rest-types func)
                         (error ()))))
          ;; if not a case type, just call with the type
          (%call-with-types-combinations (cons type type-cases) rest-types func))))


(defun call-with-types-combinations (types func)
  (%call-with-types-combinations nil types func))

(defun call-with-type-combinations (type func)
  (%call-with-types-combinations nil (list type)
                                 (compose func #'car)))

(defmacro with-types-combinations (types &body body)
  `(call-with-types-combinations ,types (lambda (,types) ,@body)))

(defmacro with-type-combinations (type &body body)
  `(call-with-type-combinations ,type (lambda (,type) ,@body)))
