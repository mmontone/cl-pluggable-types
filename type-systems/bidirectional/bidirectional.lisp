;; https://davidchristiansen.dk/tutorials/bidirectional.pdf
;; https://www.youtube.com/watch?v=utyBNDj7s2w
;; https://jaked.org/blog/2021-09-07-Reconstructing-TypeScript-part-0

(in-package :pluggable-types/bid)

(defvar *use-compiler-provided-types* t
  "When enabled, use types provided by the Lisp compiler.")

(define-condition type-checking-error (simple-error)
  ((form :initarg :form
         :accessor error-form)
   (source :initarg :source
           :accessor error-source)
   (position :initarg :position
             :accessor error-position)))

(defstruct type-env
  (symbol-nr 0 :type integer)
  (vars nil :type list)
  (ftypes nil :type list)
  (vartypes nil :type list)
  (debugp nil :type boolean))

(defmacro with-type-env ((var &optional env) &body body)
  (if env
      `(let ((,var (copy-type-env ,env)))
         ,@body)
      `(let ((,var (make-type-env)))
         ,@body)))

(defmacro appendf-front (place &rest lists)
  `(setf ,place
         (append ,@lists ,place)))

(adt:defdata (bid-type :mutable t)
  (unknown t)
  (var t t)
  (literal t))

(defun bid-type-error (message &rest args)
  (error 'type-checking-error
         :format-control message
         :format-arguments args))

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
            (mapcar #'cons vt1 vt2)))
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
    ((list _ (satisfies unknown-p))
     (adt:set-data type2 (unknown type1))
     t)
    ((list (satisfies unknown-p) _)
     (adt:set-data type1 (unknown type2))
     t)
    (_
     (or
      (some (rcurry #'typep 'var) (list type1 type2))
      (some (rcurry #'typep 'unknown) (list type1 type2))
      (subtypep type1 type2)))))

(defun ensure-types-compatible (type1 type2)
  (unless (types-compatible-p type1 type2)
    (bid-type-error "A ~a cannot be used as a ~a" type1 type2)))

(declaim (ftype (function ((or symbol function) t type-env) t)
                get-func-type))
(defgeneric get-func-type (function-designator context type-env)
  (:documentation "Get the type for FUNCTION-DESIGNATOR in CONTEXT."))

(defmethod get-func-type ((func function) context env)
  (get-func-type (compiler-info:function-name func) context env))

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
    ((and (tree-find-if (rcurry #'typep 'var)
                        type1)
          (not (tree-find-if (rcurry #'typep 'var)
                             type2)))
     type2)
    ((and (tree-find-if (rcurry #'typep 'unknown)
                        type2)
          (not (tree-find-if (rcurry #'typep 'unknown)
                             type1)))
     type1)
    (t (if (types-compatible-p type1 type2)
           type1
           type2))))

(defgeneric infer-type (form env))
(defgeneric bid-check-type (form type env))

(defmethod infer-type ((form constant-form) env)
  (if (functionp (value-of form))
      (get-func-type (value-of form) form env)
      (type-of (value-of form))))

(defmethod infer-type ((form free-function-object-form) env)
  (instantiate-type (get-func-type (name-of form) form env) env))

(defmethod infer-type ((form the-form) env)
  ;; declared type is subtype of form type. coercion.
  (bid-check-type (declared-type-of form) (value-of form) env)
  (declared-type-of form))

(defmethod bid-check-type ((form constant-form) type env)
  (let ((itype (infer-type form env)))
    (ensure-types-compatible itype type)
    (more-informative-type type itype)))

(defmethod infer-type ((form walked-lexical-variable-reference-form) env)
  (alexandria:if-let (local (assoc (name-of form)
                                   (type-env-vartypes env)))
    (cdr local)
    (error "Shouldn't happen")))

(defun parse-type-declarations (declarations)
  (let ((types))
    (dolist (declaration declarations)
      (typecase declaration
        (type-declaration-form
         (push (cons (name-of declaration)
                     (declared-type-of declaration))
               types))))
    types))

(defun parse-ftype-declarations (declarations)
  (let ((types))
    (dolist (declaration declarations)
      (typecase declaration
        (ftype-declaration-form
         (push (cons (name-of declaration)
                     (declared-type-of declaration))
               types))))
    types))

(defmethod infer-type ((form lambda-function-form) env)
  (let* ((lambda-locals (append (parse-type-declarations (declarations-of form))
                                (mapcar (lambda (arg)
                                          (cons (name-of arg) (unknown arg)))
                                        (bindings-of form))))
         (arg-types (loop for arg-name in (mapcar #'name-of (bindings-of form))
                          collect (cdr (assoc arg-name lambda-locals))))
         (body-type (unknown form)))
    (with-type-env (env env)
      (appendf-front (type-env-vartypes env) lambda-locals)
      (dolist (body-form (body-of form))
        (setq body-type (bid-check-type body-form body-type env)))
      `(function ,arg-types ,body-type))))

(defmethod bid-check-type ((form walked-form) type env)
  (let ((inferred-type (infer-type form env)))
    (ensure-types-compatible inferred-type type)
    ;;(more-informative-type type inferred-type)
    inferred-type
    ))

(defmethod infer-type ((form let-form) env)
  (let ((let-locals '()))
    (dolist (binding (bindings-of form))
      (let ((binding-type
              (infer-type (initial-value-of binding) env)))
        (push (cons (name-of binding) binding-type) let-locals)))
    ;; The type of the let is the type of the last expression in body, so return that
    (with-type-env (env env)
      (appendf-front (type-env-vartypes env) let-locals)
      (let ((body-type (unknown nil)))
        (dolist (body-form (body-of form))
          (setf body-type (infer-type body-form env)))
        body-type))))

(defmethod infer-type ((form let*-form) env)
  (let ((let-locals '()))
    (dolist (binding (bindings-of form))
      (with-type-env (env env)
        (appendf-front (type-env-vartypes env) let-locals)
        (let ((binding-type
                (infer-type (initial-value-of binding) env)))
          (push (cons (name-of binding) binding-type) let-locals))))
    ;; The type of the let is the type of the last expression in body, so return that
    (with-type-env (env env)
      (appendf-front (type-env-vartypes env) let-locals)
      (let ((body-type (unknown nil)))
        (dolist (body-form (body-of form))
          (setf body-type (infer-type body-form env)))
        body-type))))

(defmethod infer-type ((form setq-form) env)
  (let ((var-type (or (cdr (find (name-of (variable-of form))
                                 (type-env-vartypes env)
                                 :key #'car))
                      (error "Fix this"))))
    (bid-check-type (value-of form) var-type env)))

(defmethod infer-type ((form symbol-macrolet-form) env)
  (let ((body-type t))
    (dolist (body-form (body-of form))
      (setq body-type (infer-type body-form env)))
    body-type))

(define-condition return-type-condition ()
  ((form :initarg :form
         :accessor return-type-form)
   (block :initarg :block
          :accessor return-type-block)
   (type :initarg :type
         :accessor return-type)))

(defmethod infer-type ((form block-form) env)
  (let ((return-types '()))
    (handler-bind
        ((return-type-condition
           (lambda (c)
             (when (eq (return-type-block c) form)
               (push (return-type c) return-types)))))
      (let ((body-type t))
        (dolist (body-form (body-of form))
          (setq body-type (infer-type body-form env)))
        (if (null return-types)
            body-type
            `(or ,body-type ,@return-types))))))

(defmethod infer-type ((form return-from-form) env)
  (let ((type (infer-type (result-of form) env)))
    (signal 'return-type-condition
            :type type
            :form form
            :block (target-block-of form))
    type))

(defmethod infer-type ((form implicit-progn-mixin) env)
  (let ((body-type t))
    (dolist (body-form (body-of form))
      (setq body-type (infer-type body-form env)))
    body-type))

(defmethod infer-type ((form go-tag-form) env)
  't)

(defmethod infer-type ((form go-form) env)
  't)

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

(defmethod bid-check-type ((form application-form) type env)
  (let ((inferred-type (infer-type form env)))
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

(defgeneric infer-application-type (operator form env))

(defmethod infer-type ((form application-form) env)
  (infer-application-type (operator-of form) form env))

(defmethod infer-application-type (operator (form application-form) env)
  (declare (ignore operator))

  (let* ((abstract-func-type (get-func-type (operator-of form) form env))
         (args (arguments-of form))
         (func-type (instantiate-type abstract-func-type env)))
    (trivia:match func-type
      ('function
       ;; We have nothing to infer in this case
       t)
      ((cons 'function _)
       ;; A function type with arguments and return types
       ;; Assignment of passed args to function args types:
       (let ((args-types-assignment (assign-types-from-function-type-2 func-type form)))
         (if (concrete-type-p abstract-func-type)
             (progn
               ;; There are no type variables in function type.
               ;; Simply type check the arguments and result.
               ;; Check the types of the arguments
               (loop for (arg . arg-type) in args-types-assignment
                     do (bid-check-type arg arg-type env))
               ;; Return the type of the application
               (lastcar func-type))
             ;; else, the type of the function is generic
             ;; so, resolve the type variables to the inferred types of the arguments
             ;; finally typecheck with the resolved types.
             (let* ((arg-types (mapcar (rcurry #'infer-type env) args))
                    (subst (resolve-type-vars (list (cons `(function ,arg-types ,(lastcar func-type))
                                                          func-type ))))
                    (solved-arg-types (mapcar (curry #'apply-substitution* subst) (second func-type))))
               (loop for arg in args
                     for arg-type in solved-arg-types
                     do (bid-check-type arg arg-type env))
               (apply-substitution* subst (lastcar func-type))))))
      (_
       (error "Type checker error. Fix this.")))))

(defmethod infer-type ((form if-form) env)
  (let ((then-type (infer-type (then-of form) env))
        (else-type (infer-type (else-of form) env)))
    `(or ,then-type ,else-type)))

;; For local FUNCTION-BINDING-FORMs, we use their declared type.
;; If not declared, we then use 'FUNCTION as type.
;; For example:

#|
(flet ((sum (x y)
(+ x y)))
(declare (ftype (function (integer integer) integer) sum))
(mapcar #'sum
(the (list-of number)
(list 1 2 3))
(the (list-of integer)
(list 3 4 5))))
|#

;; Use the declared type of functions in scope

(defmethod infer-type ((form flet-form) env)
  (let ((ftype-declarations (parse-ftype-declarations
                             (declarations-of form))))
    (dolist (fbinding (bindings-of form))
      (let ((ftype (or (cdr (assoc (name-of fbinding) ftype-declarations))
                       '(function (&rest t) t))))
        (bid-check-type fbinding ftype env)))
    (let ((body-type (unknown nil)))
      (with-type-env (env env)
        (appendf-front (type-env-ftypes env) ftype-declarations)
        (dolist (body-form (body-of form))
          (setq body-type (infer-type body-form env))))
      body-type)))

(defmethod infer-type ((form walked-lexical-function-object-form) env)
  "Get the type from the environment."
  (get-func-type (name-of form) form env))

(defmethod infer-type ((form progn-form) env)
  (let ((body-type (unknown nil)))
    (dolist (body-form (body-of form))
      (setq body-type (infer-type body-form env)))
    body-type))

(defmethod infer-type ((form special-variable-reference-form) env)
  (or (declared-type-of form)
      (and *use-compiler-provided-types*
           (compiler-info:variable-type (name-of form)))
      (unknown form)))

(defmethod infer-type ((form walked-form) env)
  (error "Implement: ~s" form))

(defun check-form (form &optional env)
  (let ((env (or env (make-type-env)))
        (form (if (typep form 'walked-form)
                  form
                  (walk-form form))))
    (bid-check-type form (unknown form) env)))

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

;; Type vars resolution

(defun expanded-type (type)
  (trivia:match type
    ('list '(list-of t))
    ('cons '(cons-of t t))
    ('hash-table '(hash-table-of t t))
    ('function '(function (&rest t) t))
    (_ type)))

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

(declaim (ftype (function ((list-of cons)) list) resolve-type-vars))
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

(defun generic-type-p (type)
  (and (listp type)
       (eql (car type) 'all)))

(defun concrete-type-p (type)
  (not (generic-type-p type)))

(defun tree-find-if (predicate tree)
  (cond
    ((atom tree)
     (funcall predicate tree))
    ((consp tree)
     (or (tree-find-if predicate (car tree))
         (tree-find-if predicate (cdr tree))))))

(defun fully-resolved-p (thing)
  (not (tree-find-if (rcurry #'typep 'type-var) thing)))

;; Type cases

(defun case-type-p (type)
  (and (listp type)
       (eql (car type) 'case)))

(defun type-cases (type)
  (if (case-type-p type)
      (rest type)
      (list type)))

(defun %call-with-types-combinations (types-combination types func)
  (when (null types)
    (return-from %call-with-types-combinations (funcall func (reverse types-combination))))

  (destructuring-bind (type . rest-types) types
    (if (case-type-p type)
        (loop with type-cases = (type-cases type)
              for type-case = (pop type-cases) then (pop type-cases)
              while type-case
              do
                 (if (null type-cases)
                     ;; If this is the last case, then don't handle error and fail potentially
                     (return-from %call-with-types-combinations
                       (%call-with-types-combinations (cons type-case types-combination) rest-types func))
                     ;; otherwise, handle error and try with another case
                     (handler-case
                         (return-from %call-with-types-combinations
                           (%call-with-types-combinations (cons type-case types-combination) rest-types func))
                       (error ()
                         ;;(break "Try another case")
                         ))))
        ;; if not a case type, just call with the type
        (%call-with-types-combinations (cons type types-combination) rest-types func))))

(defun call-with-types-combinations (types func)
  (%call-with-types-combinations nil types func))

(defun call-with-type-combinations (type func)
  (%call-with-types-combinations nil (list type)
                                 (compose func #'car)))

(defmacro with-types-combinations (types &body body)
  `(call-with-types-combinations ,types (lambda (,types) ,@body)))

(defmacro with-type-combinations (type &body body)
  `(call-with-type-combinations ,type (lambda (,type) ,@body)))


(defun available-class-slots (class)
  (multiple-value-bind (slots allow-other-keys-p)
      (if (swank-mop:class-finalized-p class)
          (values (closer-mop:class-slots class) nil)
          (values (closer-mop:class-direct-slots class) t))
    (values slots allow-other-keys-p)))

(defmethod infer-application-type ((operator (eql 'make-instance))
                                   (form application-form) env)
  (let ((class-designator (first (arguments-of form))))
    (when (not (and (typep class-designator 'constant-form)
                    (symbolp (value-of class-designator))))
      (return-from infer-application-type
        (call-next-method)))
    (let ((class (find-class (value-of class-designator) nil)))
      (unless class
        (error 'type-checking-error
               :format-control "Class not defined: ~a"
               :format-arguments (list (value-of class-designator))))
      (let* ((args (cdr (arguments-of form)))
             (class-slots (available-class-slots class))
             (initargs (apply #'append (mapcar #'closer-mop:slot-definition-initargs class-slots))))
        (loop for initkey in args by #'cddr
              for initval in (rest args) by #'cddr
              do
                 (when (and (typep initkey 'constant-form)
                            (keywordp (value-of initkey)))
                   (unless (find (value-of initkey) initargs)
                     (error 'type-checking-error
                            :format-control "Invalid initarg: ~s. Valid initargs: ~s"
                            :format-arguments (list (value-of initkey) initargs)))
                   (let ((slot (find-if (lambda (slot)
                                          (member (value-of initkey) (closer-mop:slot-definition-initargs slot)))
                                        class-slots)))
                     (bid-check-type initval (closer-mop:slot-definition-type slot) env))))
        ;; The type of make instance is the name of the class
        (class-name class)))))
