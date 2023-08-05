;; https://davidchristiansen.dk/tutorials/bidirectional.pdf
;; https://www.youtube.com/watch?v=utyBNDj7s2w
;; https://jaked.org/blog/2021-09-07-Reconstructing-TypeScript-part-0

(defpackage :pluggable-types/bid
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
  (or
   (some (rcurry #'typep 'var) (list type1 type2))
   (some (rcurry #'typep 'unknown) (list type1 type2))
   (subtypep type1 type2)))

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
  (cond
    ((or (typep type1 'unknown)
         (typep type1 'var))
     type2)
    ((or (typep type2 'unknown)
         (typep type2 'var))
     type1)
    (t (if (subtypep type1 type2)
           type1
           type2))))

(defgeneric infer-type (form env locals))
(defgeneric bid-check-type (form type env locals))

(defmethod infer-type ((form constant-form) env locals)
  (if (functionp (value-of form))
      (get-func-type (value-of form) env)
      (type-of (value-of form))))

(defmethod infer-type ((form the-form) env locals)
  (bid-check-type form (declared-type-of form) env locals))

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
  (ensure-types-compatible (infer-type form env locals) type)
  type)

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
  (let* ((func-type (instantiate-type (get-func-type (operator-of form) env) env))
         (arg-types (assign-types-from-function-type func-type (arguments-of form)))
         (arg-type-assignments ()))
    ;; Check the types of the arguments
    (loop for arg in (arguments-of form)
          for arg-type in arg-types
          do
             (let ((checked-arg-type (bid-check-type arg (cdr arg-type) env locals)))
               (push (cons (cdr arg-type) checked-arg-type)
                     arg-type-assignments)))
    (let ((var-assignments (extract-var-assignments* arg-type-assignments)))
      (subst-all var-assignments (lastcar func-type)))))

(defun bid-check (form &optional env)
  (let ((env (or env (make-type-env)))
        (form (if (typep form 'walked-form)
                  form
                  (walk-form form))))
    (bid-check-type form (unknown form) env nil)))
