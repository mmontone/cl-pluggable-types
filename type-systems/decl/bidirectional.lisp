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

(defun type-equivalent-p (type1 type2)
  (or (some (rcurry #'typep 'unknown) (list type1 type2))
      (subtypep type1 type2)))

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

(defun more-informative-type (type1 type2)
  (cond
    ((typep type1 'unknown)
     type2)
    ((typep type2 'unknown)
     type2)
    (t (if (subtypep type1 type2)
           type1
           type2))))

(defgeneric infer-type (form env locals))

(defmethod infer-type ((form constant-form) env locals)
  (if (functionp (value-of form))
      (get-func-type (value-of form) env)
      (type-of (value-of form))))

(defgeneric bid-check-type (form type env locals))

(defmethod bid-check-type ((form constant-form) type env locals)
  (let ((itype (infer-type form env locals)))
    (if (type-equivalent-p itype type)
        (more-informative-type type itype)
        (bid-type-error "Types equivalent: ~a and ~a" type itype))))

(defun bid-check (form &optional env)
  (let ((env (or env (make-type-env)))
        (form (if (typep form 'walked-form)
                  form
                  (walk-form form))))
    (bid-check-type form (unknown form) env nil)))
