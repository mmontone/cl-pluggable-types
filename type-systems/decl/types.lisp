(in-package :pluggable-types/decl)

(declaim (declaration type* ftype*))

(deftype list-of (a) 
  (declare (ignore a))
  'list)

(deftype cons-of (a b)
  (declare (ignore a b))
  'cons)

;; Several type cases
;; If the list is declared monomorphic, then the parametric type can be applied.
;; Otherwise, typecheck with the weaker type.
(declaim (ftype* ((all (a b)
                       (function ((function (a) b)
                                  (list-of a))
                                 (list-of b)))
                  (function ((function t t) list) list))
                 mapcar))

(deftype alist ()
  'list)

(deftype alist-of (from to)
  (declare (ignore from to))
  'list)

(declaim (ftype* ((all (a b)
                       (function (a (alist-of a b)
                                    &key (:test (or function symbol))
                                    (:test-not (or function symbol))
                                    (:key (or function symbol)))
                                 (or b null)))
                  (function
                   (t list &key (:test (or function symbol))
                      (:test-not (or function symbol))
                      (:key (or function symbol)))))
                 assoc))

(declaim (ftype* ((all (a) (function ((list-of a)) a)) ;; monomorphic list
                  (all (a) (function (cons a t) a)) ;; typed cons
                  (function (list) t))
                 car))

(declaim (ftype* ((all (a) (function (unsigned-byte (list-of a)) a))
                  (function (unsigned-byte list) t))
                 nth))
