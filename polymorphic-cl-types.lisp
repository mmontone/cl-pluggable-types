(defpackage :polymorphic-cl-types
  (:use :cl :polymorphic-types))

(in-package :polymorphic-cl-types)

(declaim (declaration ftype*))

(declaim
 (ftype* (all (a) (function (a) a))
         identity)
 ;; Not possible to type mapcar with monomorphic lists
 ;;(ftype* (all (a b) (function ((function (a) b) (list-of a)) (list-of b)))
 ;;        mapcar)
 ;; We can only constrain the return type:
 ;;(ftype* (all (b) (function (function (&rest t) b) list &rest list) (list-of b)))
 ;;        mapcar)
 ;; But mapcar can also take a symbol as function, so we cannot do that,
 ;; unless we introduce conditionals in types.
 (ftype* (case
             (all (a b) (function ((function (a) b) (list-of a)) (list-of b)))
           (all (b) (function ((function (&rest t) b) list &rest list) (list-of b)))
           (function (function list &rest list) list))
         mapcar)           
 (ftype* (all (a) (function (unsigned-byte (list-of a)) a))
         nth)
 (ftype* (all (a) (function ((list-of a)) a))
         first)
 (ftype* (all (a) (function ((list-of a)) (list-of a)))
         rest)
 (ftype* (all (a b) (function ((cons-of a b)) a))
         car)
 (ftype* (all (a b) (function ((cons-of a b)) b))
         cdr)
 (ftype* (all (a b) (function (a b) (cons-of a b)))
         cons)
 (ftype* ;;(all (a) (function (&rest a) (list-of a)))
        (function (&rest t) list)
        list)
 (ftype*
  (all (a b)
       (function (a (alist-of a b)
                    &key (:test function-designator)
                    (:test-not function-designator)
                    (:key function-designator))
                 (or b null)))
  assoc)
 (ftype* (all (a b) (function (a (hash-table-of a b) &optional b) (values (or b null) boolean)))
         gethash)
 (ftype* (all (b) (function ((function (&rest t) b) &rest t) b))
         funcall))
