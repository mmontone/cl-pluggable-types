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
;; Perhaps: instead of having cases, have only the parametric type, and the algorithm can unify the arguments to T if the things are not monomorphic.
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

;; Example type:
(deftype my-type ()
  `(list-of (cons-of string pathname)))

;; Another interesting feature of a pluggable type system, apart from parameterized types, would be to typecheck the functions that are passed as symbols.

(deftype function-name ()
  '(and (or symbol
            (cons (eql setf)
                  (cons (and symbol (not (member nil t)))
                        null)))
        (not (member null t))))

(deftype function-designator ()
  '(or function function-name))

#||

if we strictly followed CLHS, then it should be the following:

(def (type e) function-designator ()
  '(or function '(and symbol (not (member nil t)))))

(def (type e) extended-function-designator ()
  '(or function function-name))

||#

(deftype function* (args-types return-type)
  (declare (ignore args-types return-type))
  `function-designator)

;; Now that type accepts both symbols and functions, and provides type checking.

;; Then the typechecker:
;; If the passed function-designator is a symbol, the type system checks that,
;; if the symbol is the name of a function with a type, and uses that type.
