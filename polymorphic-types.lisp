(defpackage :polymorphic-types
  (:use :cl)
  (:export
   #:all
   #:cons-of
   #:list-of
   #:sequence-of
   #:alist
   #:alist-of
   #:hash-table-of
   #:function-name
   #:function-designator
   #:ftype*
   #:type*))

(in-package :polymorphic-types)

(declaim (declaration ftype* type*))

(deftype list-of (a) 
  (declare (ignore a))
  'list)

(deftype sequence-of (a)
  (declare (ignore a))
  'sequence)

(deftype cons-of (a b)
  (declare (ignore a b))
  'cons)

(deftype optional (a)
  `(or ,a null))

(deftype alist ()
  'list)

(deftype alist-of (from to)
  (declare (ignore from to))
  'list)

(deftype hash-table-of (key-type value-type)
  (declare (ignore key-type value-type))
  `hash-table)

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

(deftype all (args body)
  (let ((substs (mapcar (lambda (arg)
                          (cons arg t))
                        args)))
    (subst-all substs body)))

;; Example type:
(deftype my-type ()
  `(list-of (cons-of string pathname)))
