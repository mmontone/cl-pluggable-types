(fiasco:define-test-package :pluggable-types/bid/tests
  (:use :cl :fiasco :pluggable-types/bid :arrows)
  (:import-from
   :pluggable-types/decl
   #:*funtypes*
   #:*vartypes*
   #:assign-types-from-function-type

   #:list-of
   #:cons-of
   #:all
   #:hash-table-of))

(in-package :pluggable-types/bid/tests)

(defmacro check-is-equalp (form type)
  `(is (equalp (check-form ',form) ',type)))

(defmacro check-is-subtypep (form type)
  `(is (types-compatible-p (check-form ',form) ',type)))

(defmacro check-signals-error (form)
  `(signals type-checking-error
     (check-form ',form)))

(deftest check-constant-tests ()
  (check-is-subtypep 22 number)
  (check-is-equalp '(1 2 3) (list-of number)))

(deftest function-application-tests ()
  (check-signals-error (+ 22 "lala"))
  (check-is-equalp (+ 22 40) (values number &optional)))

(deftest check-let-tests ()
  (check-is-subtypep (let ((x "lla")) x) string)
  (-> (let ((x 34)
           (y 56))
       (+ x (- y x)))
     (check-is-subtypep number)))

(deftest check-parametric-types-test ()
  ;; Evaluates to integer! : uses the (all (a) (function (a) a)) type !! :-)
  (check-is-subtypep (identity 22) number))

(deftest check-the-form-test ()
  (is (equalp (check-form '(the (list-of integer) '(1 2 3)))
              '(list-of integer)))
  (signals type-checking-error (check-form '(the (list-of integer) 22)))
  (is (equalp (check-form '(the (list-of integer) '("asdf")))
              '(list-of integer))))

(deftest check-parametric-types-test-2 ()
  (check-is-equalp (mapcar #'+ (the (list-of number) '(1 2 3)))
                   (list-of number))
  (check-is-equalp (mapcar #'+ '(1 2 3))
                   (list-of number))
  (check-signals-error
   (mapcar #'+ (the (list-of string) '("lala" 2 3))))
  (check-is-equalp
   (mapcar #'identity (the (list-of string) '("lala" 2 3)))
   (list-of string))
  (check-is-equalp (mapcar #'identity (the (list-of string) '("lala" 2 3)))
                   (list-of string))
  (check-is-equalp
   (mapcar #'print (the (list-of string) '("lala")))
   (list-of t))

  ;; (check-form '(mapcar #'identity (the list '("lala"))))

  ;; (check-form '(mapcar #'identity '("lala")))

  ;; (check-form '(mapcar #'identity (the (list-of t) '("lala"))))

  (check-is-subtypep (nth 10 (the (list-of string) '("foo" "bar")))
                     string)
  (check-signals-error (nth "lala" (the (list-of string) '("foo" "bar"))))

  (check-is-subtypep
   (let ((list (mapcar #'identity (the (list-of string) '("lala")))))
     (nth 1 list))
   string)

  (check-is-subtypep
   (let ((list (mapcar #'identity (the (list-of string) '("lala")))))
     (first list))
   string)

  (check-is-subtypep
   (let ((list (mapcar #'identity (the (list-of string) '("lala")))))
     (rest list))
   (list-of string)))

(deftest check-cons-tests ()
  (check-is-equalp (car (the (cons-of integer string) (cons 2 "lala")))
                   integer)
  (check-is-equalp (cdr (the (cons-of integer string) (cons 2 "lala")))
                   string))

#+nil(deftest lambda-tests ()
  (check-is-equalp (lambda (x) x) (all (a) (function (a) a)))
  (check-is-equalp (lambda (x y) x) (all (a b) (function (a b) a)))
  (check-is-equalp (lambda (x y) (the (cons-of integer string) (cons x y)))
                   (function (integer string) (cons-of integer string)))
  (check-is-equalp (lambda (x y) (cons (1+ x) (string-upcase y)))
                   (function (number (or string symbol character)) (cons-of number simple-string))))

#+nil(deftest type-generalization-tests ()
  (check-is-equalp (lambda (x) x) (all (a) (function (a) a)))
  (check-is-equalp (lambda (x y) x) (all (a b) (function (a b) a))))

(deftest setq-tests ()
  (check-is-equalp
   (let ((l (the (list-of (cons-of symbol string))
                 (list (cons :lala 22))))
         (x (the symbol 'asdf)))
     (setf x (car (nth 0 l))))
   symbol))

(deftest hash-table-tests ()
  (check-is-equalp
   (let ((ht (the (hash-table-of symbol string)
                  (make-hash-table))))
     ht)
   (hash-table-of symbol string))
  (check-is-equalp
   (let ((ht (the (hash-table-of symbol string)
                  (make-hash-table))))
     (gethash 'lala ht))
   (values (or string null) boolean))
  (check-signals-error
   (let ((ht (the (hash-table-of symbol string)
                  (make-hash-table))))
     (gethash 22 ht)))
  (check-signals-error
   (let ((ht (the (hash-table-of symbol string)
                  (make-hash-table))))
     (+ (gethash 'lala ht) 22))))

(defclass my-typed-class ()
  ((x :initarg :x
      :type string)
   (y :initarg :y
      :type integer)))

(deftest make-instance-tests ()
  (signals error
    (pluggable-types/bid::check-make-instance
     (hu.dwim.walker:walk-form '(make-instance 'lala))
     (pluggable-types/bid::make-type-env)))

  (finishes
    (pluggable-types/bid::check-make-instance
     (hu.dwim.walker:walk-form '(make-instance 'simple-error))
     (pluggable-types/bid::make-type-env)))

  (signals error
    (pluggable-types/bid::check-make-instance
     (hu.dwim.walker:walk-form '(make-instance 'simple-error :lala 22))
     (pluggable-types/bid::make-type-env)))

  ;; SIMPLE-ERROR slots are not properly typed unfortunately
  #+nil(pluggable-types/bid::check-make-instance
        (hu.dwim.walker:walk-form '(make-instance 'simple-error :format-control 22)) (pluggable-types/bid::make-type-env))

  ;; Invalid initargs
  (signals error
    (pluggable-types/bid::check-make-instance
     (hu.dwim.walker:walk-form '(make-instance 'my-typed-class :foo 22)) (pluggable-types/bid::make-type-env)))

  ;; Checks initargs type
  (signals error
    (pluggable-types/bid::check-make-instance
     (hu.dwim.walker:walk-form '(make-instance 'my-typed-class :x 22)) (pluggable-types/bid::make-type-env)))

  (signals error
    (pluggable-types/bid::check-make-instance
     (hu.dwim.walker:walk-form '(make-instance 'my-typed-class :x nil))
     (pluggable-types/bid::make-type-env)))

  (finishes
    (pluggable-types/bid::check-make-instance
     (hu.dwim.walker:walk-form '(make-instance 'my-typed-class :x "lala"))
     (pluggable-types/bid::make-type-env)))

  (finishes
    (pluggable-types/bid::check-make-instance
     (hu.dwim.walker:walk-form '(make-instance 'my-typed-class :y 22))
     (pluggable-types/bid::make-type-env))))

(deftest flet-tests ()
  (check-is-equalp
   (flet ((hello (x)
            x))
     (declare (ftype (function (integer) integer) hello))
     (hello 22))
   integer)

  (check-is-equalp
   (flet ((hello (x)
            x))
     (hello 22))
   t)

  (signals type-checking-error
    (check-form '(flet ((hello (x)
                         x))
                  (declare (ftype (function (integer) integer) hello))
                  (hello "lala")))))
