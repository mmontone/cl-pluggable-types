(fiasco:define-test-package :pluggable-types/decl/tests
  (:use :cl :fiasco :pluggable-types/decl :arrows))

(in-package :pluggable-types/decl/tests)

(defmacro infer-is-equalp (form type)
  `(is (equalp (infer-form ',form) ',type)))

(defmacro infer-is-subtypep (form type)
  `(is (subtypep (infer-form ',form) ',type)))

(defmacro infer-signals-error (form)
  `(signals type-unification-error (infer-form ',form)))

(deftest infer-constant-tests ()
  (infer-is-subtypep 22 number)
  (infer-is-equalp '(1 2 3) (list-of number)))

(deftest function-application-tests ()
  (infer-signals-error (+ 22 "lala"))
  (infer-is-subtypep (+ 22 40) number))
  
(deftest infer-let-tests ()
  (infer-is-subtypep (let ((x "lla")) x) string)
  (-> (let ((x 34)
           (y 56))
       (+ x (- y x)))
     (infer-is-subtypep number)))

(deftest infer-parametric-types-test ()
  ;; Evaluates to integer! : uses the (all (a) (function (a) a)) type !! :-)
  (infer-is-subtypep (identity 22) number))

(deftest infer-the-form-test ()
  (is (equalp (infer-form '(the (list-of integer) '(1 2 3)))
              '(list-of integer)))
  (signals type-unification-error (infer-form '(the (list-of integer) 22)))
  (is (equalp (infer-form '(the (list-of integer) '("asdf")))
              '(list-of integer))))

(deftest infer-parametric-types-test-2 ()
  (infer-is-equalp (mapcar #'+ (the (list-of number) '(1 2 3)))
                   (list-of number))
  (infer-is-equalp (mapcar #'+ '(1 2 3))
                   (list-of number))
  (infer-signals-error
    (mapcar #'+ (the (list-of string) '("lala" 2 3))))
  (infer-is-equalp
   (mapcar #'identity (the (list-of string) '("lala" 2 3)))
   (list-of string))
  (infer-is-equalp (mapcar #'identity (the (list-of string) '("lala" 2 3)))
                   (list-of string))
  (infer-is-equalp
       (mapcar #'print (the (list-of string) '("lala")))
       (list-of t))

  ;; (infer-form '(mapcar #'identity (the list '("lala"))))

  ;; (infer-form '(mapcar #'identity '("lala")))

  ;; (infer-form '(mapcar #'identity (the (list-of t) '("lala"))))
  
  (infer-is-subtypep (nth 10 (the (list-of string) '("foo" "bar")))
                     string)
  (infer-signals-error (nth "lala" (the (list-of string) '("foo" "bar"))))

  (infer-is-subtypep
   (let ((list (mapcar #'identity (the (list-of string) '("lala")))))
     (nth 1 list))
   string)

  (infer-is-subtypep
   (let ((list (mapcar #'identity (the (list-of string) '("lala")))))
     (first list))
   string)

  (infer-is-subtypep
   (let ((list (mapcar #'identity (the (list-of string) '("lala")))))
     (rest list))
   (list-of string)))

(deftest infer-cons-tests ()
  (infer-is-equalp (car (the (cons-of integer string) (cons 2 "lala")))
                   integer)
  (infer-is-equalp (cdr (the (cons-of integer string) (cons 2 "lala")))
                   string)) 

(deftest unify-basic-tests ()
  (is (null
       (pluggable-types/decl::unify-one 'number 'integer))))

(deftest unify-values-tests ()
  (is (null
       (pluggable-types/decl::unify-one '(values integer) 'integer)))
  (signals type-unification-error
    (pluggable-types/decl::unify-one '(values string) 'integer)))

(deftest lambda-tests ()
  (infer-is-equalp (lambda (x) x) (all (a) (function (a) a)))
  (infer-is-equalp (lambda (x y) x) (all (a b) (function (a b) a)))
  (infer-is-equalp (lambda (x y) (the (cons-of integer string) (cons x y)))
                   (function (integer string) (cons-of integer string)))
  (infer-is-equalp (lambda (x y) (cons (1+ x) (string-upcase y)))
                   (function (number (or string symbol character)) (cons-of number simple-string))))

(deftest type-generalization-tests ()
  (infer-is-equalp (lambda (x) x) (all (a) (function (a) a)))
  (infer-is-equalp (lambda (x y) x) (all (a b) (function (a b) a))))

(deftest setq-tests ()
  (infer-is-equalp
   (let ((l (the (list-of (cons-of symbol string))
                 (list (cons :lala 22))))
         (x (the symbol 'asdf)))
     (setf x (car (nth 0 l))))
   symbol))

(deftest hash-table-tests ()
  (infer-is-equalp
   (let ((ht (the (hash-table-of symbol string)
                  (make-hash-table))))
     ht)
   (hash-table-of symbol string))
  (infer-is-equalp
   (let ((ht (the (hash-table-of symbol string)
                  (make-hash-table))))
     (gethash 'lala ht))
   (values (or string null) boolean))
  (signals type-unification-error
    (infer-form '(let ((ht (the (hash-table-of symbol string)
                            (make-hash-table))))
                  (gethash 22 ht))))
  (signals type-unification-error
    (infer-form '(let ((ht (the (hash-table-of symbol string)
                            (make-hash-table))))
                  (+ (gethash 'lala ht) 22)))))
