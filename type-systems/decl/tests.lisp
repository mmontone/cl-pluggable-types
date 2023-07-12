(fiasco:define-test-package :pluggable-types/decl/tests
  (:use :cl :stefil :pluggable-types/decl :arrows))

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

;; (push '(ftype* (all (a b) (function ((or (cons-of a b) (list-of a)))
;;                            (or b a)))
;;         car)
;;       *type-declarations*)

;; (push '(ftype* (all (a b) (function ((or (cons-of a b) (list-of a)))
;;                            (or b (list-of a))))
;;         cdr)
;;       *type-declarations*)

(deftest infer-cons-tests ()
  (infer-is-equalp (car (the (cons-of integer string) (cons 2 "lala")))
                   integer)
  (infer-is-equalp (cdr (the (cons-of integer string) (cons 2 "lala")))
                   string)
  (infer-is-equalp (car (the (list-of integer) (cons 2 "lala")))
                   integer)
  (infer-is-equalp (cdr (the (list-of string) (cons 2 "lala")))
                   (list-of string)))

(deftest unify-basic-tests ()
  (is (null
       (pluggable-types/decl::unify-one 'integer 'number))))

(deftest unify-values-tests ()
  (is (null
       (pluggable-types/decl::unify-one '(values integer) 'integer)))
  (signals type-unification-error
    (pluggable-types/decl::unify-one '(values string) 'integer)))
