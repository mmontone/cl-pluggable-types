(defpackage :gradual.test
  (:use :cl :gradual :fiveam)
  (:export #:run-tests))

(in-package :gradual.test)

(defun run-tests ()
  (5am:run 'gradual.test::gradual-tests))

;; (gradual::$defparameter *count* 1 "Count" integer)

;; (gradual::$defparameter *count* 22423 "Count" integer)

;; (gradual::$defparameter *count* 1 "Count" number)

;; (gradual::$defparameter *count3* (make-hash-table) "Count" hash-table)

;; (var-type '*count*)

;; (gradual::$defparameter *count2* 1 "Count")

;; (var-type '*count2*)

;; (check-gradual-type *count* 'integer)
;; (check-gradual-type *count* 'number)

;; (gradual::$setq *count* 22)
;; (gradual::$setq *count* t)

;; (gradual::$defparameter *question* t "" boolean)

;; (gradual::$setq *question* 2)
;; (gradual::$setq *question* t)

(gradual::$defun concatenate-strings ((str1 string) (str2 string))
  (declare (return-type string))
  (concatenate 'string str1 str2))

(infer-type
 (gradual::walk-form
  '(concatenate-strings "s1" "s2")))

(infer-type
 (gradual::walk-form '(lambda (x)
	      (declare (var-type x integer))
	      x)))

(infer-type
 (gradual::walk-form
  '(lambda (x)
    x)))

(infer-type
 (gradual::walk-form
  '(lambda ((x string))
    x)))

(setf *run-test-when-defined* t)

(def-suite gradual-tests :description "Gradual typing tests")

(in-suite gradual-tests)

(def-test typed-lambda-list ()
  (is (equalp
       (multiple-value-list
	(gradual::parse-typed-lambda-list '(a)))
       (list '((A T))
	     NIL
	     NIL
	     NIL
	     NIL
	     NIL
	     NIL)))
  (is (equalp
       (multiple-value-list
	(gradual::parse-typed-lambda-list '((a integer))))
       (list '((A INTEGER))
	     NIL
	     NIL
	     NIL
	     NIL
	     NIL
	     NIL)))
  
  ;; Optionals
  (is (equalp
       (multiple-value-list
	(gradual::parse-typed-lambda-list '((a string) &optional (x 33 integer))))
       (list
	'((A STRING))
	'((X 33 INTEGER))
	NIL
	NIL
	NIL
	NIL
	NIL)))
  (is (equalp
       (multiple-value-list
	(gradual::parse-typed-lambda-list '((a string) &optional (x 33 integer x-supplied-p))))
       (list
	'((A STRING))
	'((X 33 INTEGER X-SUPPLIED-P))
	NIL
	NIL
	NIL
	NIL
	NIL)))
  (is (equalp
       (multiple-value-list
	(gradual::parse-typed-lambda-list '((a string) &optional x)))
       (list
	'((A STRING))
	'((X NIL T))
	NIL
	NIL
	NIL
	NIL
	NIL)))

  ;; Keyword
  (is (equalp
       (multiple-value-list
	(gradual::parse-typed-lambda-list '((a string) &key (x 33 integer))))
       (list
	'((A STRING))
	NIL
	NIL
	'(((:X X) 33 INTEGER))
	NIL
	NIL
	T)))
  (is (equalp
       (multiple-value-list
	(gradual::parse-typed-lambda-list '((a string) &key (x 33 integer x-supplied-p))))
       (list
	'((A STRING))
	NIL
	NIL
	'(((:X X) 33 INTEGER X-SUPPLIED-P))
	NIL
	NIL
	T)))
  (is (equalp
       (multiple-value-list
	(gradual::parse-typed-lambda-list '((a string) &key x)))
       (list
	'((A STRING))
	NIL
	NIL
	'(((:X X) NIL T))
	NIL
	NIL
	T)))
  (is (equalp
       (multiple-value-list
	(gradual::parse-typed-lambda-list '((a string) &key ((:x var) 22 integer))))
       (list
	'((A STRING))
	NIL
	NIL
	'(((:X VAR) 22 INTEGER))
	NIL
	NIL
	T)))

  ;; Rest
  #+nil(is (equalp
       (multiple-value-list
	(gradual::parse-typed-lambda-list '((a string) &rest (nums integer))))
       (list
	'((A STRING))
	NIL
	NIL
	'(((:X VAR) 22 INTEGER))
	NIL
	NIL
	T)))
  
  )

(def-test types-lambda-list ()
  (is (equalp
       (multiple-value-list 
	(gradual::parse-types-lambda-list '(integer integer)))
       '((INTEGER INTEGER)
	 NIL
	 NIL
	 NIL
	 NIL
	 NIL
	 NIL)))
  (is (equalp
       (multiple-value-list 
	(gradual::parse-types-lambda-list '(integer &optional integer)))
       '((INTEGER)
	 (INTEGER)
	 NIL
	 NIL
	 NIL
	 NIL
	 NIL)))
  (is (equalp
       (multiple-value-list 
	(gradual::parse-types-lambda-list '(integer &key (x integer))))
       '((INTEGER)
	 NIL
	 NIL
	 ((X . INTEGER))
	 NIL
	 NIL
	 T)))
  (signals error
    (gradual::parse-types-lambda-list '(integer &optional (x integer))))
  (signals error
    (gradual::parse-types-lambda-list '(integer &key (x 22))))
  (signals error
    (gradual::parse-types-lambda-list '(integer &key (x integer 20))))
  (signals error
    (gradual::parse-types-lambda-list '(integer &key integer)))
  (signals error
    (gradual::parse-types-lambda-list '(2 2))))
