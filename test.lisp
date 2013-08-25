(defpackage :gradual.test
  (:use :cl :gradual :fiveam))

(in-package :gradual.test)

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
	NIL))))

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
  (signals error
    (gradual::parse-types-lambda-list '(2 2))))
