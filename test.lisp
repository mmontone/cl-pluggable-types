(defpackage :gradual.test
  (:use :cl :gradual :mw-equiv :fiveam)
  (:export #:run-tests))

(in-package :gradual.test)

(defun run-tests ()
  (5am:run 'gradual.test::gradual-tests))

;; (gradual::typed-defparameter *count* 1 "Count" integer)

;; (gradual::typed-defparameter *count* 22423 "Count" integer)

;; (gradual::typed-defparameter *count* 1 "Count" number)

;; (gradual::typed-defparameter *count3* (make-hash-table) "Count" hash-table)

;; (var-type '*count*)

;; (gradual::typed-defparameter *count2* 1 "Count")

;; (var-type '*count2*)

;; (check-gradual-type *count* 'integer)
;; (check-gradual-type *count* 'number)

;; (gradual::$setq *count* 22)
;; (gradual::$setq *count* t)

;; (gradual::typed-defparameter *question* t "" boolean)

;; (gradual::$setq *question* 2)
;; (gradual::$setq *question* t)

(gradual::typed-defun concatenate-strings ((str1 string) (str2 string))
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
  (is (object=
       (multiple-value-list
	(gradual::parse-typed-lambda-list '(a)))
       (list '((A T))
	     NIL
	     NIL
	     NIL
	     NIL
	     NIL
	     NIL)))
  (is (object=
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
  (is (object=
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
  (is (object=
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
  (is (object=
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
  (is (object=
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
  (is (object=
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
  (is (object=
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
  (is (object=
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
  #+nil(is (object=
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
  (is (object=
       (multiple-value-list 
	(gradual::parse-types-lambda-list '(integer integer)))
       '((INTEGER INTEGER)
	 NIL
	 NIL
	 NIL
	 NIL
	 NIL
	 NIL)))
  (is (object=
       (multiple-value-list 
	(gradual::parse-types-lambda-list '(integer &optional integer)))
       '((INTEGER)
	 (INTEGER)
	 NIL
	 NIL
	 NIL
	 NIL
	 NIL)))
  (is (object=
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

(def-test typed-defun ()
  (gradual::typed-defun hello ((x string))
    (declare (return-type string))
    x)
  (is (object=
       (fun-type 'hello)
       (fun (string) string)))

  (gradual::typed-defun hello ((x integer) &optional (y "ok" string))
    (declare (return-type integer))
    x)
  (is (object=
       (fun-type 'hello)
       (fun (integer &optional string) integer)))

  (gradual::typed-defun hello ((x string) &key (y nil boolean))
    (declare (return-type integer))
    22)
  (is (object= (fun-type 'hello)
	      (fun (string &key (y boolean)) integer))))

;; typechecking tests

(def-test free-application-typechecking ()
  (macrolet ((is-typed (form type)
	       `(is (object=
		     (gradual::%typecheck-form
		      (gradual::walk-form ',form)
		      (gradual::make-typing-environment))
		     ',type)))
	     (signals-type-error (form)
	       `(signals gradual-type-error
		  (gradual::%typecheck-form
		   (gradual::walk-form ',form)
		   (gradual::make-typing-environment)))))
    ;; required args types
    (gradual::typed-defun f1 ((x string))
      (declare (return-type string))
      x)
    (is-typed (f1 "asdf") string)
    (signals-type-error (f1 22))

    ;; optional args types
    (gradual::typed-defun f2 ((x integer) &optional (y nil string))
      (declare (return-type integer))
      22)
    (is-typed (f2 22) integer)
    (is-typed (f2 22 "asdf") integer)
    (signals-type-error (f2 22 23))
    (signals-type-error (f2 "asfd" "sdf"))

    ;; keyword args types
    (gradual::typed-defun f3 ((x integer) &key (y nil string)
			      (z t boolean))
      (declare (return-type boolean))
      (and (> x 3)
	   (or (null y)
	       (> (length y) 3))
	   z))
    (is-typed (f3 22) boolean)
    (is-typed (f3 22 :y "hello") boolean)
    (is-typed (f3 22 :z t :y "bye") boolean)
    (is-typed (f3 22 :z nil) boolean)
    (signals-type-error (f3 22 :z 22))

    ;; rest args types
    (gradual::typed-defun f4 ((x string) &rest (rest integer))
      (declare (return-type string))
      (concatenate 'string x (prin1-to-string (apply #'+ rest))))
    (is-typed (f4 "hello") string)
    (is-typed (f4 "hello" 22) string)
    (is-typed (f4 "hello" 34 54 545) string)
    (signals-type-error (f4 22))
    (signals-type-error (f4 "hello" "lala"))
    (signals-type-error (f4 "hello" 34 4544 "foo" 34))))

(def-test type-casting-test ()
  (gradual::typed-defun f5 ((x string) (y string))
    (declare (return-type string))
    (concatenate 'string x y))
  ;; This signals an error because the casting from sequence to string is not being done
  (signals gradual-type-error
    (gradual::%typecheck-form (gradual::fun-source 'f5)
			      (gradual::make-typing-environment)))

  (gradual::typed-defun f6 ((x string) (y string))
    (declare (return-type string))
    ((string) concatenate 'string x y))
  (is (object= (gradual::%typecheck-form (gradual::fun-source 'f6)
					(gradual::make-typing-environment))
	      (fun (string string) string))))

(def-test parse-type-test ()
  (let ((type (gradual::parse-type '<a>)))
    (is (and (typep type 'gradual::type-var)
	     (object= (gradual::name type)
		     'a))))
  (is (object= (gradual::parse-type 'string) 'string))
  (gradual::parse-type '(function ((function (<a>) <b>) (list <a>)) (list <b>))))
		     
(def-test extract-type-declarations-test ()
  (is (equalp
       (gradual::extract-type-declarations '((declare (ignore x))
					     (declare (var-type x integer))
					     (declare (fun-type f (integer -> integer)))
					     (declare (return-type integer))))
       '(((DECLARE (FUN-TYPE F (INTEGER -> INTEGER))))
	 ((DECLARE (VAR-TYPE X INTEGER)))
	 (DECLARE (RETURN-TYPE INTEGER))
	 ((DECLARE (IGNORE X)))))))
	 
