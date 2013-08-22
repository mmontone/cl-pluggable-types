(in-package :gradual)

($defparameter *count* 1 "Count" integer)

($defparameter *count* 22423 "Count" integer)

($defparameter *count* 1 "Count" number)

($defparameter *count3* (make-hash-table) "Count" hash-table)

(var-type '*count*)

($defparameter *count2* 1 "Count")

(var-type '*count2*)

(check-gradual-type *count* 'integer)
(check-gradual-type *count* 'number)

($setq *count* 22)
($setq *count* t)

($defparameter *question* t "" boolean)

($setq *question* 2)
($setq *question* t)

($defun concatenate-strings ((str1 string) (str2 string))
  (declare (return-type string))
  (concatenate 'string str1 str2))

(infer-type (walk-form '(concatenate-strings "s1" "s2")))

(infer-type (walk-form '(lambda (x)
			 (declare (var-type x integer))
			 x)))

(infer-type (walk-form '(lambda (x)
			 x)))

(infer-type (walk-form '(lambda ((x string))
			 x)))
