;; Provides compiler info (specially from declarations) in a portable way

(defpackage :compiler-info
  (:use :cl)
  (:export #:function-info
           #:variable-info
           #:function-type
           #:variable-type))

(in-package :compiler-info)

(declaim (ftype (function (symbol) t) function-type variable-type))

#+sbcl
(defun function-type (fname)
  (sb-introspect:function-type fname))

#+sbcl
(defun variable-type (varname)
  (cdr (assoc 'type (third (multiple-value-list (sb-cltl2:variable-information varname))))))

#+sbcl
(defun function-info (fname)
  (sb-cltl2:function-information fname))

#+sbcl
(defun variable-info (varname)
  (sb-cltl2:variable-information varname))

#+ccl
(defun variable-info (varname)
  (ccl::variable-information varname))

#+ccl
(defun function-info (fname))

#+ccl
(defun function-type (fname)
  (ccl::find-ftype-decl fname))
