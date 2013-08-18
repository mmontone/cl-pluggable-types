;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

#|

TODO provide the new api based on this old code. see lexenv-sbcl.lisp for an example.

#+(and allegro (version>= 7 0))
(progn

(defmethod lexical-variables ((env sys::augmentable-environment))
  (let (fns)
    (system::map-over-environment-variables
     (lambda (symbol type rest)
       (declare (ignore rest))
       (when (and (eq type :lexical)
                  (sys:variable-information symbol env))
         (push symbol fns)))
     env)
    fns))

(defmethod lexical-functions ((env sys::augmentable-environment))
  (let (fns)
    (system::map-over-environment-functions
     (lambda (name type rest)
       (when (and (eq type :function)
                  (sys:function-information name env))
         (push name fns)))
     env)
    fns))

(defmethod lexical-macros ((env sys::augmentable-environment))
  (let (fns)
    (system::map-over-environment-functions
     (lambda (name type rest)
       (when (eq type :macro)
         (push (cons name (car rest)) fns)))
     env)
    fns))

(defmethod lexical-symbol-macros ((env sys::augmentable-environment))
  (let (fns)
    (system::map-over-environment-variables
     (lambda (symbol type rest)
       (when (eq type :symbol-macro)
         (push (cons symbol (car rest)) fns)))
     env)
    fns))

(defmethod augment-with-variable ((env sys::augmentable-environment) var)
  (system:augment-environment env :variable (list var)))

(defmethod augment-with-function ((env sys::augmentable-environment) fun)
  (system:augment-environment env :function (list fun)))

(defmethod augment-with-macro ((env sys::augmentable-environment) mac def)
  (system:augment-environment env :macro (list (list mac def))))

(defmethod augment-with-symbol-macro ((env sys::augmentable-environment) symmac def)
  (system:augment-environment env :symbol-macro (list (list symmac def))))

) ; #+(and allegro (version>= 7 0))

|#
