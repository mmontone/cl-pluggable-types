;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

#|

TODO provide the new api based on this old code. see lexenv-sbcl.lisp for an example.

#+(and lispworks macosx)
(progn

(defmethod lexical-variables ((environment system::augmented-environment))
  (mapcar (lambda (venv)
            (slot-value venv 'compiler::name))
          (remove-if (lambda (venv)
                       ;; regular variables, the ones we're interested
                       ;; in, appear to have a NIL in this slot.
                       (slot-value venv 'compiler::kind))
                     (slot-value environment 'compiler::venv))))

(defmethod lexical-functions ((environment system::augmented-environment))
  (mapcar #'car
          (remove-if (lambda (fenv)
                       ;; remove all the macros
                       (eql 'compiler::macro (slot-value (cdr fenv) 'compiler::function-or-macro)))
                     (slot-value environment 'compiler::fenv))))

(defmethod lexical-variables ((environment compiler::environment))
  (mapcar (lambda (venv)
            (slot-value venv 'compiler::name))
          (remove-if (lambda (venv)
                       ;; regular variables, the ones we're interested
                       ;; in, appear to have a NIL in this slot.
                       (slot-value venv 'compiler::kind))
                     (slot-value environment 'compiler::venv))))

(defmethod lexical-functions ((environment compiler::environment))
  (mapcar #'car
          (remove-if (lambda (fenv)
                       ;; remove all the macros
                       (macro-function (car fenv) environment))
                     (slot-value environment 'compiler::fenv))))

) ; #+(and lispworks macosx)

#+(and lispworks (or win32 linux))
(progn

(defun lexical-runtime-p (value)
  (and (symbolp value)
       (eq (symbol-package value) nil)))

(defmethod lexical-variables ((environment lexical::environment))
  (loop for candidate in (slot-value environment 'lexical::variables)
        if (lexical-runtime-p (cdr candidate))
        collect (car candidate)))

(defmethod lexical-functions ((environment lexical::environment))
  (loop for candidate in (slot-value environment 'lexical::functions)
        if (lexical-runtime-p (cdr candidate))
        collect (car candidate)))


(defmethod lexical-symbol-macros ((environment lexical::environment))
  (loop for candidate in (slot-value environment 'lexical::variables)
        unless (lexical-runtime-p (cdr candidate))
        collect candidate))

(defmethod lexical-macros ((environment lexical::environment))
  (loop for candidate in (slot-value environment 'lexical::functions)
        unless (lexical-runtime-p (cdr candidate))
        collect candidate))

(defmethod augment-with-variable ((env lexical::environment) var)
  (harlequin-common-lisp:augment-environment
   env :variable (list var)))

(defmethod augment-with-function ((env lexical::environment) fun)
  (harlequin-common-lisp:augment-environment
   env :function (list fun)))

(defmethod augment-with-macro ((env lexical::environment) mac def)
  (harlequin-common-lisp:augment-environment
   env :macro (list (list mac def))))

(defmethod augment-with-symbol-macro ((env lexical::environment) symmac def)
  (harlequin-common-lisp:augment-environment
   env :symbol-macro (list (list symmac def))))

) ; #+(and lispworks (or win32 linux))

|#
