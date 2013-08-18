;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

#|

TODO provide the new api based on this old code. see lexenv-sbcl.lisp for an example.

(defmethod lexical-variables ((environment c::lexenv))
  (loop
     for var-spec in (c::lexenv-variables environment)
     ;; variable refs are (NAME . LAMBDA-VAR), we want to void
     ;; symbol-macrolets which are (NAME SYSTEM:MACRO . EXPANSION)
     when (and (atom (cdr var-spec))
               ;; don't return ignored vars
               (not (eq (type-of (cdr var-spec)) 'c::global-var))
               (not (c::lambda-var-ignorep (cdr var-spec))))
     collect (car var-spec)))

(defmethod lexical-functions ((environment c::lexenv))
  (loop
     for func-spec in (c::lexenv-functions environment)
     ;; flet and labels function look like ((FLET ACTUAL-NAME) . STUFF)
     if (and (consp (first func-spec))
             (member (car (first func-spec)) '(flet labels)))
       collect (second (first func-spec))
     ;; macrolets look like (NAME SYSTEM:MACRO . STUFF)
     else if (and (consp (cdr func-spec))
                  (eql 'system:macro (second func-spec)))
     ;; except that we don't return macros for now
     do (progn)
     ;; handle the case  (NAME . #<C::FUNCTIONAL>)
     else if (typep (cdr func-spec) 'C::FUNCTIONAL)
       collect (car func-spec)
     ;; if we get here we're confused :(
     else
       do (error "Sorry, don't know how to handle the lexcial function spec ~S."
                 func-spec)))

(defmethod lexical-macros ((environment c::lexenv))
  (loop
   for mac-spec in (c::lexenv-functions environment)
   when (and (consp (cdr mac-spec))
             (eq 'system::macro (cadr mac-spec)))
   collect (cons (car mac-spec) (cddr mac-spec))))

(defmethod lexical-symbol-macros ((environment c::lexenv))
  (loop
   for mac-spec in (c::lexenv-variables environment)
   when (and (consp (cdr mac-spec))
             (eq 'system::macro (cadr mac-spec)))
   collect (cons (car mac-spec) (cddr mac-spec))))

(defmethod augment-with-variable ((env c::lexenv) var)
  (c::make-lexenv :default env
                  :variables (list (cons var (c::make-lambda-var :name var)))))

(defmethod augment-with-function ((env c::lexenv) fun)
  (c::make-lexenv :default env
                  :functions (list (cons fun (lambda () 42)))))

(defmethod augment-with-macro ((env c::lexenv) mac def)
  (c::make-lexenv :default env
                  :functions (list (list* mac 'system::macro def))))

(defmethod augment-with-symbol-macro ((env c::lexenv) symmac def)
  (c::make-lexenv :default env
                  :variables (list (list* symmac 'system::macro def))))

|#
