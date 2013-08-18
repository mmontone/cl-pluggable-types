;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

;;;
;;; SBCL
;;;
(defun make-empty-lexical-environment ()
  (sb-kernel:make-null-lexenv))

;;;
;;; iteration
;;;
(defun iterate-variables-in-lexenv (visitor lexenv &key include-ignored? include-specials?)
  (loop
     :for entry :in (sb-c::lexenv-vars lexenv)
     :for name = (first entry)
     :for definition = (rest entry)
     :for ignored? = (and (typep definition 'sb-c::lambda-var)
                          (sb-c::lambda-var-ignorep definition))
     :for special? = (typep definition 'sb-c::global-var)
     :unless (and (consp definition)
                  (eq 'sb-sys::macro (first definition)))
     :do (when (and (or (not ignored?)
                        include-ignored?)
                    (or (not special?)
                        include-specials?))
           (funcall visitor name :ignored? ignored? :special? special?))))

(defun iterate-functions-in-lexenv (visitor lexenv)
  (loop
     :for entry :in (sb-c::lexenv-funs lexenv)
     :for name = (first entry)
     :for definition = (rest entry)
     :unless (and (consp definition)
                  (eq 'sb-sys::macro (first definition)))
     :do (funcall visitor name)))

(defun iterate-macros-in-lexenv (visitor lexenv)
  (loop
     :for entry :in (sb-c::lexenv-funs lexenv)
     :for name = (first entry)
     :for definition = (rest entry)
     :for macro? = (and (consp definition)
                        (eq 'sb-sys::macro (first definition)))
     :for macro-function = (when macro?
                             (rest definition))
     :when macro?
     :do (progn
           (assert (functionp macro-function))
           (funcall visitor name macro-function))))

(defun iterate-symbol-macros-in-lexenv (visitor lexenv)
  (loop
     :for entry :in (sb-c::lexenv-vars lexenv)
     :for name = (first entry)
     :for definition = (rest entry)
     :for macro? = (and (consp definition)
                        (eq 'sb-sys::macro (first definition)))
     :for macro-body = (when macro?
                         (rest definition))
     :when macro?
     :do (funcall visitor name macro-body)))

(defun iterate-blocks-in-lexenv (visitor lexenv)
  (loop
     :for entry :in (sb-c::lexenv-blocks lexenv)
     :for name = (first entry)
     :do (funcall visitor name)))

(defun iterate-tags-in-lexenv (visitor lexenv)
  (loop
     :for entry :in (sb-c::lexenv-tags lexenv)
     :for name = (first entry)
     :do (funcall visitor name)))

;;;
;;; augmentation
;;;
(defun augment-lexenv-with-variable (name lexenv &key special ignored)
  (let ((var (if special
                 (sb-c::make-global-var :%source-name name)
                 (sb-c::make-lambda-var :%source-name name))))
    (when ignored
      (setf (sb-c::lambda-var-ignorep var) t))
    (sb-c::make-lexenv :default lexenv
                       :vars (list (cons name var)))))

(defun augment-lexenv-with-function (name lexenv)
  (sb-c::make-lexenv :default lexenv :funs (list (cons name t))))

(defun augment-lexenv-with-macro (name def lexenv)
  (sb-c::make-lexenv :default lexenv :funs (list (list* name 'sb-sys::macro def))))

(defun augment-lexenv-with-symbol-macro (name def lexenv)
  (sb-c::make-lexenv :default lexenv :vars (list (list* name 'sb-sys::macro def))))

(defun augment-lexenv-with-block (name lexenv)
  (sb-c::make-lexenv :default lexenv :blocks (list (list name))))

(defun augment-lexenv-with-tag (name lexenv)
  (sb-c::make-lexenv :default lexenv :tags (list (list name))))
