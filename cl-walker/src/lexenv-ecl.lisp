;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

;;;
;;; ECL
;;;

(defun make-empty-lexical-environment ()
  (c::cmp-env-new))

;;;
;;; utilities
;;;

(defun ecl-variable-spec-p (spec)
  (and (listp spec)
       (member (second spec) '(:special t))))

(defun ecl-symbol-macro-spec-p (spec)
  (and (listp spec)
       (eql (second spec) 'si::symbol-macro)))

(defun ecl-macro-spec-p (spec)
  (and (listp spec)
       (eql (second spec) 'si::macro)))

(defun ecl-function-spec-p (spec)
  (and (listp spec)
       (eql (second spec) 'c::function)))

(defun ecl-block-spec-p (spec)
  (and (listp spec)
       (eql (first spec) :block)))

(defun ecl-tag-spec-p (spec)
  (and (listp spec)
       (eql (first spec) :tag)))

(defun ecl-special-var-p (spec)
  (eql (second spec) :special))

(defun ecl-ignored-var-p (spec)
  (< (c::var-ref (fourth spec)) 0))

;;;
;;; iteration
;;;

(defun iterate-variables-in-lexenv (visitor lexenv
                                    &key include-ignored? include-specials?)
  (dolist (spec (c::cmp-env-variables lexenv))
    (when (ecl-variable-spec-p spec)
      (let* ((name     (first spec))
             (special? (ecl-special-var-p spec))
             (ignored? (ecl-ignored-var-p spec)))
        (when (and (or (not special?)
                       include-specials?)
                   (or (not ignored?)
                       include-ignored?))
          (funcall visitor name :ignored? ignored? :special? special?))))))

(defun iterate-symbol-macros-in-lexenv (visitor lexenv)
  (dolist (spec (c::cmp-env-variables lexenv))
    (when (ecl-symbol-macro-spec-p spec)
      (funcall visitor (first spec) (funcall (third spec) nil nil)))))

(defun iterate-functions-in-lexenv (visitor lexenv)
  (dolist (spec (c::cmp-env-functions lexenv))
    (when (ecl-function-spec-p spec)
      (funcall visitor (first spec)))))

(defun iterate-macros-in-lexenv (visitor lexenv)
  (dolist (spec (c::cmp-env-functions lexenv))
    (when (ecl-macro-spec-p spec)
      (assert (functionp (third spec)))
      (funcall visitor (first spec) (third spec)))))

(defun iterate-blocks-in-lexenv (visitor lexenv)
  (dolist (spec (c::cmp-env-variables lexenv))
    (when (ecl-block-spec-p spec)
      (funcall visitor (second spec)))))

(defun iterate-tags-in-lexenv (visitor lexenv)
  (dolist (spec (c::cmp-env-variables lexenv))
    (when (ecl-tag-spec-p spec)
      (dolist (name (second spec))
        (funcall visitor name)))))

;;;
;;; augmentation
;;;

(defun augment-lexenv-with-variable (name lexenv &key special ignored)
  (let* ((env (c::cmp-env-copy lexenv))
         (var (if special
                  (c::c1make-global-variable name :warn nil :kind 'c::special)
                  (c::%make-var :name name :kind 'c::lexical
                                :ref (if ignored -1 0)))))
    ;; cmp-env-register-var has a bug and would
    ;; update the wrong environment
    (push (list (c::var-name var)
                (if special :special t)
                t var)
          (c::cmp-env-variables env))
    env))

(defun augment-lexenv-with-function (name lexenv)
  (let* ((env (c::cmp-env-copy lexenv))
         (fun (c::make-fun :name name)))
    (c::cmp-env-register-function fun env)
    env))

(defun augment-lexenv-with-macro (name def lexenv)
  (let* ((env  (c::cmp-env-copy lexenv)))
    (c::cmp-env-register-macro name def env)
    env))

(defun augment-lexenv-with-symbol-macro (name def lexenv)
  (let* ((env  (c::cmp-env-copy lexenv)))
    (c::cmp-env-register-symbol-macro name def env)
    env))

(defun augment-lexenv-with-block (name lexenv)
  (let* ((env  (c::cmp-env-copy lexenv)))
    (c::cmp-env-register-block (c::make-blk :name name))
    env))

(defun augment-lexenv-with-tag (name lexenv)
  (let* ((env  (c::cmp-env-copy lexenv)))
    (c::cmp-env-register-tag (c::make-tag :name name))
    env))

