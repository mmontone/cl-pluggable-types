;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

(defparameter *lexical-environment-functions*
  '((make-empty-lexical-environment  "Returns an empty lexical environment useful for testing and playing around in the repl.")
    (iterate-variables-in-lexenv     "(funcall VISITOR name &key ignored? special?) for each variable definition in LEXENV.")
    (iterate-functions-in-lexenv     "(funcall VISITOR name) for each function definition in LEXENV.")
    (iterate-macros-in-lexenv        "(funcall VISITOR name macro-function) for each macro definition in LEXENV.")
    (iterate-symbol-macros-in-lexenv "(funcall VISITOR name macro-function) for each symbol macro definition in LEXENV.")
    (iterate-blocks-in-lexenv        "(funcall VISITOR name) for each block in LEXENV.")
    (iterate-tags-in-lexenv          "(funcall VISITOR name) for each tag in LEXENV.")
    (augment-lexenv-with-variable)
    (augment-lexenv-with-function)
    (augment-lexenv-with-macro)
    (augment-lexenv-with-symbol-macro)
    (augment-lexenv-with-block)
    (augment-lexenv-with-tag)
    ))

;;; set up some docstrings
(loop
   :for (name documentation) :in *lexical-environment-functions*
   :do (when documentation
         (setf (documentation name 'function) documentation)))

(defun unimplemented-lexical-environment-function ()
  (cerror "ignore and try to continue" "This is not implemented for your lisp, sorry. You may try to continue, but...")
  nil)

;;; if there was no definition provided for some of the functions in
;;; *lexical-environment-functions* then register a function that will
;;; signal an error.
(eval-when (:load-toplevel :execute)
  (loop
     :for (name) :in *lexical-environment-functions*
     :do (unless (fboundp name)
           (setf (fdefinition name)
                 (lambda (&rest args)
                   (declare (ignore args))
                   (unimplemented-lexical-environment-function))))))

(defun lookup-in-lexenv (kind name lexenv &key (otherwise :error))
  (let ((result
         (multiple-value-list
          (ecase kind
            (:variable     (find-variable-in-lexenv name lexenv))
            (:function     (find-function-in-lexenv name lexenv))
            (:macro        (find-macro-in-lexenv name lexenv))
            (:symbol-macro (find-symbol-macro-in-lexenv name lexenv))
            (:block        (find-block-in-lexenv name lexenv))
            (:tag          (find-tag-in-lexenv name lexenv))))))
    (if (first result)
        (values-list result)
        (cond
          ((eq otherwise :error) (error "Could not find ~S of kind ~S in lexenv" name kind))
          ((eq otherwise :warn)  (warn  "Could not find ~S of kind ~S in lexenv" name kind))
          (t (if (functionp otherwise)
                 (funcall otherwise)
                 otherwise))))))

(defun augment-lexenv (kind name lexenv &rest args)
  (ecase kind
    (:variable     (progn
                     (assert (null args))
                     (augment-lexenv-with-variable name lexenv)))
    (:function     (progn
                     (assert (null args))
                     (augment-lexenv-with-function name lexenv)))
    (:macro        (destructuring-bind (macro-definition) args
                     (augment-lexenv-with-macro name macro-definition lexenv)))
    (:symbol-macro (destructuring-bind (macro-definition) args
                     (augment-lexenv-with-symbol-macro name macro-definition lexenv)))
    (:block        (progn
                     (assert (null args))
                     (augment-lexenv-with-block name lexenv)))
    (:tag          (progn
                     (assert (null args))
                     (augment-lexenv-with-tag name lexenv)))))

(defmacro augment-lexenv! (kind name env &rest other-datum)
  `(setf ,env (augment-lexenv ,kind ,name ,env ,@other-datum)))

;;;
;;; variables
;;;
(defmacro do-variables-in-lexenv ((lexenv name &optional
                                          (ignored? (gensym) ignored-provided?)
                                          (special? (gensym) special-provided?))
                                  &body body)
  `(iterate-variables-in-lexenv
    (lambda (,name &key ((:ignored? ,ignored?) nil) ((:special? ,special?) nil))
      (declare (ignorable ,@(unless ignored-provided? (list ignored?))
                          ,@(unless special-provided? (list special?))))
      ,@body)
    ,lexenv
    :include-ignored? ,ignored-provided?
    :include-specials? ,special-provided?))

(defun collect-variables-in-lexenv (lexenv &key include-ignored? include-specials? filter)
  (let ((result (list)))
    (iterate-variables-in-lexenv
     (lambda (name &key ignored? special? &allow-other-keys)
       (when (or (not filter)
                 (funcall filter name :ignored? ignored? :special? special?))
         (push name result)))
     lexenv
     :include-ignored? include-ignored?
     :include-specials? include-specials?)
    (nreverse result)))

(defun find-variable-in-lexenv (name-to-find lexenv &key include-ignored? include-specials?)
  (iterate-variables-in-lexenv
   (lambda (name &key ignored? &allow-other-keys)
     (when (eq name name-to-find)
       (return-from find-variable-in-lexenv (values name ignored?))))
   lexenv
   :include-ignored? include-ignored?
   :include-specials? include-specials?)
  (values nil))

;;;
;;; functions
;;;
(defmacro do-functions-in-lexenv ((lexenv name) &body body)
  `(iterate-functions-in-lexenv
    (lambda (,name)
      ,@body)
    ,lexenv))

(defun collect-functions-in-lexenv (lexenv &key filter)
  (let ((result (list)))
    (iterate-functions-in-lexenv
     (lambda (name)
       (when (or (not filter)
                 (funcall filter name))
         (push name result)))
     lexenv)
    (nreverse result)))

(defun find-function-in-lexenv (name-to-find lexenv)
  (iterate-functions-in-lexenv
   (lambda (name)
     (when (eq name name-to-find)
       (return-from find-function-in-lexenv (values name))))
   lexenv)
  (values nil))

;;;
;;; macros
;;;
(defmacro do-macros-in-lexenv ((lexenv name &optional (macro-fn (gensym) macro-fn-provided?))
                               &body body)
  `(iterate-macros-in-lexenv
    (lambda (,name ,macro-fn)
      ,@(unless macro-fn-provided?
        `((declare (ignore ,macro-fn))))
      ,@body)
    ,lexenv))

(defun collect-macros-in-lexenv (lexenv &key filter)
  (let ((result (list)))
    (iterate-macros-in-lexenv
     (lambda (name macro-function)
       (declare (ignore macro-function))
       (when (or (not filter)
                 (funcall filter name))
         (push name result)))
     lexenv)
    (nreverse result)))

(defun find-macro-in-lexenv (name-to-find lexenv)
  (iterate-macros-in-lexenv
   (lambda (name macro-function)
     (when (eq name name-to-find)
       (return-from find-macro-in-lexenv (values name macro-function))))
   lexenv)
  (values nil))

;;;
;;; symbol-macros
;;;
(defmacro do-symbol-macros-in-lexenv ((lexenv name &optional (definition (gensym) definition-provided?))
                                      &body body)
  `(iterate-symbol-macros-in-lexenv
    (lambda (,name ,definition)
      ,@(unless definition-provided?
        `((declare (ignore ,definition))))
      ,@body)
    ,lexenv))

(defun collect-symbol-macros-in-lexenv (lexenv &key filter)
  (let ((result (list)))
    (iterate-symbol-macros-in-lexenv
     (lambda (name macro-body)
       (declare (ignore macro-body))
       (when (or (not filter)
                 (funcall filter name))
         (push name result)))
     lexenv)
    (nreverse result)))

(defun find-symbol-macro-in-lexenv (name-to-find lexenv)
  (iterate-symbol-macros-in-lexenv
   (lambda (name macro-body)
     (when (eq name name-to-find)
       (return-from find-symbol-macro-in-lexenv (values name macro-body))))
   lexenv)
  (values nil))

;;;
;;; blocks
;;;
(defmacro do-blocks-in-lexenv ((lexenv name) &body body)
  `(iterate-blocks-in-lexenv
    (lambda (,name)
      ,@body)
    ,lexenv))

(defun collect-blocks-in-lexenv (lexenv &key filter)
  (let ((result (list)))
    (iterate-blocks-in-lexenv
     (lambda (name)
       (when (or (not filter)
                 (funcall filter name))
         (push name result)))
     lexenv)
    (nreverse result)))

(defun find-block-in-lexenv (name-to-find lexenv)
  (iterate-blocks-in-lexenv
   (lambda (name)
     (when (eq name name-to-find)
       (return-from find-block-in-lexenv (values name))))
   lexenv)
  (values nil))

;;;
;;; tags
;;;
(defmacro do-tags-in-lexenv ((lexenv name) &body body)
  `(iterate-tags-in-lexenv
    (lambda (,name)
      ,@body)
    ,lexenv))

(defun collect-tags-in-lexenv (lexenv &key filter)
  (let ((result (list)))
    (iterate-tags-in-lexenv
     (lambda (name)
       (when (or (not filter)
                 (funcall filter name))
         (push name result)))
     lexenv)
    (nreverse result)))

(defun find-tag-in-lexenv (name-to-find lexenv)
  (iterate-tags-in-lexenv
   (lambda (name)
     (when (eq name name-to-find)
       (return-from find-tag-in-lexenv (values name))))
   lexenv)
  (values nil))
