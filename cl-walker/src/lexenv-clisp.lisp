;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

#|

TODO provide the new api based on this old code. see lexenv-sbcl.lisp for an example.

(defun walk-vector-tree (function vector-tree)
  (labels ((%walk (vector-tree)
             (loop
                for index upfrom 0 by 2
                for tree-top = (aref vector-tree index)
                if (null tree-top)
                  do (return-from %walk nil)
                else if (vectorp tree-top)
                  do (return-from %walk
                       (%walk tree-top))
                else
                  do (funcall function
                              (aref vector-tree index)
                              (aref vector-tree (1+ index))))))
    (%walk vector-tree)))

(defmethod lexical-variables ((environment vector))
  (let ((vars '()))
    (when (aref environment 0)
      (walk-vector-tree (lambda (var-name var-spec)
                          (unless (system::symbol-macro-p var-spec)
                            (push var-name vars)))
                        (aref environment 0)))
    vars))

(defmethod lexical-functions ((environment vector))
  (let ((vars '()))
    (when (aref environment 1)
      (walk-vector-tree (lambda (func-name func-spec)
                          (push func-name vars))
                        (aref environment 1)))
    vars))

(defmethod lexical-macros ((environment vector))
  (let ((macros '()))
    (when (aref environment 1)
      (walk-vector-tree
       (lambda (macro-name macro-spec)
         (if (system::macrop macro-spec)
             (push (cons macro-name
                         (macro-function macro-name environment))
                   macros)))
       (aref environment 1)))
    macros))

(defmethod lexical-symbol-macros ((environment vector))
  (let (symbol-macros '())
    (when (aref environment 0)
      (walk-vector-tree
       (lambda (macro-name macro-spec)
         (if (system::symbol-macro-p macro-spec)
             (push (cons macro-name
                         (macroexpand-1 macro-name environment))
                   symbol-macros)))
       (aref environment 0)))
    symbol-macros))

(defun augment-with-var-and-fun (env &key var fun)
  (let* ((old-vars (aref env 0))
         (old-funs (aref env 1))
         (new-vars (if var
                       (make-array 3 :initial-contents (list (car var) (cdr var) old-vars))
                       (make-array 1 :initial-contents (list old-vars))))
         (new-funs (if fun
                       (make-array 3 :initial-contents (list (car fun) (cdr fun) old-funs))
                       (make-array 1 :initial-contents (list old-funs)))))
    (make-array 2 :initial-contents (list new-vars new-funs))))

;; I don't know whether T is an acceptable value to store here, but
;; CLISP does not complain.
(defmethod augment-with-variable ((env vector) var)
  (augment-with-var-and-fun env :var (cons var t)))

(defmethod augment-with-function ((env vector) fun)
  (augment-with-var-and-fun env :fun (cons fun t)))

(defmethod augment-with-macro ((env vector) mac def)
  (augment-with-var-and-fun env :fun (cons mac (system::make-macro def))))

(defmethod augment-with-symbol-macro ((env vector) symmac def)
  (augment-with-var-and-fun env :var (cons symmac (system::make-symbol-macro def))))

|#
