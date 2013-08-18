;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

;;;; These are for forms which certain compilers treat specially but
;;;; aren't macros or special-operators.

#+lispworks
(defwalker-handler compiler::internal-the (form parent env)
  (walk-form (third form) parent env))
