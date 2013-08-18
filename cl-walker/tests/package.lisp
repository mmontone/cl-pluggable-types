;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(cl:in-package :cl-walker)

(defpackage :cl-walker-test
  (:use :common-lisp
        :cl-walker
        :alexandria
        :stefil
        :metabang-bind
        ))

(import
 '()
 :cl-walker-test)

(in-package :cl-walker-test)

(defsuite* (test :in root-suite))
