;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(cl:in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:cl-walker.system)
    (defpackage #:cl-walker.system
      (:use :common-lisp :asdf))))

(in-package #:cl-walker.system)

(defsystem :cl-walker
  :version "1.0"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>")
  :depends-on (:alexandria)
  :components ((:static-file "cl-walker.asd")
               (:module "src"
                :components ((:file "package")
                             (:file "duplicates" :depends-on ("package"))
                             #+sbcl(:file "lexenv-sbcl" :depends-on ("duplicates" "package"))
                             #+cmu(:file "lexenv-cmucl" :depends-on ("duplicates" "package"))
                             #+clisp(:file "lexenv-clisp" :depends-on ("duplicates" "package"))
                             #+lispworks(:file "lexenv-lispworks" :depends-on ("duplicates" "package"))
                             #+allegro(:file "lexenv-allegro" :depends-on ("duplicates" "package"))
                             #+openmcl(:file "lexenv-openmcl" :depends-on ("duplicates" "package"))
                             #+ecl(:file "lexenv-ecl" :depends-on ("duplicates" "package"))
                             (:file "lexenv-late" :depends-on ("package" "duplicates"
                                                               #+sbcl "lexenv-sbcl"))
                             (:file "infrastructure" :depends-on ("package" "lexenv-late" "duplicates"))
                             (:file "progn" :depends-on ("infrastructure"))
                             (:file "functions" :depends-on ("infrastructure" "progn"))
                             (:file "handlers" :depends-on ("infrastructure" "functions"))
                             (:file "ast-utils" :depends-on ("infrastructure" "handlers" "progn" "functions"))
                             (:file "implementation-specific" :depends-on ("infrastructure"))))))

(defsystem :cl-walker-test
  :depends-on (:cl-walker :stefil :alexandria :metabang-bind)
  :components ((:module "tests"
                        :components ((:file "package")
                                     (:file "walk-unwalk" :depends-on ("package"))
                                     (:file "ast-utils" :depends-on ("package"))
                                     (:file "lexenv" :depends-on ("package"))
                                     (:file "macros" :depends-on ("package"))))))

(defmethod perform ((op asdf:test-op) (system (eql (find-system :cl-walker))))
  (asdf:oos 'asdf:load-op :cl-walker-test)
  (in-package :cl-walker-test)
  (declaim (optimize (debug 3)))
  (warn "(declaim (optimize (debug 3))) was issued to help later C-c C-c'ing")
  (eval (read-from-string "(progn
                             (stefil:funcall-test-with-feedback-message 'test))"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-walker))))
  nil)
