;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker-test)

(defsuite* (test/macros :in test))

(deftest test/macros/macrolet ()
  (check-walk-unwalk '(macrolet ((+ (&body body)
                                  (reverse body)))
                       (+ 1 2 3 -))
                     '(locally (- 3 2 1)))

  (check-walk-unwalk '(macrolet ())
                     '(locally))

  (check-walk-unwalk '(macrolet ((+ (&body body)
                                  (reverse body)))
                       (princ "1111")
                       (+ 1 2 3 -))
                     '(locally
                       (princ "1111")
                       (- 3 2 1))))

(deftest test/macros/symbol-macrolet ()
  (check-walk-unwalk '(let ((obj 42))
                       (symbol-macrolet ((a (slot-value obj 'a))
                                         (b (slot-value obj 'b)))
                         (+ a b)))
                     '(let ((obj 42))
                       (locally
                           (+ (slot-value obj 'a) (slot-value obj 'b)))))

  (check-walk-unwalk '(symbol-macrolet ())
                     '(locally))

  (check-walk-unwalk '(let ((obj 42))
                       (symbol-macrolet ((a (slot-value obj 'a)))
                         (null a)
                         (/ a 2)))
                     '(let ((obj 42))
                       (locally
                           (null (slot-value obj 'a))
                         (/ (slot-value obj 'a) 2)))))

