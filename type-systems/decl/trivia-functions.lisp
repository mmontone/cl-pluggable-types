;; TODO: it would be desirable to be able to control the order
;; in which the match-methods patterns are evaluated.
;; Trivia library patterns in MATCH expressions are also evaluated in
;; the order they were defined. There's no notion of more/less specific pattern than other.
;; So, either add an order number to define-match-method, or
;; implement a more-specific-pattern-p function somehow.
;; Or both.

(defpackage :trivia-functions
  (:use :cl)
  (:export
   #:define-match-function
   #:define-match-method))

(in-package :trivia-functions)

(defun match-function-methods (fname)
  (getf (get fname :trivia-function) :methods))

(defmacro define-match-function (fname args)
  `(progn
     (setf (get ',fname :trivia-function)
           (list :args ',args :methods nil))
     (defun ,fname ,args
       (dolist (method (match-function-methods ',fname))
         (multiple-value-bind (result matchedp)
             (funcall method ,@args)
           (when matchedp
             (return-from ,fname (values result t))))))))

(defmacro define-match-method (fname (pattern &rest args) &body body)
  (unless (get fname :trivia-function)
    (error "Match function not defined: ~a" fname))
  `(flet ((match-method (pattern-arg ,@args)
            (trivia:match pattern-arg
              (,pattern (values ,@body t))
              (_ (values nil nil)))))
     (push #'match-method (getf (get ',fname :trivia-function) :methods))
     ',fname))

;; (define-match-function match-test (pattern x))

;; (match-test 2 2)

;; (define-match-method match-test ("foo" x)
;;   (format nil "foo!! ~a" x))

;; (match-test 2 2)
;; (match-test "foo" 22)
;; (match-test '(foo "hello") 40)

;; (define-match-method match-test ((list 'foo string) x)
;;   (format nil "foo: ~a. ~a" string x))

;; (match-test '(foo "hello") 40)
