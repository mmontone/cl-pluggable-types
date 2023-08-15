(defpackage :typed-syntax
  (:nicknames :<t>)
  (:use :cl)
  (:shadow
   #:defun #:flet #:defvar #:defparameter)
  (:export
   #:defun
   #:flet
   #:defvar
   #:defparameter))

(in-package :typed-syntax)

(cl:defun extract-function-type (def)
  ;; TODO
  `(function (&rest t) t))

(cl:defun tree-remove-if (predicate tree)
  (if (atom tree)
      (if (funcall predicate tree)
          nil
          tree)
      (let ((car (tree-remove-if predicate (car tree)))
            (cdr (tree-remove-if predicate (cdr tree))))
        (cond
          ((and car cdr)
           (cons car cdr))
          (car car)))))

(tree-remove-if (lambda (x) (eql 'foo x)) '(foo))
(tree-remove-if (lambda (x) (eql 'foo x)) '(foo foo))
(tree-remove-if (lambda (x) (eql 'foo x)) '((foo foo)))
(tree-remove-if (lambda (x) (eql 'foo x)) '(x (foo foo)))
(tree-remove-if (lambda (x) (eql 'foo x)) '(x (y foo foo)))

(cl:defun type-annotation-p (thing)
  (and ))


(cl:defun remove-type-annotations (def)
  def)

(defmacro defun (name args &body body)
  "Define a function allowing type annotations.

Syntax:

Example:

(<t>:defun sum (x <integer> y <integer>) <integer>
    (+ x y))
"
  (let ((function-type (extract-function-type
                        `(defun ,name ,args ,@body)))
        (untyped-definition (remove-type-annotations
                             `(defun ,name ,args ,@body))))
    `(progn
       (declaim (ftype ,function-type) ,name)
       ,(destructuring-bind (name args &body body) untyped-definition
          `(cl:defun ,name ,args ,@body)))))
