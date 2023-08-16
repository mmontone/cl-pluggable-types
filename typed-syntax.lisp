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
        (if (null car)
            cdr
            (cons car cdr)))))

(tree-remove-if (lambda (x) (eql 'foo x)) '(foo))
(tree-remove-if (lambda (x) (eql 'foo x)) '(foo foo))
(tree-remove-if (lambda (x) (eql 'foo x)) '((foo foo)))
(tree-remove-if (lambda (x) (eql 'foo x)) '(x (foo foo)))
(tree-remove-if (lambda (x) (eql 'foo x)) '(x (y foo foo)))

(tree-remove-if #'vectorp '(x #(integer) y #(string)))

(defstruct type-annotation
  type)

(cl:defun remove-type-annotations (def)
  (let ((annotated-def (parse-type-annotations def)))
    (destructuring-bind (def name annotated-args &body annotated-body)
        annotated-def
      `(,def ,name ,(tree-remove-if #'type-annotation-p annotated-args)
         ,@(if (type-annotation-p (first annotated-body))
              (rest annotated-body)
              annotated-body)))))

(remove-type-annotations
 '(defun hello (x <integer>)
   (print "hello")))

(remove-type-annotations
 '(defun hello (x <integer> y <string>)
   (print "hello")))

(remove-type-annotations
 '(defun hello (x <list-of string>) <string>
   (first x)))

(remove-type-annotations
 '(defun hello (x <list-of string> &optional (y <string> "lala")) <string>
   (first x)))   

(cl:defun parse-type-annotations (list-of-symbols)
  (let* ((str (prin1-to-string list-of-symbols))
         (str (ppcre:regex-replace-all "\\<" str "#S(TYPE-ANNOTATION :TYPE ("))
         (str (ppcre:regex-replace-all ">" str "))")))
    (read-from-string str)))

(parse-type-annotations (read-from-string "(<list-of number>)"))
(parse-type-annotations (read-from-string "(<number>)"))

(parse-type-annotations (read-from-string "(<list-of <cons-of integer string>>)"))

(parse-type-annotations (read-from-string "(<list-of (cons-of integer string)>)"))

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
