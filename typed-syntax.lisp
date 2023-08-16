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

(parse-type-annotations (read-from-string "(defun hello (x <integer> y <string>) <integer>
(print (> 2 3)))"))

(parse-type-annotations (read-from-string "(> 2 3)"))
(parse-type-annotations (read-from-string "(string> 2 3)"))

(parse-type-annotations '<string>)

(cl:defun annotate-defun (defun)
  (destructuring-bind (defun name args &body body)
      defun
    (let ((return-type (parse-type-annotations (first body))))
      (list* defun name (parse-type-annotations args)
             (when (type-annotation-p return-type)
               return-type)
             (if (type-annotation-p return-type)
                 (rest body)
                 body)))))

(annotate-defun '(defun my-func (x <integer> &optional (y <list-of string>)) x))
(annotate-defun '(defun my-func (x <integer> &optional (y <list-of string>)) <integer> x))

(cl:defun cl-type (type-annotation)
  (let ((cl-type (type-annotation-type type-annotation)))
    (if (and (listp cl-type) (null (cdr cl-type)))
        (car cl-type)
        cl-type)))

(cl:defun extract-function-types (annotated-defun)
  (let ((status :required)
        (arg-position :arg)
        (required '())
        (required-types '())
        (optional '())
        (optional-types '())
        (key '())
        (rest nil)
        (return nil))
    (destructuring-bind (defun name args &body body) annotated-defun
      (declare (ignore defun name))
      (dolist (arg args) args
        (block next
          (cl:flet ((maybe-switch-status ()
                      (when (eql arg '&optional)
                        (setf status :optional)
                        (setf arg-position :arg)
                        (return-from next))
                      (when (eql arg '&key)
                        (setf status :key)
                        (setf arg-position :arg)
                        (return-from next))
                      (when (eql arg '&rest)
                        (setf status :rest)
                        (setf arg-position :arg)
                        (return-from next))))
            (ecase status
              (:required
               (maybe-switch-status)
               (ecase arg-position
                 (:arg
                  (push arg required)
                  (setf arg-position :type))
                 (:type
                  (if (not (type-annotation-p arg))
                      (progn
                        (push (make-type-annotation :type 't) required-types)
                        (push arg required))
                      (progn
                        (push arg required-types)
                        (setf arg-position :arg))))))
              (:optional
               (tagbody retry
                  (ecase arg-position
                    (:arg
                     (cond
                       ((atom arg)
                        (push arg optional)
                        (setf arg-position :type))
                       ((listp arg)
                        (let ((type
                                (find-if #'type-annotation-p arg)))
                          (if type
                              (push type optional)
                              (push (make-type-annotation :type 't) optional-types))))))
                    (:type
                     (when (not (type-annotation-p arg))
                       (push (make-type-annotation :type 't) optional-types)
                       (setf arg-position arg)
                       (go retry))
                     (push arg optional-types)
                     (setf arg-position arg)))))
              (:key (error "TODO"))
              (:rest (error "TODO"))))))
      (setf return (if (type-annotation-p (first body))
                       (first body)
                       (make-type-annotation :type 't)))
      (values (mapcar #'cons
                      (reverse required)
                      (reverse required-types))
              (mapcar #'cons
                      (reverse optional)
                      (reverse optional-types))
              key rest return))))

(cl:defun extract-cl-function-type (annotated-defun)
  (multiple-value-bind (required optional key rest return)
      (extract-function-types annotated-defun)
    `(function (,@(mapcar (alexandria:compose #'cl-type #'cdr) required))
               ,(cl-type return))))

(extract-function-types
 (annotate-defun
  '(defun hello (x <integer>))))

(extract-function-types
 (annotate-defun
  '(defun hello (x <integer> y <string>))))

(extract-function-types
 (annotate-defun
  '(defun hello (x y <string>))))

(extract-function-types
 (annotate-defun
  '(defun hello (x <integer> y <string> &optional z))))

(extract-cl-function-type (annotate-defun
  '(defun hello (x <integer> y <string>)))) 
    
    

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
