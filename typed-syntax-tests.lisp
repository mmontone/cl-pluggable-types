(in-package :typed-syntax)

(tree-remove-if (lambda (x) (eql 'foo x)) '(foo))
(tree-remove-if (lambda (x) (eql 'foo x)) '(foo foo))
(tree-remove-if (lambda (x) (eql 'foo x)) '((foo foo)))
(tree-remove-if (lambda (x) (eql 'foo x)) '(x (foo foo)))
(tree-remove-if (lambda (x) (eql 'foo x)) '(x (y foo foo)))
(tree-remove-if #'vectorp '(x #(integer) y #(string)))

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
 '(defun hello (x <list-of string>) <list-of string>
   (first x)))

(remove-type-annotations
 '(defun hello (x <list-of string> &optional (y <string> "lala")) <string>
   (first x)))


(parse-type-annotations (read-from-string "(<list-of number>)"))
(parse-type-annotations (read-from-string "(<number>)"))
(parse-type-annotations (read-from-string "(<list-of <cons-of integer string>>)"))
(parse-type-annotations (read-from-string "(<list-of (cons-of integer string)>)"))
(parse-type-annotations (read-from-string "(defun hello (x <integer> y <string>) <integer>
(print (> 2 3)))"))
(parse-type-annotations (read-from-string "(> 2 3)"))
(parse-type-annotations (read-from-string "(string> 2 3)"))
(parse-type-annotations '<string>)


(annotate-defun '(defun my-func (x <integer> &optional (y <list-of string>)) x))
(annotate-defun '(defun my-func (x <integer> &optional (y <list-of string>)) <integer> x))

(annotate-defun '(defun my-func (x <integer> &optional (y <list-of string>)) <list-of string> x))

(extract-function-types
 (annotate-defun
  '(defun hello (x <integer>))))

(extract-function-types
 (annotate-defun
  '(defun hello (x <integer>) <string>)))

(extract-function-types
 (annotate-defun
  '(defun hello (x <integer> y <string>))))

(extract-function-types
 (annotate-defun
  '(defun hello (x y <string>))))

(extract-function-types
 (annotate-defun
  '(defun hello (x <integer> y <string> &optional z))))

(extract-cl-function-type
 (annotate-defun
  '(defun hello (x <integer> y <string>))))

(extract-cl-function-type
 (annotate-defun
  '(defun hello (x <integer> y <string> &optional z))))

(extract-cl-function-type
 (annotate-defun
  '(defun hello (x <integer> y <string> &optional z <boolean>))))

(extract-cl-function-type
 (annotate-defun
  '(defun hello (x <integer> y <string> &optional (z <boolean> t)))))

(extract-cl-function-type
 (annotate-defun
  '(defun hello (x <integer> y <string> &optional (z <boolean> t) w))))

(extract-cl-function-type
 (annotate-defun
  '(defun hello (x <integer> y <string> &optional (z <boolean> t) w) <string>)))

(extract-cl-function-type
 (annotate-defun
  '(defun hello (x <integer> y <string> &optional (z <boolean> t) w) <list-of string>)))

(extract-cl-function-type
 (annotate-defun
  '(defun hello (x <integer> y <string> &optional (z <list-of boolean> t) w) <list-of string>
    (< 2 4))))

(extract-cl-function-type
 (annotate-defun
  '(defun hello (x <integer> y <string> &key (z <list-of boolean> 'lala) w) <list-of string>
    (< 2 4))))

(count-ocurrences #\< (symbol-name '<asdf<asdf>>))

(extract-return-type '(<list-of string> (print 'lala)))
(extract-return-type '(<string> (print 'lala)))
(extract-return-type '(<list-of (cons-of number integer)> (print 'lala)))
(parse-type-annotations (extract-return-type '(<list-of (cons-of number integer)> (print 'lala))))

(<t>:defun test1 (x <integer> y <integer>) <integer>
  (+ x y))

(<t>:defun test2 (x <integer> &key (y <integer> 22))
  (+ x y))

(<t>:defun test3 (x <integer> &rest more <string>)
  (apply #'+ x more))
