(in-package :cl-user)

;; Load the walker system

(load (merge-pathnames
       "cl-walker/cl-walker.asd"
       (make-pathname :directory (pathname-directory *load-pathname*))))

(defpackage gradual-system
  (:use :cl :asdf))
  
(in-package :gradual-system)

(defsystem :gradual
  :name "gradual"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :version "0.1"
  :maintainer "Mariano Montone <marianomontone@gmail.com>"
  :licence "
Copyright (c) 2011 Mariano Montone

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the \"Software\"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE."
  :description "Gradual typing for Common Lisp"
  :long-description "Gradual typing for Common Lisp"
  :components
  ((:file "package")
   (:file "gradual")
   (:file "conditions")
   (:file "types")
   (:file "environment")
   (:file "typechecking")
   (:file "inference")
   (:file "gradual-common-lisp"))
  :serial t
  :depends-on (:cl-walker :anaphora :alexandria :mw-equiv))
