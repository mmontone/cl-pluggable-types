(in-package :pluggable-types/decl)

(defvar *funtypes* nil)
(defvar *vartypes* nil)

(declaim (ftype (function (pathname) (list-of t)) typecheck-file))
(defun typecheck-file (file)
  (let (defs)
    (with-open-file (in file)
      (let ((eof (list nil)))
        (do ((file-position (file-position in) (file-position in))
             (code (read in nil eof) (read in nil eof)))
            ((eq code eof) (values))
          (let ((def (handler-case (hu.dwim.walker:walk-form code)
                       (error (e)
                         e))))
            (push (cons file-position def) defs)))))
    (nreverse defs)))

(declaim (ftype* (function (pathname) (values (list-of t) (list-of t))) read-type-declarations-from-file))
(defun read-type-declarations-from-file (pathname)
  (let ((ftypes nil)
        (vartypes nil))
    (with-open-file (in pathname)
      (let ((eof (list nil)))
        (do ((file-position (file-position in) (file-position in))
             (code (read in nil eof) (read in nil eof)))
            ((eq code eof) (values))
          (trivia:match code
            ((list 'in-package package-name)
             (setf *package* (find-package package-name)))
            ((cons 'declaim declarations)
             (dolist (declaration declarations)
               (trivia:match declaration
                 ((list (or 'ftype* 'ftype) ftype fname)
                  (push (cons fname ftype) ftypes))
                 ((list (or 'type 'type*) vartype varname)
                  (push (cons varname vartype) vartypes)))))))))
    (values (nreverse vartypes) (nreverse ftypes))))

;; (read-type-declarations-from-file (asdf:system-relative-pathname :pluggable-types-decl "type-systems/decl/read.lisp"))

;; (read-type-declarations-from-file (asdf:system-relative-pathname :pluggable-types-decl "type-systems/decl/cl-types.lisp"))

(defun load-type-declarations-from-file (pathname)
  (multiple-value-bind (vartypes funtypes)
      (read-type-declarations-from-file pathname)
    (appendf *vartypes* vartypes *vartypes*)
    (appendf *funtypes* funtypes *funtypes*)
    t))

;; (load-type-declarations-from-file (asdf:system-relative-pathname :pluggable-types-decl "type-systems/decl/read.lisp"))

(load-type-declarations-from-file (asdf:system-relative-pathname :pluggable-types-decl "type-systems/decl/cl-types.lisp"))


(declaim (ftype (function (pathname function-designator) t)
                read-lisp-file-definitions))
(defun read-lisp-file-definitions (pathname func)
  "General purpose function for reading definitions from a lisp file."
  (with-open-file (in pathname)
    (let ((eof (list nil)))
      (do ((file-position (file-position in) (file-position in))
           (code (read in nil eof) (read in nil eof)))
          ((eq code eof) (values))
        (funcall func code)))))
