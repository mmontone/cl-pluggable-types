(in-package :pluggable-types/decl)

(declaim (declaration ftype* type*))

(defvar *funtypes* nil)
(defvar *vartypes* nil)

(declaim (ftype* (function (pathname) (list-of t)) typecheck-file))
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
                  (push (cons vartype varname) vartypes)))))))))
    (values (nreverse vartypes) (nreverse ftypes))))

;; (read-type-declarations-from-file (asdf:system-relative-pathname :pluggable-types-decl "type-systems/decl/read.lisp"))

;; (read-type-declarations-from-file (asdf:system-relative-pathname :pluggable-types-decl "type-systems/decl/cl-types.lisp"))

(defun load-type-declarations-from-file (pathname)
  (multiple-value-bind (vartypes funtypes)
      (read-type-declarations-from-file pathname)
    (nconcf *vartypes* vartypes *vartypes*)
    (nconcf *funtypes* funtypes *funtypes*)))

;; (load-type-declarations-from-file (asdf:system-relative-pathname :pluggable-types-decl "type-systems/decl/read.lisp"))

;; (load-type-declarations-from-file (asdf:system-relative-pathname :pluggable-types-decl "type-systems/decl/cl-types.lisp"))
