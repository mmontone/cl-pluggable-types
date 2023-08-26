(in-package :pluggable-types)

(defclass type-checker-analyzer (code-analyzers:code-analyzer)
  ((type-checker :initarg :type-checker
                 :accessor type-checker
                 :type type-checker)))

(defun load-type-declaration (expr file)
  "Read and load the type declaration, if EXPR is a type declaration."
  (trivia:match expr
    ((cons 'eval-when (cons _ forms))
     (mapcar (rcurry #'load-type-declaration file) forms))
    ((cons 'declaim declarations)
     (dolist (declaration declarations)
       (trivia:match declaration
         ((cons 'typecheck args)
          (destructuring-bind (toggle &optional (scope :package)) args
            (toggle-typechecking toggle scope file)))
         ((list (or 'ftype 'ftype*) ftype fname)
          (push (cons fname ftype) *funtypes*))
         ((list (or 'type 'type*) type name)
          (push (cons name type) *vartypes*)))))))

(defun type-check-definition (expr)
  "Type check EXPR when appropiate."
  (unless (member (package-name *package*) *ignore-packages*)
    (trivia:match expr
      ((cons 'eval-when (cons _ forms))
       (mapcar #'type-check-definition forms))
      ((list* 'defun fname _)
       (unless (member fname *ignore-defs*)
         (call-with-type-error-handler
          (lambda () (check-form expr))))))))

(defun load-file-type-declarations (file &rest args)
  (declare (ignore args))
  (read-lisp-file-definitions (pathname file)
                              (rcurry #'load-type-declaration (pathname file))))

(defun check-file-types (file &rest args)
  (declare (ignore args))
  (when (and *compile-checks*
             (not (member file *ignore-files*)))
    (read-lisp-file-definitions (pathname file) #'type-check-definition)))
