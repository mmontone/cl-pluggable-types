(in-package :pluggable-types/const)

(defvar *compile-checks* nil
  "When enabled, run the type checker when a file or function is compiled.")

(defvar *debug-compile-checks* nil
  "When enabled, type checking errors are not handled.")

(defvar *type-error-reporter* 'type-check-warn
  "The function for reporting a type error.")

(defun type-check-warn (type-error)
  (warn (mutils-utils:condition-message type-error)))

(defun load-type-declaration (expr)
  "Read and load the type declaration, if EXPR is a type declaration."
  (trivia:match expr
    ((cons 'eval-when (cons _ forms))
     (mapcar #'load-type-declaration forms))
    ((cons 'declaim declarations)
     (dolist (declaration declarations)
       (trivia:match declaration
         ((list (or 'ftype 'ftype*) ftype fname)
          (push (cons fname ftype) *funtypes*))
         ((list (or 'type 'type*) type name)
          (push (cons name type) *vartypes*)))))))

(defun call-with-type-error-handler (func)
  (if *debug-compile-checks*
      (funcall func)
      (handler-case
          (funcall func)
        (pluggable-types/const::type-checking-error (e)
          (funcall *type-error-reporter* e))
        (error (e)
          (warn 'simple-warning
                :format-control "Error typechecking: ~a"
                :format-arguments (list (mutils-utils:condition-message e)))))))

(defun type-check-definition (expr)
  "Type check EXPR when appropiate."
  (trivia:match expr
    ((cons 'eval-when (cons _ forms))
     (mapcar #'type-check-definition forms))
    ((cons 'defun _)
     (call-with-type-error-handler
      (lambda () (pluggable-types/const::check-form expr))))))

(defun load-file-type-declarations (file &rest args)
  (declare (ignore args))
  (read-lisp-file-definitions (pathname file) #'load-type-declaration))

(defun check-file-types (file &rest args)
  (declare (ignore args))
  (when *compile-checks*
    (read-lisp-file-definitions (pathname file) #'type-check-definition)))

(push 'check-file-types compiler-hooks:*after-compile-file-hooks*)
(push 'load-file-type-declarations compiler-hooks:*after-compile-file-hooks*)
