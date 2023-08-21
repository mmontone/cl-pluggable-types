(in-package :pluggable-types)

(defvar *compile-checks* nil
  "When enabled, run the type checker when a file or function is compiled.")

(defvar *debug-compile-checks* nil
  "When enabled, type checking errors are not handled.")

(defvar *type-error-reporter* 'type-check-warn
  "The function for reporting a type error.")

(defvar *ignore-packages* '()
  "List of packages to ignore when typechecking.")

(defvar *ignore-files* '()
  "List of files to ignore when typechecking.")

(defvar *ignore-defs* '()
  "The list of function definitions to ignore when typechecking.")

(defvar *typecheck-packages* '()
  "When set, only typecheck packages from this list.")

(defvar *typeckeck-files* '()
  "When set, only typecheck files from this list.")

;; Use the TYPECHECK declaration to control what gets typechecked
(declaim (declaration typecheck))

(defun type-check-warn (type-error)
  (warn (mutils-utils:condition-message type-error)))

(defun type-check-error (type-error)
  (error (mutils-utils:condition-message type-error)))

(declaim (ftype (function (boolean (or symbol (member :package :file)) &rest t) t)
                toggle-typechecking))
(defun toggle-typechecking (enable what &rest args)
  (ecase what
    (:package
     (if enable
         (removef *ignore-packages* (package-name *package*))
         (pushnew (package-name *package*) *ignore-packages*)))
    (:file
     (if enable
         (removef *ignore-files* (the pathname (car args)))
         (pushnew (the pathname (car args)) *ignore-files*)))
    (t (pushnew (the symbol what) *ignore-defs*))))

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

(defun call-with-type-error-handler (func)
  (if *debug-compile-checks*
      (funcall func)
      (handler-case
          (funcall func)
        (type-checking-error (e)
          (funcall *type-error-reporter* e))
        (error (e)
          (warn 'simple-warning
                :format-control "Error typechecking: ~a"
                :format-arguments (list (mutils-utils:condition-message e)))))))

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

(push 'check-file-types compiler-hooks:*after-compile-file-hooks*)
(push 'load-file-type-declarations compiler-hooks:*after-compile-file-hooks*)
