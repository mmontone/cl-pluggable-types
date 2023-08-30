(in-package :pluggable-types)

(defclass type-checker-analyzer (code-analyzers:code-analyzer)
  ((type-checker :initarg :type-checker
                 :accessor type-checker
                 :initform *type-checker*
                 :type (or null type-checker))))

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

(defun type-check-definition (expr analyzer)
  "Type check EXPR when appropiate."
  (let ((type-checker (or (type-checker analyzer)
                          *type-checker*)))
    (when type-checker
      (unless (member (package-name *package*) (code-analyzers:ignored-packages analyzer))
        (trivia:match expr
          ((cons 'eval-when (cons _ forms))
           (mapcar #'type-check-definition forms analyzer))
          ((list* 'defun fname _)
           (unless (member fname (code-analyzers:ignored-definitions analyzer))
             (call-with-type-error-handler
              (lambda () (check-form expr :env nil :type-checker type-checker))))))))))

(defmethod code-analyzers:analyze-file ((analyzer type-checker-analyzer)
                                        file)
  ;; First load type declarations
  (code-analyzers:read-lisp-file-definitions (pathname file)
                              (rcurry #'load-type-declaration (pathname file)))
  ;; Then check types
  (code-analyzers:read-lisp-file-definitions (pathname file)
                                             (rcurry #'type-check-definition analyzer)))

(code-analyzers:register-code-analyzer
 'type-checker (make-instance 'type-checker-analyzer))
