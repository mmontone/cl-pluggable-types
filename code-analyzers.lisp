(defpackage :code-analyzers
  (:use :cl :alexandria)
  (:export #:code-analyzer)
  (:documentation "Abstract interface for running code analyzers."))

(in-package :code-analyzers)

(defvar *code-analyzers* (make-hash-table)
  "The table of available CODE-ANALYZERs.")

(defvar *code-analyzers-enabled* t
  "When enabled, run code analyzers when file and definitions are compiled.")

(defvar *debug-code-analyzers* nil
  "When enabled, errors in the code analyzers are not handled.")

(defvar *analyzed-file* nil
  "Current file being analyzed.")

(defclass code-analyzer ()
  ((ignored-packages
    :initform nil
    :type list
    :accessor ignored-packages
    :documentation "List of packages to ignore by the CODE-ANALYZER.")
   (ignored-files
    :initform nil
    :type list
    :accessor ignored-files
    :documentation "List of files to be ignored by the CODE-ANALYZER.")
   (ignored-definitions
    :initform nil
    :type list
    :accessor ignored-definitions
    :documentation "List of function definitions to be ignored by the CODE-ANALYZER.")
   (packages
    :initform nil
    :type list
    :accessor packages
    :documentation "List of packages to analyze.")
   (files
    :initform nil
    :type list
    :accessor files
    :documentation "List of files to analyze.")
   (definitions
    :initform nil
    :type list
    :accessor definitions
    :documentation "List of definitions to analyze."))
  (:documentation "A code analyzer"))

;; Use the ANALYZE declaration to control what gets analyzed.
;; Syntax: (ANALIZE analizer-name option &rest args)
(declaim (declaration analyze))

(defun condition-message (condition)
  "Get the descriptive message of CONDITION."
  (with-output-to-string (s)
    (write condition :escape nil :stream s)))

(defun analyzer-warn (error)
  (warn (condition-message error)))

(defun analyzer-error (error)
  (error (condition-message error)))

(defgeneric analyze-definition (analizer definition)
  (:documentation "Use ANALIZER to analyzed DEFINITION."))

(defgeneric analyze-file (analizer file)
  (:documentation "Use ANALIZER to analyze FILE."))

(defgeneric process-declaration (analyzer option args)
  (:documentation "Process ANALYZE declaration."))

(defun parse-analyzer-scope (scope)
  "Parse elements in SCOPE.

The elements in SCOPE can be:
- :package, then set to the current package.
- :file, then set to the current file being compiled.
- A symbol, then it is assumed as a function definition name.
- (PACKAGE package-name), then the package with that name.
- A PATHNAME, then the file at that pathname name."

  (let ((packages '())
        (files '())
        (definitions '()))
    (dolist (element scope)
      (cond
        ((and (listp element)
              (eql (first element) 'package))
         (push (the symbol (second element)) packages))
        ((eql element :package)
         (push (package-name *package*) packages))
        ((pathnamep element)
         (push element files))
        ((eql element :file)
         (push (the pathname *analyzed-file*) files))
        ((and (symbolp element)
              (not (keywordp element)))
         (push element definitions))))
    (values definitions files packages)))

(defmethod process-declaration ((analyzer code-analyzer)
                                (option (eql :analyze))
                                scope)
  "Enable analysis for elements in SCOPE.

The elements in SCOPE can be:
- :package, then set to the current package.
- :file, then set to the current file being compiled.
- A symbol, then it is assumed as a function definition name.
- (PACKAGE package-name), then the package with that name.
- A PATHNAME, then the file at that pathname name.

Examples:

(DECLAIM (ANALYZE my-analyzer :analyze :package))

(DECLAIM (ANALYZE my-analyzer :analyze (package some-package) (package other-package)))"

  (multiple-value-bind (definitions files packages)
      (parse-analyzer-scope scope)
    (appendf (packages analyzer) packages)
    (appendf (files analyzer) files)
    (appendf (definitions analyzer) definitions)))

(defmethod process-declaration ((analyzer code-analyzer)
                                (option (eql :ignore))
                                scope)
  "Disable analysis for elements in SCOPE.

The elements in SCOPE can be:
- :package, then set to the current package.
- :file, then set to the current file being compiled.
- A symbol, then it is assumed as a function definition name.
- (PACKAGE package-name), then the package with that name.
- A PATHNAME, then the file at that pathname name.

Examples:

(DECLAIM (ANALYZE my-analyzer :ignore :package))

(DECLAIM (ANALYZE my-analyzer :ignore #p\"some-file.lisp\")))"

  (multiple-value-bind (definitions files packages)
      (parse-analyzer-scope scope)
    (appendf (ignored-packages analyzer) packages)
    (appendf (ignored-files analyzer) files)
    (appendf (ignored-definitions analyzer) definitions)))

(defun call-with-analyzer-error-handler (func)
  (if *debug-code-analyzers*
      (funcall func)
      (handler-case
          (funcall func)
        (error (e)
          (warn 'simple-warning
                :format-control "Error analyzing: ~a"
                :format-arguments (list (condition-message e)))))))

(defun should-analyze-p (analyzer file)
  (not (member file (ignored-files analyzer))))

(defun analyze-file-hook (file &rest args)
  (declare (ignore args))
  (when *code-analyzers-enabled*
    (dolist (analyzer *code-analyzers*)
      (when (should-analyze-p analyzer file)
        (analyze-file analyzer file)))))

(push 'analyze-file-hook compiler-hooks:*after-compile-file-hooks*)
