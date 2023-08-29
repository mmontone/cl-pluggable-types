(require :alexandria)

(defpackage :code-analyzers
  (:use :cl :alexandria)
  (:export #:code-analyzer
           #:analyze
           #:find-code-analyzer
           #:*code-analyzers*
           #:*code-analyzers-enabled*
           #:*debug-code-analyzers*
           #:analyze-file
           #:analyze-definition
           #:read-lisp-file-definitions
           #:register-code-analyzer)
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

(defvar *ignore-declarations-errors* nil)

(defclass code-analyzer ()
  ((enabled
    :initform t
    :type boolean
    :accessor analyzer-enabled-p
    :documentation "Whether the analyzer is enabled or not.")
   (ignored-packages
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
;; Syntax: (ANALYZE analyzer-name option &rest args)
;; Default options:
;; - :analyze, followed by what to analyze.
;; - :ignore, followed by what to ignore.
(declaim (declaration analyze))

(defun condition-message (condition)
  "Get the descriptive message of CONDITION."
  (with-output-to-string (s)
    (write condition :escape nil :stream s)))

(defun analyzer-warn (error)
  (warn (condition-message error)))

(defun analyzer-error (error)
  (error (condition-message error)))

(defgeneric analyze-definition (analyzer definition)
  (:documentation "Use ANALYZER to analyzed DEFINITION."))

(defgeneric analyze-file (analyzer file)
  (:documentation "Use ANALYZER to analyze FILE.
May want to use READ-LISP-FILE-DEFINITIONS."))

(defgeneric process-declaration (analyzer option args)
  (:documentation "Process ANALYZE declaration."))

(declaim (ftype (function (symbol code-analyzer) t)
                register-code-analyzer))
(defun register-code-analyzer (name code-analyzer)
  "Register CODE-ANALYZER under NAME."
  (setf (gethash (the symbol name) *code-analyzers*)
        (the code-analyzer code-analyzer)))

(defun call-with-analyzer-error-handler (func)
  (if *debug-code-analyzers*
      (funcall func)
      (handler-case
          (funcall func)
        (error (e)
          (warn 'simple-warning
                :format-control "Error analyzing: ~a"
                :format-arguments (list (condition-message e)))))))

(defmethod analyze-file ((analyzer code-analyzer) file)
  "The default file analyzer. Read the file definitions and invoke ANALYZE-DEFINITION."
  (read-lisp-file-definitions
   file
   (curry #'analyze-definition analyzer)))

(defun should-analyze-p (analyzer thing)
  (etypecase thing
    (package
     (not (member (package-name thing) (ignored-packages analyzer))))
    (pathname
     (not (member thing (ignored-files analyzer))))
    (symbol
     (not (member thing (ignored-definitions analyzer))))))

(defun find-code-analyzer (name &optional (error-p t))
  "Find a code analyzer by name."
  (or (gethash name *code-analyzers*)
      (and error-p (error "Code analyzer not available: ~s" name))))

(defun analyze-file-hook (file &rest args)
  (declare (ignore args))
  ;; Always analyze with a CONTROLLER-CODE-ANALYZER first
  (analyze-file (make-instance 'controller-code-analyzer) file)
  ;; Then use
  (when *code-analyzers-enabled*
    (dolist (analyzer *code-analyzers*)
      (when (and (analyzer-enabled-p analyzer)
                 (should-analyze-p analyzer file))
        (analyze-file analyzer file)))))

(declaim (ftype (function (pathname (or symbol function)) t)
                read-lisp-file-edefinitions))
(defun read-lisp-file-definitions (pathname func)
  "General purpose function for reading definitions from a lisp file."
  (with-open-file (in pathname)
    (let ((eof (list nil)))
      (do ((file-position (file-position in) (file-position in))
           (code (read in nil eof) (read in nil eof)))
          ((eq code eof) (values))
        (funcall func code)))))

(push 'analyze-file-hook compiler-hooks:*after-compile-file-hooks*)

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

;; The ANALYZERS analyzer controls all analyzers (whether they are enabled or not, debugging, error handling, etc)
;; Syntax: (ANALYZE ANALYZERS option &rest args)
;; Options:
;; - :debug, follwed by a boolean. Affects all analyzers.
;; - :enabled, follwed by a boolean, and an optional list of analyzers. Affects the passed analyzers, or all analyzers.

(defclass controller-code-analyzer (code-analyzer)
  ()
  (:documentation "Reads ANALYZE declarations to control CODE-ANALYZERs behaviour."))

(defmethod process-declaration ((analyzer controller-code-analyzer)
                                (option (eql :enabled))
                                value)
  (destructuring-bind (enabled-p &rest analyzers) value
    (if (null analyzers)
        (setf *code-analyzers-enabled* (not (not enabled-p)))
        (dolist (analyzer-name analyzers)
          (setf (analyzer-enabled-p analyzer) (not (not enabled-p)))))))

(defmethod process-declaration ((analyzer controller-code-analyzer)
                                (option (eql :debug))
                                value)
  (setf *debug-code-analyzers* (not (not (car value)))))
