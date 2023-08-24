(in-package :pluggable-types)

(defvar *debug* nil)

(defun enable-debugging (&optional (enable-p t))
  (setf *debug* enable-p))

(defun debug-format (&rest args)
  (when *debug*
    (apply #'format args)))

(defgeneric typecheck-everything (type-checker &optional output))
