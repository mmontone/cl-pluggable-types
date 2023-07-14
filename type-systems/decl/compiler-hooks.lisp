(defpackage :compiler-hooks
  (:use :cl)
  (:export
   #:*before-compile-hooks*
   #:*after-compile-hooks*
   #:*before-compile-file-hooks*
   #:*after-compile-file-hooks*))

(in-package :compiler-hooks)

(defvar *after-compile-hooks* nil)
(defvar *before-compile-hooks* nil)

(defvar *before-compile-file-hooks* nil)
(defvar *after-compile-file-hooks* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-package-locks:without-package-locks
    (let ((compile (fdefinition 'compile)))
      (flet ((compile-with-hooks (name &optional definition warnings-p failure-p)
               (dolist (hook *before-compile-hooks*)
                 (funcall hook name definition warnings-p failure-p))
               (prog1
                   (funcall compile name definition warnings-p failure-p)
                 (dolist (hook *after-compile-hooks*)
                   (funcall hook name definition warnings-p failure-p)))))
        (setf (fdefinition 'compile) #'compile-with-hooks)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-package-locks:without-package-locks
    (let ((compile-file (fdefinition 'compile-file)))
      (flet ((compile-file-with-hooks (input-file &rest args &key output-file verbose
                                                    print external-format output-truename warnings-p failure-p)
               (dolist (hook *before-compile-file-hooks*)
                 (apply hook input-file args))
               (prog1
                   (apply compile-file input-file args)
                 (dolist (hook *after-compile-file-hooks*)
                   (apply hook input-file args)))))
        (setf (fdefinition 'compile-file) #'compile-file-with-hooks)))))
