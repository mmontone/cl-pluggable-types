(defpackage :lisp-critic-warnings
  (:use :cl))

(in-package :lisp-critic-warnings)

(define-condition lisp-critic-style-warning (alexandria:simple-style-warning)
  ())

(defun reformat-critique (critique)
  "Remove the separators from CRITIQUE."
  (with-input-from-string (in critique)
    (let ((lines (uiop/stream:slurp-stream-lines in)))
      (with-output-to-string (s)
        (dolist (line (butlast (rest lines)))
          (write-string line s)
          (terpri s))))))

(defun critique-file (file &rest args)
  "Critique definitions found in FILE, using patterns in NAMES."
  (declare (ignore args))
  (let ((names (lisp-critic::get-pattern-names)))
    (with-open-file (in file)
      (let ((eof (list nil)))
        (do ((code (read in nil eof) (read in nil eof)))
            ((eql code eof) (values))
          (let ((critique
                  (with-output-to-string (out)
                    (lisp-critic::critique-definition code out names))))
            (unless (zerop (length critique))
              (setq critique (reformat-critique critique))
              ;; TODO: the signaled condition does not contain a source code location.
              ;; Would that be possible to add?
              (warn 'lisp-critic-style-warning :format-control critique))))))))

;; (pushnew 'critique-file compiler-hooks:*after-compile-file-hooks*)
