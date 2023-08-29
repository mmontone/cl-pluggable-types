(require :code-analyzers)
(require :lisp-critic)

(defpackage :critic-code-analyzer
  (:use :cl)
  (:export :critic
           :critic-code-analyzer))

(in-package :critic-code-analyzer)

(defvar *patterns* (lisp-critic::get-pattern-names))

(defclass critic-code-analyzer (code-analyzers:code-analyzer)
  ())

(defun reformat-critique (critique)
  "Remove the separators from CRITIQUE."
  (with-input-from-string (in critique)
    (let ((lines (uiop/stream:slurp-stream-lines in)))
      (with-output-to-string (s)
        (dolist (line (butlast (rest lines)))
          (write-string line s)
          (terpri s))))))

(defmethod code-analyzers:analyze-definition
    ((analyzer critic-code-analyzer) code)
  (let ((critique
          (with-output-to-string (out)
            (lisp-critic::critique-definition code out *patterns*))))
    (unless (zerop (length critique))
      (setq critique (reformat-critique critique))
      ;; TODO: the signaled condition does not contain a source code location.
      ;; Would that be possible to add?
      (warn 'code-analyzers:code-analyzer-warning :format-control critique))))

(code-analyzers:register-code-analyzer 'critic (make-instance 'critic-code-analyzer))

(provide :critic-code-analyzer)
