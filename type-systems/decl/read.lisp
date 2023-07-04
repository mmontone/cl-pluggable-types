(in-package :pluggable-types/decl)

(defun typecheck-file
    (file)
  (let (defs)
    (with-open-file (in file)
      (let ((eof (list nil)))
        (do ((file-position (file-position in) (file-position in))
	     (code (read in nil eof) (read in nil eof)))
            ((eq code eof) (values))
          (let ((def (handler-case (hu.dwim.walker:walk-form code)
                       (error (e)
                         e))))
            (push (cons file-position def) defs)))))
    (nreverse defs)))


