(defpackage :extensible-declarations
  (:nicknames :xdecl)
  (:use :cl)
  (:export
   :declaim*
   :proclaim*
   :declare*))

(in-package :extensible-declarations)

;; note that the 'declaration' declaration exists, for custom declarations:
;; (proclaim '(declaration my-declaration))
;; (declaration name1 name2 ...) advises the compiler that each namej is a valid but non-standard declaration name. The purpose of this is to tell one compiler not to issue warnings for declarations meant for another compiler or other program processor. 

(defvar *declaration-handlers*
  (make-hash-table))

(defun proclaim* (declarations)
  (dolist (declaration declarations)
    (let ((declaration-type (first declaration)))
      (if ))))
