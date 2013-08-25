(in-package :gradual)

(define-condition gradual-type-error (simple-error)
  ((source :initarg :source
	   :accessor source)))

(defun gradual-type-error (source message &rest args)
  (cerror "Continue"
	  'gradual-type-error
	  :format-control message
	  :format-arguments args
	  :source source))


  
