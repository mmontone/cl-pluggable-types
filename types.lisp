(in-package :gradual)

(defun equidimensional (a)
  (or (< (array-rank a) 2)
      (apply #'= (array-dimensions a))))

(deftype square-matrix (&optional type size)
  `(and (array ,type (,size ,size))
	(satisfies equidimensional)))

(defun plistp (list)
  (and (listp list)
       (evenp (length list))
       (loop
	  for result = t then (and result (keywordp k))	    
	  for k in list by #'cddr
	  finally (return result))))

(deftype plist ()
  `(and cons
	(satisfies plistp)))

;(subtypep 'plist 'cons)

(defun alistp (list)
  (and (listp list)
       (reduce (lambda (b elem)
		 (and b (consp elem)))
	       list :initial-value t)))

(deftype alist ()
  `(and cons
	(satisfies alistp)))

;; Function types

;; TODO:

;; Look at sb-kernel::values-specifier-type
;; Example: (sb-kernel::values-specifier-type 'number)
;; Actually, lots of function types stuff here is a reimplementation of
;; sb-kernel::values-specifier-type.
;; For instance, evaluate this:
;; (sb-kernel::values-specifier-type '(function (string &optional boolean &key (x number)) string))
;; (sb-kernel::values-specifier-type '(function (integer &rest string) (values boolean integer)))

;; Ideas?::
;; Consider (optional) checked-exceptions declarations and (optional) dynamic variable (implicit parameters) usage declarations in function types
;; Like:
;; (defun open-file (f)
;;     (declare (signals 'file-not-found-error))
;;   ...)
;;
;; (defun scoped-function ()
;;   (declare (uses *var*))
;;   ...)

(defstruct args-type
  required-args-types
  optional-args-types
  keyword-args-types
  rest-arg-type)

(defun print-args-type (struct stream)
  (format stream "~A~A~A~A"
	  (format nil "~{~a~^ ~}" (args-type-required-args-types struct))
	  (or (aand (args-type-optional-args-types struct)
		    (format nil " &optional ~{~a~^ ~}" it))
	      "")
	  (or (aand (args-type-keyword-args-types struct)
		    (with-output-to-string (s)
		      (format s " &key ")
		      (loop for (var . type) in it
			 do (format s "(~A ~A)" var type))))
	      "")
	  (or (aand (args-type-rest-arg-type struct)
		    (format nil " &rest ~A" it))
	      "")))

(defstruct (function-type
	     (:include args-type)
	     (:print-function
	      (lambda (struct stream depth)
		(declare (ignore depth))
		(format stream "(FUN (")
		(print-args-type struct stream)
		(format stream ") ~A)" (function-type-return-type struct)))))
  return-type)

(defun function-type-spec (function-type)
  `(FUNCTION (
	      ,@(args-type-required-args-types function-type)
		,@(awhen (args-type-optional-args-types function-type)
			 (cons '&optional it))
		,@(awhen (args-type-keyword-args-types function-type)
			 (cons '&key 
			       (mapcar (lambda (var-and-type)
					 (list (car var-and-type)
					       (cdr var-and-type)))
				       it)))
		,@(awhen (args-type-rest-arg-type function-type)
			 (list '&rest
			       it))
		)
	     ,(function-type-return-type function-type)))

(defmethod gradual-subtypep (t1 t2)
  (if (equalp t2 t)
      t
      (subtypep t1 t2)))

(defmethod gradual-subtypep (t1 (t2 cons))
  (if (equalp (first t2) 'values)
      (gradual-subtypep t1 (second t2))
      (subtypep t1 t2)))

(defmethod gradual-subtypep ((t1 cons) t2)
  (if (equalp (first t1) 'values)
      (gradual-subtypep (second t1) t2)
      (subtypep t1 t2)))

(defmethod gradual-subtypep ((t1 function-type) t2)
  (subtypep (function-type-spec t1) t2))

(defmethod gradual-subtypep (t1 (t2 function-type))
  (subtypep t1 (function-type-spec t2)))

;; Types parsing

(defclass type-var ()
  ((name :initarg :name
	 :accessor name
	 :type symbol
	 :initform (error "Provide the type-var name"))))

(defmethod print-object ((type-var type-var) stream)
  (format stream "<~A>" (name type-var)))

(defclass union-type ()
  ((types :initarg :types
	  :accessor types
	  :initform (error "Provide the types")
	  :type list)))

(defmethod print-object ((union-type union-type) stream)
  (format stream "(OR ~{~A~^ ~})" (types union-type)))

(defclass values-type ()
  ((args :initarg :args)))

(defclass member-type ()
  ((types :initarg :types
	  :accessor types
	  :initform (error "Provide the types")
	  :type list)))

(defun parse-type (spec)
  (if (symbolp spec)
      (let ((spec-string (symbol-name spec)))
	(if (and (equalp (char spec-string 0) #\<)
		 (equalp (char spec-string (1- (length spec-string)))
			 #\>))
	    ;; It is a type variable
	    (make-instance 'type-var
			   :name (intern
				  (subseq spec-string 1
					  (1- (length spec-string)))))
	    ;; else, just use the symbol
	    spec))
      ;; else
      ))
