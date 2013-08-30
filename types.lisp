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

(defstruct (function-type
	     (:print-function
	      (lambda (struct stream depth)
		(declare (ignore depth))
		(format stream "(FUN (~A~A~A~A) ~A)"
			(format nil "~{~a~^ ~}" (function-type-required-args-types struct))
			(or (aand (function-type-optional-args-types struct)
				  (format nil " &optional ~{~a~^ ~}" it))
			    "")
			(or (aand (function-type-keyword-args-types struct)
				  (with-output-to-string (s)
				    (format s " &key ")
				    (loop for (var . type) in it
				       do (format s "(~A ~A)" var type))))
			    "")
			(or (aand (function-type-rest-arg-type struct)
				  (format nil " &rest ~A" it))
			    "")
			(function-type-return-type struct)))))
  required-args-types
  optional-args-types
  keyword-args-types
  rest-arg-type
  return-type)

(defun function-type-spec (function-type)
  `(FUNCTION (
	      ,@(function-type-required-args-types function-type)
		,@(awhen (function-type-optional-args-types function-type)
			 (cons '&optional it))
		,@(awhen (function-type-keyword-args-types function-type)
			 (cons '&key 
			       (mapcar (lambda (var-and-type)
					 (list (car var-and-type)
					       (cdr var-and-type)))
				       it)))
		,@(awhen (function-type-rest-arg-type function-type)
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

