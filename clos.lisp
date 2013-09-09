(in-package :gradual)

;; TODO: try the MOP to define typed instances of classes, generic functions and methods
;;       as an alternative to macro expansion
;; Example: (defclass person ()
;;             ((name :accessor name
;;                    :type string))
;;             (:metaclass typed-class))
;;
;;          (defgeneric fullname (person)
;;              (:return-type string)
;;              (:generic-function-class typed-generic-function))

;; Ideas for polymorphism:

;; (typed-defclass (queue <a>) ()
;;    ((elems :initform nil
;; 	   :accessor elems
;; 	   :type (list <a>))))

;; ;; assume push has been shadowed from cl package
;; (typed-defmethod push ((elem <a>) (queue (queue <a>)))
;;   (declare (return-type (queue <a>)))
;;   (setf (elems queue) (cl:push elem (elems queue)))
;;   queue)

;; (typed-defmethod pop ((queue (queue <a>)))
;;    (declare (return-type <a>))
;;    (cl:pop (elems queue)))

;; (make-instance '(queue integer))

(defmethod make-instance ((class cons) &rest initargs)
  "Instantiate a polymorphic class"
  (error "Not implemented"))

(defvar *typed-generic-functions* nil)

(defclass typed-standard-generic-function (standard-generic-function)
  ((type :initarg :type
	 :initform (error "Provide the type")
	 :accessor generic-function-type)
   #+nil(source :initarg :source
	   :initform (error "Provide the source")
	   :accessor generic-function-source)
   )
  (:metaclass closer-mop:funcallable-standard-class))

(defclass typed-standard-method (standard-method)
  ((type :initarg :type
	 :initform (error "Provide the type")
	 :accessor method-type)
   (walked-source :initarg :walked-source
		  :initform (error "Provide the walked source")
		  :accessor method-source)))

(defmethod initialize-instance :after ((generic-function typed-standard-generic-function)
				       &rest initargs)
  (declare (ignore initargs))
  (setf *typed-generic-functions* (remove generic-function *typed-generic-functions*))
  (push generic-function *typed-generic-functions*))

(defun list-all-typed-generic-functions ()
  *typed-generic-functions*)

(defun find-typed-generic-function (name &optional (errorp t))
  (or (find name *typed-generic-functions*
	    :key #'closer-mop:generic-function-name)
      (and errorp
	   (error "Typed generic function ~A not found" name))))

(defmacro typed-defclass (name direct-superclasses direct-slots &rest options)
  `(progn
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)
     ,@(loop for slot in direct-slots
	    for props = (rest slot)
	    when (and (or (getf props :accessor)
			  (getf props :writer)
			  (getf props :reader))
		      (getf props :type))
	    appending (append
		       (when (getf props :accessor)
			 (list
			  `(typed-defmethod ,(getf props :accessor) ((object ,name))
			     (declare (return-type ,(getf props :type)))
			     (slot-value object ',(first slot)))
			  `(typed-defmethod (setf ,(getf props :accessor))
			       ((value ,(getf props :type)) (object ,name))
			     (declare (return-type ,(getf props :type)))
			     (setf (slot-value object ',(first slot)) value)
			     value)))
		       (when (getf props :reader)
			 (list
			  `(typed-defmethod ,(getf props :reader) ((object ,name))
			     (declare (return-type ,(getf props :type)))
			     (slot-value object ',(first slot)))))
		       (when (getf props :writer)
			 (list
			  `(typed-defmethod (setf ,(getf props :writer))
			       ((value ,(getf props :type)) (object ,name))
			     (declare (return-type ,(getf props :type)))
			     (setf (slot-value object ',(first slot)) value)
			     value)))))))

(defmacro typed-defgeneric (fun-name lambda-list &rest options)
  (flet ((options-remove (options list)
	   (loop for option in list
	      when (not (member (car option) options))
	      collect option))
	 (get-option (options key)
	   (loop for option in options
	      when (equalp key (car option))
	      return (cadr option))))

    ;; If the generic function definition doesn't contain a :typed or :type option,
    ;; then we assume the generic function to be untyped
    (if (not (or (get-option options :typed)
		 (get-option options :type)))
	`(defgeneric ,fun-name ,lambda-list ,@options)
	;; else
	;; we assume the generic function is typed
	(let ((generic-function-options
	       (options-remove
		(list :type :typed :return-type) options)))
	  
	  (when (get-option options :generic-function-class)
	    (error "Invalid option :generic-function-class for a typed generic function"))
	  (when (get-option options :method-class)
	    (error "Invalid option :method-class for a typed generic function"))

	  (if (get-option options :typed)
	      (progn
		(when (not (get-option options :return-type))
		  (simple-program-error "The return-type is missing"))
	    
		;; Treat the generic function lambda-list as a types lambda-list
		(multiple-value-bind (required-args-types
				      optional-args-types
				      rest-arg-type
				      keyword-args-types)
		    (parse-types-lambda-list lambda-list)
		  `(closer-mop:ensure-generic-function
		    ',fun-name
		    :lambda-list ',(types-lambda-list-to-normal lambda-list)
		    :type (make-function-type
			   :required-args-types ',required-args-types
			   :optional-args-types ',optional-args-types
			   :rest-arg-type ',rest-arg-type
			   :keyword-args-types ',keyword-args-types
			   :return-type ',(get-option options :return-type))
		    :generic-function-class 'typed-standard-generic-function
		    :method-class 'typed-standard-method
		    ,@generic-function-options)))
	      ;; else, (getf options :type)
	      (let ((function-type (parse-type (get-option options :type))))
		(when (not (typep function-type 'function-type))
		  (simple-program-error "Invalid type for generic function ~A" (get-option options :type)))
		`(closer-mop:ensure-generic-function
		  ',fun-name
		  :lambda-list ',lambda-list
		  :type (parse-type ',(get-option options :type))
		  :generic-function-class 'typed-standard-generic-function
		  :method-class 'typed-standard-method
		  ,@generic-function-options)))))))

(defun parse-defmethod (form)
  (let ((name (first form))
        (qualifiers nil))
    (do ((rest (cdr form) (cdr rest)))
        ((not (symbolp (car rest)))
         (values name (nreverse qualifiers) (car rest) (cdr rest)))
      (push (car rest) qualifiers))))

(defmacro typed-defmethod (&rest args)
  ;; Our approach here is this:
  ;; If there's not previous typed generic function, then
  ;; we throw a warning?? and consider the method/generic function untyped.
  (multiple-value-bind (name qualifiers lambda-list body)
      (parse-defmethod args)
    (declare (ignorable lambda-list))
    (multiple-value-bind (remaining-forms declarations doc-string)
	(parse-body body)
      (declare (ignore remaining-forms))
      (multiple-value-bind (function-type-declarations
			    var-type-declarations
			    return-type-declaration
			    other-declarations)
	  (extract-type-declarations declarations)
	(declare (ignore other-declarations))
	(multiple-value-bind (required-args
			      optional-args
			      rest-arg key-args)
	    (parse-typed-lambda-list lambda-list)
	  (flet ((read-arg-type (arg)
		   (let ((declared-type
			  (or
			   (aand
			    (find arg var-type-declarations
				  :key (lambda (declaration)
					 (destructuring-bind
					       (declare (var-type var type))
					     declaration
					   (declare (ignore declare var-type type))
					   var)))
			    (destructuring-bind (declare (var-type var type))
				it
			      (declare (ignore declare var-type var))
			      type))
			   t))
			 (lambda-list-type (or (aand (or (find arg (append required-args
									   optional-args
									   (list rest-arg))
							       :key #'first)
							 (find arg key-args
							       :key (compose #'second #'first)))
						     (if (equalp (length it) 2)
							 (second it)
							 (third it)))
					       t)))
		     (when (not (or (and (equalp lambda-list-type t)
					 (equalp declared-type t))
				    (or (equalp lambda-list-type t)
					(equalp declared-type t))))
		       (error "Duplicate type declaration for ~A. Declaration type: ~A lambda list type: ~A"
			      arg declared-type lambda-list-type))
		     (or (and (equalp lambda-list-type t)
			      declared-type)
			 (and (equalp declared-type t)
			      lambda-list-type)))))
	    (when function-type-declarations
	      (warn "~A doesn't make sense here" function-type-declarations))
	    (let* ((return-type (or (and return-type-declaration
					 (destructuring-bind (DECLARE (RETURN-TYPE type))
					     return-type-declaration
					   (declare (ignore declare return-type))
					   type))
				    t))
		   (required-args-types (mapcar #'read-arg-type (mapcar #'first required-args)))
		   (optional-args-types (mapcar #'read-arg-type (mapcar #'first optional-args)))
		   (keyword-args-types (mapcar (lambda (arg)
						 (cons arg
						       (read-arg-type arg)))
					       (mapcar (compose #'second #'first) key-args)))
		   (rest-arg-type (when rest-arg (read-arg-type (first rest-arg))))
		   (function-type `(make-function-type :required-args-types ',required-args-types
						       :optional-args-types ',optional-args-types
						       :keyword-args-types ',keyword-args-types
						       :rest-arg-type ',rest-arg-type
						       :return-type ',return-type))
		   (source (walk-form `(lambda ;; ,lambda-list  -- Use this eventually
					   ,(typed-lambda-list-to-normal lambda-list)
					 ,@body)))
		   (fbody (if *runtime-type-assertions-enabled*
			      `(progn
				 ,@(loop for arg in (append (mapcar #'first required-args)
							    (mapcar #'first optional-args)
							    (mapcar (compose #'second #'first) key-args))
				      for arg-type = (read-arg-type arg)
				      when (not (equalp arg-type t))
				      collect `(check-gradual-type ,arg ',arg-type))
				 ,@(when rest-arg
					 (with-unique-names (rest-elem)
					   (let ((arg-type (read-arg-type (first rest-arg))))
					     `((loop for ,rest-elem in ,(first rest-arg)
						  do (check-gradual-type ,rest-elem ',arg-type))))))
				 ,@(if (not (equalp return-type t))
				       (with-unique-names (result)
					 `((let ((,result (progn ,@(mapcar #'unwalk-form (body-of source)))))
					     (check-gradual-type ,result ',return-type)
					     ,result)))
				       (mapcar #'unwalk-form (body-of source))))
			      `(progn ,@(mapcar #'unwalk-form (body-of source))))))
	      `(if (not (find-typed-generic-function ',name))
		   (defmethod ,name ,@qualifiers ,(typed-lambda-list-to-normal lambda-list)
			      ,@(remove-if (lambda (declaration)
					     (member declaration (list 'function-type
								       'return-type
								       'var-type)))
					   declarations :key #'caadr)
			      ,fbody)
		   ;; else
		   (let* ((gf (find-typed-generic-function ',name))
			  (method-lambda (closer-mop::make-method-lambda
					  gf
					  (closer-mop::class-prototype (closer-mop::generic-function-method-class gf))
					  '(lambda ,(typed-lambda-list-to-normal lambda-list)
					    ,@(when doc-string (list doc-string))
					    ,@(remove-if (lambda (declaration)
							   (member declaration (list 'function-type
										     'return-type
										     'var-type)))
							 declarations :key #'caadr)
									   
					    ,fbody)
					  nil)))
		     (closer-mop::add-method
		      gf
		      (make-instance 'typed-standard-method
				     :qualifiers ',qualifiers
				     :lambda-list ',(typed-lambda-list-to-normal lambda-list)
				     :specializers ',(mapcar #'sb-pcl::specializer-from-type required-args-types)
				     :function (compile nil method-lambda)	     
				     :walked-source
				     (walk-form
				      '(lambda ;; ,lambda-list  -- Use this eventually
					,(typed-lambda-list-to-normal lambda-list)
					,@(when doc-string (list doc-string))
					,@(remove-if (lambda (declaration)
						       (member declaration (list 'function-type
										 'return-type
										 'var-type)))
						     declarations :key #'caadr)
					,fbody))
				     :type ,function-type))
		     (when *typechecking-enabled*
		       (typecheck-everything))
		     ',name)))))))))
