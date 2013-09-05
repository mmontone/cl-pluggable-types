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
			 (list `(typed-defmethod ,(getf props :accessor) ((object ,name))
						 (declare (return-type ,(getf props :type)))
						 (slot-value object ',(first slot)))
			       `(typed-defmethod (setf ,(getf props :accessor))
						 ((value ,(getf props :type)) (object ,name))
						 (declare (return-type ,(getf props :type)))
						 (setf (slot-value object ',(first slot)) value)
						 value)))
		       (when (getf props :reader)
			 (list `(typed-defmethod ,(getf props :reader) ((object ,name))
						 (declare (return-type ,(getf props :type)))
						 (slot-value object ',(first slot)))))
		       (when (getf props :writer)
			 (list `(typed-defmethod (setf ,(getf props :writer))
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
    (if (not (or (get-option options :typed)
		 (get-option options :type)))
	`(defgeneric ,fun-name ,lambda-list ,@options)
	;; else
	(let ((generic-function-options
	       (options-remove
		(list :type :typed :return-type) options))) 
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
		  `(progn
		     (set-fun-type ',fun-name
				   (make-function-type
				    :required-args-types ',required-args-types
				    :optional-args-types ',optional-args-types
				    :rest-arg-type ',rest-arg-type
				    :keyword-args-types ',keyword-args-types
				    :return-type ',(get-option options :return-type)))
		     (defgeneric ,fun-name ,(types-lambda-list-to-normal lambda-list)
		       ,@generic-function-options))))
	      ;; else, (getf options :type)
	      (let ((function-type (parse-type (get-option options :type))))
		(when (not (typep function-type 'function-type))
		  (simple-program-error "Invalid type for generic function ~A" (get-option options :type)))
		 `(progn
		    (set-fun-type ',fun-name (parse-type ',(get-option options :type)))
		    (defgeneric ,fun-name ,lambda-list
		      ,@generic-function-options))))))))

(defun parse-defmethod (form)
  (let ((name (first form))
        (qualifiers nil))
    (do ((rest (cdr form) (cdr rest)))
        ((not (symbolp (car rest)))
         (values name (nreverse qualifiers) (car rest) (cdr rest)))
      (push (car rest) qualifiers))))

(defun parse-typed-method-lambda-list (lambda-list &key (normalize t)
						     allow-specializers
						     (normalize-optional normalize)
						     (normalize-keyword normalize)
						     (normalize-auxilary normalize))
  "Parses a gradual typed method lambda-list, returning as multiple values:

1. Required parameters.

2. Optional parameter specifications, normalized into form:

   (name init suppliedp)

3. Name of the rest parameter, or NIL.

4. Keyword parameter specifications, normalized into form:

   ((keyword-name name) init suppliedp)

5. Boolean indicating &ALLOW-OTHER-KEYS presence.

6. &AUX parameter specifications, normalized into form

   (name init).

7. Existence of &KEY in the lambda-list.

Signals a PROGRAM-ERROR is the lambda-list is malformed."
  (let ((state :required)
        (allow-other-keys nil)
        (auxp nil)
        (required nil)
        (optional nil)
        (rest nil)
        (keys nil)
        (keyp nil)
        (aux nil))
    (labels ((fail (elt)
               (simple-program-error "Misplaced ~S in ordinary lambda-list:~%  ~S"
                                     elt lambda-list))
             (check-variable (elt what &optional (allow-specializers allow-specializers))
               (unless (and (or (symbolp elt)
                                (and allow-specializers
                                     (consp elt) (= 2 (length elt)) (symbolp (first elt))))
                            (not (constantp elt)))
                 (simple-program-error "Invalid ~A ~S in gradual lambda-list:~%  ~S"
                                       what elt lambda-list)))
	     (check-type-spec (type what)
	       (declare (ignore type what))
	       ;; TODO: how to verify a typespec is valid? symbolp is not enough
	       #+nil(unless (symbolp type)
		 (simple-program-error "Invalid ~A type spec ~A in typed lambda list:~% ~S"
				       what
				       type
				       lambda-list)))
             (check-spec (spec what)
               (destructuring-bind (init &optional type suppliedp) spec
                 (declare (ignore init))
		 (when type
		   (check-type-spec type what))
		 (when suppliedp
		   (check-variable suppliedp what nil)))))
      (dolist (elt lambda-list)
        (case elt
          (&optional
           (if (eq state :required)
               (setf state elt)
               (fail elt)))
          (&rest
           (if (member state '(:required &optional))
               (setf state elt)
               (fail elt)))
          (&key
           (if (member state '(:required &optional :after-rest))
               (setf state elt)
               (fail elt))
           (setf keyp t))
          (&allow-other-keys
           (if (eq state '&key)
               (setf allow-other-keys t
                     state elt)
               (fail elt)))
          (&aux
           (cond ((eq state '&rest)
                  (fail elt))
                 (auxp
                  (simple-program-error "Multiple ~S in ordinary lambda-list:~%  ~S"
                                        elt lambda-list))
                 (t
                  (setf auxp t
                        state elt))
                 ))
          (otherwise
           (when (member elt '#.(set-difference lambda-list-keywords
                                                '(&optional &rest &key &allow-other-keys &aux)))
             (simple-program-error
              "Bad lambda-list keyword ~S in ordinary lambda-list:~%  ~S"
              elt lambda-list))
           (case state
             (:required
	      (if (listp elt)
		  (destructuring-bind (var type-or-specializer) elt
		    (check-variable var "required parameter")
		    (check-type-spec type-or-specializer "required parameter")
		    (let ((type
			   (or (and (consp type-or-specializer)
				    (equalp (first type-or-specializer)
					    'eql)
				    (or (ignore-errors
					  (%typecheck-form
					   (walk-form (second type-or-specializer))
					   (make-typing-environment)))
					t))
			       type-or-specializer)))
		      (push (list var type-or-specializer type) required)))
		  ; else
		  (progn
		    (check-variable elt "required parameter")
		    (push (list elt t t) required))))
             (&optional
              (cond ((consp elt)
                     (destructuring-bind (name &rest tail) elt
                       (check-variable name "optional parameter")
                       (cond ((cdr tail)
                              (check-spec tail "optional-supplied-p parameter"))
                             (normalize-optional
                              (setf elt (append elt '(t)))))))
                    (t
                     (check-variable elt "optional parameter")
                     (when normalize-optional
                       (setf elt (cons elt '(nil t))))))
              (push (ensure-list elt) optional))
             (&rest
	      (if (consp elt)
		  (destructuring-bind (var type) elt
		    (check-variable var "rest parameter")
		    (check-type-spec type "rest parameter")
		    (setf rest elt))
		  ;; else
		  (progn
		    (check-variable elt "rest parameter")
		    (setf rest (list elt t))))
	      (setf state :after-rest))
             (&key
              (cond ((consp elt)
                     (destructuring-bind (var-or-kv &rest tail) elt
                       (cond ((consp var-or-kv)
                              (destructuring-bind (keyword var) var-or-kv
                                (unless (symbolp keyword)
                                  (simple-program-error "Invalid keyword name ~S in ordinary ~
                                                         lambda-list:~%  ~S"
                                                        keyword lambda-list))
                                (check-variable var "keyword parameter")))
                             (t
                              (check-variable var-or-kv "keyword parameter")
                              (when normalize-keyword
                                (setf var-or-kv (list (make-keyword var-or-kv) var-or-kv))
				)))
                       (if (cdr tail)
                           (check-spec tail "keyword-supplied-p parameter")
                           (when normalize-keyword
                             (setf tail (append tail '(t)))))
                       (setf elt (cons var-or-kv tail))))
                    (t
                     (check-variable elt "keyword parameter")
                     (setf elt (if normalize-keyword
                                   (list (list (make-keyword elt) elt) nil t)
                                   elt))))
              (push elt keys))
             (&aux
              (if (consp elt)
                  (destructuring-bind (var &optional init) elt
                    (declare (ignore init))
                    (check-variable var "&aux parameter"))
                  (progn
                    (check-variable elt "&aux parameter")
                    (setf elt (list* elt (when normalize-auxilary
                                           '(nil))))))
              (push elt aux))
             (t
              (simple-program-error "Invalid typed lambda-list:~%  ~S" lambda-list)))))))
    (values (nreverse required) (nreverse optional) rest (nreverse keys)
            allow-other-keys (nreverse aux) keyp)))

(defun typed-method-lambda-list-to-normal (typed-lambda-list)
  (multiple-value-bind (required optional rest
				 keys allow-other-keys aux)
      (parse-typed-method-lambda-list typed-lambda-list)
    (append (mapcar (lambda (arg)
		      (list (first arg)
			    (second arg)))
		    required)
	    (when optional
	      (cons '&optional
		    (mapcar (lambda (arg)
			      (list (first arg)
				    (second arg)))
			    optional)))
	    (when rest
	      (cons '&rest (list (first rest))))
	    (when keys
	      (cons '&key
		    (mapcar (lambda (arg)
			      (list (first arg)
				    (second arg)))
			    keys)))
	    (when allow-other-keys
	      (cons '&allow-other-keys
		    allow-other-keys))
	    (when aux
	      (cons '&aux aux)))))

(defmacro typed-defmethod (&rest args)
  ;; Our approach here is this:
  ;; If there's not previous typed generic function, then
  ;; we throw a warning and consider the method/generic function untyped.
  (multiple-value-bind (name qualifiers lambda-list body)
      (parse-defmethod args)
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
	    (parse-typed-method-lambda-list args)
	(flet ((read-arg-type (arg)
		 (let ((declared-type
			(or
			 (aand
			  (find arg var-type-declarations
				:key (lambda (declaration)
				       (destructuring-bind (declare (var-type var type)) declaration
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
		 (source (walk-form `(defun ,name ;; ,args  -- Use this eventually
					 ,(typed-lambda-list-to-normal args)
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
	    `(if (not (fun-type ',name))
		 (warn "No generic function. ~A method is untyped when defining method"
		       ',name)
		 ;; else
		 (progn
		   (add-typed-method ',name (make-instance 'typed-method
							   :source (walk-form '(defun ,name ;; ,args  -- Use this eventually
										,(typed-lambda-list-to-normal args)
										,@body))
							   :type ,function-type))
		   (defmethod ,name ,(typed-method-lambda-list-to-normal args)
		     ,@(when doc-string (list doc-string))
		     ,@(remove-if (lambda (declaration)
				    (member declaration (list 'function-type 'return-type 'var-type)))
				  declarations :key #'caadr)
		     ,fbody)
		   (when *typechecking-enabled*
		     (typecheck))
		   ',name)))))))))

(defmethod make-instance ((class cons) &rest initargs)
  "Instantiate a polymorphic class"
  (error "Not implemented"))

#+nil(defun slots-for-class (class-name)
  (let ((class (find-class class-name)))
    (nconc (mapcar #'closer-mop:slot-definition-name
                   (class-instance-slots class))
           (mapcar #'closer-mop:slot-definition-name
                   (closer-mop:class-class-slots class)))))

(defun split-off-declarations (body)
  (do ((rest body (cdr rest))
       (declarations nil))
      ((null rest)
       (values declarations nil))
    (if (or (stringp (car rest))
            (and (listp (car rest))
                 (eq 'declare (car (car rest)))))
        (push (car rest) declarations)
        (return-from split-off-declarations
          (values (nreverse declarations) rest)))))
