(in-package :pluggable-types)

(defun parse-typed-lambda-list (lambda-list &key (normalize t)
					      allow-specializers
					      (normalize-optional normalize)
					      (normalize-keyword normalize)
					      (normalize-auxilary normalize))
  "Parses a gradual lambda-list, returning as multiple values:

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
		  (progn
		    (check-variable (first elt) "required parameter")
		    (check-type-spec (second elt) "required parameter")
		    (push elt required))
		  (progn
		    (check-variable elt "required parameter")
		    (push (list elt t) required))))
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

(defun typed-lambda-list-to-normal (typed-lambda-list)
  (multiple-value-bind (required optional rest keys allow-other-keys aux)
      (parse-typed-lambda-list typed-lambda-list)
    (append (mapcar #'first required)
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

(defun types-lambda-list-to-normal (types-lambda-list)
  (multiple-value-bind (required optional rest keys allow-other-keys aux)
      (parse-types-lambda-list types-lambda-list)
    (append required
	    (when optional
	      (cons '&optional
		    optional))
	    (when rest
	      (cons '&rest (list rest)))
	    (when keys
	      (cons '&key
		    (mapcar #'first
			    keys)))
	    (when allow-other-keys
	      (cons '&allow-other-keys
		    allow-other-keys))
	    (when aux
	      (cons '&aux aux)))))

(defun extract-type-declarations (declarations)
  (let* ((function-type-declarations 
	  (remove-if-not (lambda (x)
			   (equalp x 'fun-type))
			 declarations
			 :key #'caadr))
	 (var-type-declarations
	  (remove-if-not (lambda (x)
			   (equalp x 'var-type))
			 declarations
			 :key #'caadr))
	 (return-type-declarations
	  (remove-if-not (lambda (x)
			   (equalp x 'return-type))
			 declarations
			 :key #'caadr))
	 (other-declarations (set-difference declarations
					     (append function-type-declarations
						     var-type-declarations
						     return-type-declarations))))
    (values function-type-declarations
	    var-type-declarations
	    (first return-type-declarations)
	    other-declarations)))

(defun parse-types-lambda-list (lambda-list &key (normalize t)
					      allow-specializers
					      (normalize-optional normalize)
					      (normalize-keyword normalize)
					      (normalize-auxilary normalize))
  "Parses a types lambda-list, returning as multiple values:

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
               (simple-program-error "Misplaced ~S in types lambda-list:~%  ~S"
                                     elt lambda-list))
             (check-variable (elt what &optional (allow-specializers allow-specializers))
               (unless (and (or (symbolp elt)
                                (and allow-specializers
                                     (consp elt) (= 2 (length elt)) (symbolp (first elt))))
                            (not (constantp elt)))
                 (simple-program-error "Invalid ~A ~S in ordinary lambda-list:~%  ~S"
                                       what elt lambda-list)))
             (check-spec (spec what)
               (destructuring-bind (init suppliedp) spec
                 (declare (ignore init))
                 (check-variable suppliedp what nil)))
	     (check-type-spec (type what)
	       ;; TODO: verify type spec validity. symbolp is not enough
	       #+nil(unless (symbolp type)
		 (simple-program-error "Invalid ~A type spec ~A in types lambda list:~% ~S"
				       what
				       type
				       lambda-list))))
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
                  (simple-program-error "Multiple ~S in types lambda-list:~%  ~S"
                                        elt lambda-list))
                 (t
                  (setf auxp t
                        state elt))
                 ))
          (otherwise
           (when (member elt '#.(set-difference lambda-list-keywords
                                                '(&optional &rest &key &allow-other-keys &aux)))
             (simple-program-error
              "Bad lambda-list keyword ~S in types lambda-list:~%  ~S"
              elt lambda-list))
           (case state
             (:required
              (check-type-spec elt "required parameter")
              (push elt required))
             (&optional
	      (check-type-spec elt "optional parameter")
	      (push elt optional))
             (&rest
              (check-type-spec elt "rest parameter")
              (setf rest elt
                    state :after-rest))
             (&key
	      (when (not (and (consp elt)
			      (equalp (length elt) 2)))
		(simple-program-error "Invalid keyword type spec ~A in lambda-list:~% ~S"
				      elt
				      lambda-list))
	      (destructuring-bind (var type) elt
		(check-variable var "keyword parameter")
		(check-type-spec type "keyword parameter")
		(push (cons (car elt) (cadr elt)) keys)))
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
              (simple-program-error "Invalid types lambda-list:~%  ~S" lambda-list)))))))
    (values (nreverse required) (nreverse optional) rest (nreverse keys)
            allow-other-keys (nreverse aux) keyp)))

#+util
(defun format-readme ()
  (clhs-linker:link-file
   (asdf:system-relative-pathname :pluggable-types "README.source.md")
   (asdf:system-relative-pathname :pluggable-types "README.md")))
