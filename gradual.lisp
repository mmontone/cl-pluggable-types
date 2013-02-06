(in-package :gradual)

(defparameter *var-types* (make-hash-table :test #'equalp))
(defparameter *fun-types* (make-hash-table :test #'equalp))
(defparameter *environment* (make-hash-table :test #'equalp))

(defun set-var-type (var type)
  (setf (gethash var *var-types*) type))

(defun var-type (var)
  (or
   (gethash var *var-types*)
   'any))

(defun set-fun-type (fun-name type)
  (setf (gethash fun-name *fun-types*) type))

(defun fun-type (fun-name)
  (or
   (gethash fun-name *fun-types*)
   '(list :required-vars any
     :return any)))

(defun gradual-type-error ()
  (error "Type error"))

(defmacro $defparameter (var val &optional doc (type 'any))
  `(prog1
     (defparameter ,var ,val ,doc)
     (when (not (check-gradual-type ,var ',type))
       (gradual-type-error))
     (set-var-type ',var ',type)))

(defun check-gradual-type (object type &key (throw-error-p t))
  (let ((type-check-result (%check-gradual-type (type-of object) type)))
    (when (and (not type-check-result) throw-error-p)
      (gradual-type-error))
    type-check-result))

(defmethod %check-gradual-type (object-type type)
  (or (equalp object-type type)
      (error "Undefined")))

(defmethod %check-gradual-type (object-type (type (eql 'any)))
  t)

(defmethod %check-gradual-type (object-type (type (eql 'integer)))
  (and (listp object-type) (equalp (first object-type) 'integer)))

(defmethod %check-gradual-type (object-type (type (eql 'number)))
  (or (and (listp object-type) (equalp (first object-type) 'integer))
      (equalp object-type 'bit)))

(defmacro $setq (var value)
  `(progn
     (check-gradual-type ,value (var-type ',var))
     (setq ,var ,value)))

(defun parse-gradual-lambda-list (lambda-list &key (normalize t)
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
             (check-spec (spec what)
               (destructuring-bind (init suppliedp) spec
                 (declare (ignore init))
                 (check-variable suppliedp what nil)))
	     (check-type-spec (type)
	       (unless (symbolp type)
		 (simple-program-error "Invalid type spec ~A" type))))
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
		    (check-type-spec (second elt))
		    (push elt required))
		  (progn
		    (check-variable elt "required parameter")
		    (push (list elt 'any) required))))
             (&optional
              (cond ((consp elt)
                     (destructuring-bind (name &rest tail) elt
                       (check-variable name "optional parameter")
                       (cond ((cdr tail)
                              (check-spec tail "optional-supplied-p parameter"))
                             (normalize-optional
                              (setf elt (append elt '(nil)))))))
                    (t
                     (check-variable elt "optional parameter")
                     (when normalize-optional
                       (setf elt (cons elt '(nil nil))))))
              (push (ensure-list elt) optional))
             (&rest
              (check-variable elt "rest parameter")
              (setf rest elt
                    state :after-rest))
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
                                (setf var-or-kv (list (make-keyword var-or-kv) var-or-kv)))))
                       (if (cdr tail)
                           (check-spec tail "keyword-supplied-p parameter")
                           (when normalize-keyword
                             (setf tail (append tail '(nil)))))
                       (setf elt (cons var-or-kv tail))))
                    (t
                     (check-variable elt "keyword parameter")
                     (setf elt (if normalize-keyword
                                   (list (list (make-keyword elt) elt) nil nil)
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
              (simple-program-error "Invalid ordinary lambda-list:~%  ~S" lambda-list)))))))
    (values (nreverse required) (nreverse optional) rest (nreverse keys)
            allow-other-keys (nreverse aux) keyp)))

#+nil(defmacro $defun (name args return-type &body body)
  (multiple-value-bind (required-vars-spec)
      (parse-gradual-lambda-list args)
    (let ((required-types (mapcar #'second required-vars-spec))
	  (required-vars (mapcar #'first required-vars-spec)))
      `(progn
	 (set-fun-type ',name
		       (list :required-vars ',required-types
			     :return ',return-type))
	 (defun ,name ,required-vars ,@body)))))

(defmacro $defun (name args &body body)
  (multiple-value-bind (remaining-forms declarations doc-string)
      (parse-body body)
    (let ((function-type-declaration
	   (find 'function-type declarations :key #'caadr)))
      `(progn
	 ,@(when function-type-declaration
		 `((set-fun-type ',name ',(cadadr function-type-declaration))))
	 (defun ,name ,args
	   ,doc-string
	   ,@(remove 'function-type declarations :key #'caadr)
	   ,@remaining-forms)))))

(defvar *typechecking-enabled* nil)

(defun call-with-typechecking (enabled-p function)
  (let ((*typechecking-enabled* enabled-p))
    (funcall function)))

(defmacro without-typechecking (&body body)
  `(call-with-typechecking nil (lambda () ,@body)))

(defmacro with-typechecking (&body body)
  `(call-with-typechecking t (lambda () ,@body)))