(in-package :gradual)

(defun infer-type (form type-system &rest args)
  "Infer the type of FORM under a fresh TYPING-ENVIRONMENT."
  (let ((type-system (if (symbolp type-system)
                         (apply #'make-instance type-system args)
                         type-system)))
    (type-system-infer-type
     type-system
     (if (typep form 'cl-walker:walked-form)
       form
       (cl-walker:walk-form form))
     (make-typing-environment type-system))))

(defgeneric type-system-infer-type (type-system form typing-environment)
  (:documentation "Infer the type of FORM under TYPING-ENVIRONMENT."))

(defmethod type-system-infer-type ((type-system gradual-type-system)
                                   (form free-variable-reference-form) typing-env)
  (var-type (name-of form)))

(defmethod type-system-infer-type ((type-system gradual-type-system)
                                   (form special-variable-reference-form) typing-env)
  (var-type (name-of form)))

(defmethod type-system-infer-type ((type-system gradual-type-system)
                                   (form constant-form) typing-environment)
  (type-of (value-of form)))

(defmethod type-system-infer-type ((type-system gradual-type-system)
                                   (form free-application-form) typing-environment)
  (cond
    ;; test for: (make-instance 'foo)
    ((and (eql (operator-of form) 'make-instance)
          (typep (first (arguments-of form)) 'cl-walker:constant-form))
     (value-of (first (arguments-of form))))
    ;; otherwise, use function type information
    (t
     (let ((function-type (fun-type (operator-of form)))
           (args-types (mapcar (lambda (arg)
                                 (type-system-infer-type type-system arg typing-environment))
                               (arguments-of form))))
       (return-type function-type)))))

(defun canonize-type (type)
  (cond
    ((and (listp type)
          (eql (first type) 'or))
     ;; TODO: do better
     (let ((rest-types (remove-duplicates (rest type))))
       (if (= (length rest-types) 1)
           (first rest-types)
           `(or ,@rest-types))))
    (t
     type)))

(defmethod type-system-infer-type ((type-system gradual-type-system)
                                   (form if-form) typing-env)
  (canonize-type
   `(or
     ,(type-system-infer-type type-system (then-of form) typing-env)
     ,(type-system-infer-type type-system (else-of form) typing-env))))

(defmethod type-system-infer-type ((type-system gradual-type-system)
                                   (form let-form) typing-environment)
  (let ((fresh-typing-environment typing-environment))
    (loop for binding in (bindings-of form)
          do
             (setf fresh-typing-environment
                   (setf (type-of-var fresh-typing-environment (name-of binding))
                         (if (not (cl-walker::type-spec binding))
                             (type-system-infer-type type-system (value-of binding) typing-environment)
                             (cl-walker::type-spec binding)))))
    (type-system-infer-type type-system (car (last (body-of form))) fresh-typing-environment)))

(defmethod type-system-infer-type ((type-system gradual-type-system)
                                   (form walked-lexical-variable-reference-form) typing-environment)
  (type-of-var (name-of form) typing-environment))

(defmethod type-system-infer-type ((type-system gradual-type-system)
                                   (form the-form) typing-environment)
  (let* ((the-value (value-of form))
         (the-value-type (type-system-infer-type type-system the-value typing-environment))
         (declared-type (cl-walker::type-of form)))
    (unless (or (subtypep declared-type the-value-type)
                (subtypep the-value-type declared-type))
      (cerror "Continue" "Types not compatible: ~a and ~a when typechecking: ~a" the-value-type declared-type form))
    declared-type))

(defmethod type-system-infer-type ((type-system gradual-type-system)
                                   (form lambda-function-form) typing-environment)
  (let* ((args-type-declarations (remove-if-not (lambda (declare)
                                                  (typep declare 'cl-walker::var-type-declaration-form))
                                                (declares-of form)))
         (arg-types (mapcar (lambda (arg)
                              (cons (name-of arg)
                                    (let ((declared-type (aand
                                                          (find (name-of arg)
                                                                args-type-declarations
                                                                :key #'name-of)
                                                          (cl-walker::type-of it)))
                                          (lambda-list-type (cl-walker::type-spec arg)))
                                      (when (and (and declared-type lambda-list-type)
                                                 (not (equalp declared-type lambda-list-type)))
                                        (error "Duplicate type declaration for ~A" (name-of arg)))
                                      (or declared-type lambda-list-type
                                          (and (typep arg 'cl-walker::optional-function-argument-form)
                                               (aand
                                                (default-value-of arg)
                                                (type-system-infer-type type-system it typing-environment)))
                                          t))))
                            (arguments-of form)))
         (return-type (let ((return-type-declaration
                              (remove-if-not (lambda (declare)
                                               (typep declare 'cl-walker::return-type-declaration-form))
                                             (declares-of form))))
                        (if return-type-declaration
                            (cl-walker::type-of return-type-declaration)
                                        ; else
                            (let ((fresh-typing-environment (copy-typing-environment typing-environment)))
                              (loop for (arg . type) in  arg-types
                                    do (setf (type-of-var arg fresh-typing-environment) type))
                              (type-system-infer-type type-system
                                                      (car (last (body-of form)))
                                                      fresh-typing-environment))))))
    (make-function-type :required-args-types (mapcar #'cdr arg-types)
                        :return-type return-type)))
