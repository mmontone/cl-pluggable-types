(in-package :gradual)

(defun infer-type (form)
  "Infer the type of FORM under a fresh TYPING-ENVIRONMENT."
  (%infer-type (if (typep form 'cl-walker:walked-form)
                   form
                   (cl-walker:walk-form form))
               (make-typing-environment)))

(defgeneric %infer-type (form typing-environment)
  (:documentation "Infer the type of FORM under TYPING-ENVIRONMENT."))

(defmethod %infer-type ((form free-variable-reference-form) typing-env)
  (var-type (name-of form)))

(defmethod %infer-type ((form special-variable-reference-form) typing-env)
  (var-type (name-of form)))

(defmethod %infer-type ((form constant-form) typing-environment)
  (type-of (value-of form)))

(defmethod %infer-type ((form free-application-form) typing-environment)
  (cond
    ;; test for: (make-instance 'foo) 
    ((and (eql (operator-of form) 'make-instance)
          (typep (first (arguments-of form)) 'cl-walker:constant-form))
     (value-of (first (arguments-of form))))
    ;; otherwise, use function type information
    (t
     (let ((function-type (fun-type (operator-of form)))
           (args-types (mapcar #'infer-type (arguments-of form))))
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

(defmethod %infer-type ((form if-form) typing-env)
  (canonize-type
   `(or
     ,(%infer-type (then-of form) typing-env)
     ,(%infer-type (else-of form) typing-env))))

(defmethod %infer-type ((form let-form) typing-environment)
  (let ((fresh-typing-environment typing-environment))
    (loop for binding in (bindings-of form)
          do
             (setf fresh-typing-environment
                   (set-env-var-type fresh-typing-environment
                                     (name-of binding)
                                     (if (not (cl-walker::type-spec binding))
                                         (%infer-type (value-of binding) typing-environment)
                                         (cl-walker::type-spec binding)))))
    (%infer-type (car (last (body-of form))) fresh-typing-environment)))

(defmethod %infer-type ((form walked-lexical-variable-reference-form) typing-environment)
  (env-var-type typing-environment (name-of form)))

(defmethod %infer-type ((form the-form) typing-environment)
  (let* ((the-value (value-of form))
         (the-value-type (%infer-type the-value typing-environment))
         (declared-type (cl-walker::type-of form)))
    (unless (or (subtypep declared-type the-value-type)
                (subtypep the-value-type declared-type))
      (cerror "Continue" "Types not compatible: ~a and ~a when typechecking: ~a" the-value-type declared-type form))
    declared-type))

(defmethod %infer-type ((form lambda-function-form) typing-environment)
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
                                                (%infer-type it typing-environment)))
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
                                    do (setf fresh-typing-environment (set-env-var-type fresh-typing-environment arg type)))
                              (%infer-type (car (last (body-of form)))
                                           fresh-typing-environment))))))
    (make-function-type :required-args-types (mapcar #'cdr arg-types)
                        :return-type return-type)))
