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
  (let ((function-type (fun-type (operator-of form)))
        (args-types (mapcar #'infer-type (arguments-of form))))
    (return-type function-type)))

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
  (cl-walker::type-of form))

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
