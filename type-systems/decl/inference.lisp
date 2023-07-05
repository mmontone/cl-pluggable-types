(in-package :pluggable-types/decl)

(defmethod pluggable-types::infer-type (form (type-system (eql 'decl-type-system)) &rest args)
  "Infer the type of FORM under a fresh TYPING-ENVIRONMENT."
  (let* ((type-system (apply #'make-instance type-system args))
         (env (pluggable-types::make-typing-environment type-system)))
    (values
     (pluggable-types::type-system-infer-type
      type-system
      (if (typep form 'cl-walker:walked-form)
          form
          (cl-walker:walk-form form))
      env)
     env
     type-system)))

(defmethod pluggable-types::type-system-infer-type ((type-system decl-type-system)
                                                    (form free-variable-reference-form) typing-env)
  (pluggable-types::var-type (name-of form)))

(defmethod pluggable-types::type-system-infer-type ((type-system decl-type-system)
                                                    (form special-variable-reference-form) typing-env)
  (pluggable-types::var-type (name-of form)))

(defmethod pluggable-types::type-system-infer-type ((type-system decl-type-system)
                                                    (form constant-form) typing-environment)
  (type-of (value-of form)))

(defmethod pluggable-types::type-system-infer-type ((type-system decl-type-system)
                                                    (form free-application-form) typing-environment)
  (cond
    ;; test for: (make-instance 'foo)
    ((and (eql (operator-of form) 'make-instance)
          (typep (first (arguments-of form)) 'cl-walker:constant-form))
     (value-of (first (arguments-of form))))
    ;; test for: (apply 'something &rest args) or (apply #'something &rest args)
    ((and (eql (operator-of form) 'apply)
          (typep (first (arguments-of form)) '(or cl-walker:constant-form
                                               cl-walker:free-function-object-form)))
     ;; TODO:
     'function)
    ;; otherwise, use function type information
    (t
     (let ((function-type (pluggable-types::fun-type (operator-of form)))
           (args-types (mapcar (lambda (arg)
                                 (pluggable-types::type-system-infer-type type-system arg typing-environment))
                               (arguments-of form))))
       (if function-type
           t ;;(return-type function-type)
           t)))))

(defmethod pluggable-types::type-system-infer-type ((type-system decl-type-system)
                                                    (form free-function-object-form)
                                                    env)
  (or (pluggable-types::fun-type (name-of form)) 'function))

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

(defmethod pluggable-types::type-system-infer-type ((type-system decl-type-system)
                                                    (form if-form) typing-env)
  (canonize-type
   `(or
     ,(pluggable-types::type-system-infer-type type-system (then-of form) typing-env)
     ,(pluggable-types::type-system-infer-type type-system (else-of form) typing-env))))

(defmethod pluggable-types::type-system-infer-type ((type-system decl-type-system)
                                                    (form let-form) typing-environment)
  (let ((fresh-typing-environment typing-environment))
    (loop for binding in (bindings-of form)
          do
             (setf fresh-typing-environment
                   (setf (type-of-var (name-of binding) fresh-typing-environment)
                         (if (not (cl-walker::type-spec binding))
                             (pluggable-types::type-system-infer-type type-system (value-of binding) typing-environment)
                             (cl-walker::type-spec binding)))))
    (pluggable-types::type-system-infer-type type-system (car (last (body-of form))) fresh-typing-environment)))

(defmethod pluggable-types::type-system-infer-type ((type-system decl-type-system)
                                                    (form walked-lexical-variable-reference-form) typing-environment)
  (pluggable-types::type-of-var (name-of form) typing-environment))

(defmethod pluggable-types::type-system-infer-type ((type-system decl-type-system)
                                                    (form the-form) typing-environment)
  (let* ((the-value (value-of form))
         (the-value-type (pluggable-types::type-system-infer-type type-system the-value typing-environment))
         (declared-type (cl-walker::type-of form)))
    (unless (or (subtypep declared-type the-value-type)
                (subtypep the-value-type declared-type))
      (cerror "Continue" "Types not compatible: ~a and ~a when typechecking: ~a" the-value-type declared-type form))
    (setf (pluggable-types::declared-type form typing-environment) declared-type)
    declared-type))

(defmethod pluggable-types::type-system-infer-type ((type-system decl-type-system)
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
                                                (pluggable-types::type-system-infer-type type-system it typing-environment)))
                                          t))))
                            (arguments-of form)))
         (return-type (let ((return-type-declaration
                              (remove-if-not (lambda (declare)
                                               (typep declare 'cl-walker::return-type-declaration-form))
                                             (declares-of form))))
                        (if return-type-declaration
                            (cl-walker::type-of return-type-declaration)
                            ;; else
                            (let ((fresh-typing-environment (copy-typing-environment typing-environment)))
                              (loop for (arg . type) in  arg-types
                                    do (setf (type-of-var arg fresh-typing-environment) type))
                              (pluggable-types::type-system-infer-type type-system
                                                                       (car (last (body-of form)))
                                                                       fresh-typing-environment))))))
    (make-function-type :required-args-types (mapcar #'cdr arg-types)
                        :return-type return-type)))

(defmethod pluggable-types::type-system-infer-type ((type-system decl-type-system)
                                                    (form progn-form)
                                                    env)
  (lastcar
   (mapcar (lambda (f)
             (pluggable-types::type-system-infer-type type-system f env))
           (body-of form))))

(declaim (ftype (function (walked-form (function (walked-form) (values))) (values))
                deep-walk-code))
(defgeneric deep-walk-code (walked-form handler)
  (:documentation "Walk across code."))

(defmethod deep-walk-code ((form implicit-progn-mixin) handler)
  (funcall handler form)
  (dolist (subform (body-of form))
    (deep-walk-code subform handler)))

(defmethod deep-walk-code ((form let-form) handler)
  (funcall handler form)
  (dolist (binding (bindings-of form))
    (deep-walk-code (value-of binding) handler))
  (dolist (subform (body-of form))
    (deep-walk-code subform handler)))

(defmethod deep-walk-code ((form application-form) handler)
  (funcall handler form)
  (dolist (arg (arguments-of form))
    (deep-walk-code arg handler)))

(defmethod deep-walk-code ((form walked-form) handler)
  (funcall handler form))

(define-condition return-from-form-inferred ()
  ((inferred-type :initarg :inferred-type
                  :accessor return-from-inferred-type)))

(defmethod pluggable-types::type-system-infer-type ((type-system decl-type-system)
                                                    (form return-from-form)
                                                    env)
  (let ((type (pluggable-types::type-system-infer-type type-system (result-of form) env)))
    (signal 'return-from-form-inferred :inferred-type type)
    type))

(defmethod pluggable-types::type-system-infer-type ((type-system decl-type-system)
                                                    (form block-form)
                                                    env)
  "The OR of the types of RETURN-FROMs and the type of the BODY."
  (let ((return-from-types nil))
    ;; Call infer recursively and setup a handler for return-from forms.
    ;; The handler stores the inferred type and continues.
    (handler-bind ((return-from-form-inferred
                     (lambda (c)
                       (push (return-from-inferred-type c) return-from-types))))
      (let ((body-type (lastcar (mapcar (lambda (f)
                                          (pluggable-types::type-system-infer-type type-system f env))
                                        (body-of form)))))
        (canonize-type
         `(or ,@return-from-types ,body-type))))))


(defun assign-types-from-function-type (function-type args &key (arg-name-accessor #'identity))
  (assert (eql (first function-type) 'function))
  (destructuring-bind (_ arg-types return-type) function-type
    (declare (ignore _ return-type))
    (let ((lambda-section '&required)
          (assignments)
          (args-queue args))
      (dolist (arg-type arg-types)
        (block nil
          (when (member arg-type '(&optional &key &rest &aux))
            (setf lambda-section arg-type)
            (return))
          (case lambda-section
            (&required
             (let ((arg (pop args-queue)))
               (when (null arg)
                 (error "Not enough args"))
             (push (cons arg arg-type) assignments)))
            (&optional
             (let ((arg (pop args-queue)))
               (when (null arg)
                 (return))
               (push (cons arg arg-type) assignments)))
            (&key
             (destructuring-bind (key type) arg-type
               (let ((arg-val (getf args-queue key)))
                 (when arg-val
                   (push (cons arg-val type) assignments)))
               (alexandria:remove-from-plistf args-queue key)))
            (&rest
             ;; Consume all the passed args
             (dolist (arg args-queue)
               (push (cons arg arg-type)
                     assignments))
             (setf args-queue nil)))))
      (unless (null args-queue)
        (if (eql lambda-section '&key)
            (error "Invalid key arguments in: ~s" args-queue)
            (error "Too many arguments")))
      (nreverse assignments))))

(assign-types-from-function-type '(function () t) '())
(assign-types-from-function-type '(function (number) t) '(x))
(assign-types-from-function-type '(function (string &optional number) t)
                                 '(x))
(assign-types-from-function-type '(function (string &optional number) t)
                                 '())

(assign-types-from-function-type '(function (string &optional number) t)
                                 '(x y))

(assign-types-from-function-type '(function (string &optional number) t)
                                 '(x y z))

(assign-types-from-function-type '(function (string &key (:y number)) t)
                                 '(x))

(assign-types-from-function-type '(function (string &key (:y number)) t)
                                 '(x :y y))

(assign-types-from-function-type '(function (string &key (:y number)) t)
                                 '(x :y y :z "lala"))

(assign-types-from-function-type '(function (string &key (:y number)) t)
                                 '(x :y y :z "lala"))

(assign-types-from-function-type '(function (&rest number) t)
                                 '(x y z))
