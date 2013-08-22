;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

(defclass application-form (walked-form)
  ((operator :accessor operator-of :initarg :operator)
   (arguments :accessor arguments-of :initarg :arguments)))

(defunwalker-handler application-form (operator arguments)
  (cons operator (unwalk-forms arguments)))

(defprint-object application-form
  ;; the bang sign is a weak try... but at least mark it somehow that it's not a normal sexp...
  (princ "!(")
  (princ (operator-of -self-))
  (princ " ")
  (let ((first t))
    (dolist (arg (arguments-of -self-))
      (unless first
        (princ " "))
      (princ arg)
      (setf first nil)))
  (princ ")"))

(defclass lexical-application-form (application-form)
  ((code :accessor code-of :initarg :code)))

(defclass walked-lexical-application-form (lexical-application-form)
  ())

(defclass unwalked-lexical-application-form (lexical-application-form)
  ())

(defclass free-application-form (application-form)
  ())

(defclass lambda-application-form (application-form)
  ())

(defunwalker-handler lambda-application-form (operator arguments)
  ;; The cadr is for getting rid of (function ...) which we can't have
  ;; at the beginning of a form.
  (cons (cadr (unwalk-form operator)) (unwalk-forms arguments)))

(defwalker-handler application (form parent env)
  (block nil
    (destructuring-bind (op &rest args) form
      (when (lambda-form? op)
        (return
          (with-form-object (application 'lambda-application-form parent)
            (setf (operator-of application) (walk-lambda op application env)
                  (arguments-of application) (mapcar (lambda (form)
                                                       (walk-form form application env))
                                                     args)))))
      (let ((lexenv (cdr env)))
        (awhen (lookup-in-walkenv :macro op env)
          (let ((*inside-macroexpansion* t))
            (return (walk-form (funcall it form lexenv) parent env))))
        (when (and (symbolp op)
                   (macro-name? op lexenv))
          (multiple-value-bind (expansion expanded?)
              (walker-macroexpand-1 form lexenv)
            (when expanded?
              (let ((*inside-macroexpansion* t))
                (return (walk-form expansion parent env)))))))
      (let ((app (aif (lookup-in-walkenv :function op env)
                      (make-instance 'walked-lexical-application-form :code it)
                      (if (lookup-in-walkenv :unwalked-function op env)
                          (make-instance 'unwalked-lexical-application-form)
                          (progn
                            (when (and (symbolp op)
                                       (not (function-name? op)))
                              (undefined-reference :function op))
                            (make-instance 'free-application-form))))))
        (setf (operator-of app) op
              (parent-of app) parent
              (source-of app) form
              (arguments-of app) (mapcar (lambda (form)
                                           (walk-form form app env))
                                         args))
        app))))

;;;; Functions

(defclass function-form (walked-form)
  ())

(defclass lambda-function-form (function-form implicit-progn-with-declare-mixin)
  ((arguments :accessor arguments-of :initarg :arguments)))

(defunwalker-handler lambda-function-form (arguments body declares)
  `(function
    (lambda ,(unwalk-lambda-list arguments)
     ,@(unwalk-declarations declares)
     ,@(unwalk-forms body))))

(defclass function-definition-form (lambda-function-form binding-entry-mixin)
  ())

(defun env-with-function (env func)
  (augment-walkenv env :function (name-of func) func))

(defwalker-handler defun (form parent env)
  (with-form-object (node 'function-definition-form parent
                          :name (second form))
    (walk-lambda-like node (third form)
                      (nthcdr 3 form)
                      (env-with-function env node))))

(defunwalker-handler function-definition-form (form name arguments body declares)
  `(defun ,name ,(unwalk-lambda-list arguments) 
     ,@(unwalk-declarations declares)
     ,@(unwalk-forms body)))

(defclass named-lambda-function-form (lambda-function-form binding-entry-mixin)
  ((special-form :accessor special-form-of :initarg :special-form)))

(defunwalker-handler named-lambda-function-form (special-form name arguments body declares)
  `(function
    (,special-form ,name ,(unwalk-lambda-list arguments)
     ,@(unwalk-declarations declares)
     ,@(unwalk-forms body))))

(defclass labels-function-form (lambda-function-form binding-entry-mixin)
  ())

(defunwalker-handler labels-function-form (form name arguments body declares)
  `(,name ,(unwalk-lambda-list arguments)
     ,@(unwalk-declarations declares)
     ,@(unwalk-forms body)))


(defclass function-object-form (walked-form)
  ((name :accessor name-of :initarg :name)))

(defunwalker-handler function-object-form (name)
  `(function ,name))

(defclass lexical-function-object-form (function-object-form)
  ())

(defclass walked-lexical-function-object-form (lexical-function-object-form)
  ((binding :accessor binding-of :initarg :binding)))

(defclass unwalked-lexical-function-object-form (lexical-function-object-form)
  ())

(defclass free-function-object-form (function-object-form)
  ())

(defwalker-handler function (form parent env)
  (cond
    ((lambda-form? (second form))
     ;; (function (lambda ...))
     (walk-lambda (second form) parent env))
    #+sbcl
    ((and (consp (second form))
          (eq (first (second form)) 'sb-int:named-lambda))
     (let ((named-lambda-form (second form)))
       (with-form-object (node 'named-lambda-function-form parent
                               :special-form (first named-lambda-form)
                               :name (second named-lambda-form))
         (walk-lambda-like node (third named-lambda-form)
                           (nthcdr 3 named-lambda-form)
                           (env-with-function env node)))))
    (t
     ;; (function foo)
     (let* ((name    (second form))
            (binding (lookup-in-walkenv :function name env)))
       (if binding
           (make-form-object 'walked-lexical-function-object-form
                             parent :name name :binding binding)
           (make-form-object (if (lookup-in-walkenv :unwalked-function (second form) env)
                                 'unwalked-lexical-function-object-form
                                 'free-function-object-form)
                             parent
                             :name name))))))

(defun walk-lambda (form parent env)
  (with-current-form form
    (with-form-object (ast-node 'lambda-function-form parent)
      (walk-lambda-like ast-node (second form) (cddr form) env))))

(defun %walk-lambda-like (ast-node args body env)
  (multiple-value-setf ((arguments-of ast-node) env)
    (walk-lambda-list args ast-node env))
  (walk-implict-progn ast-node body env :declare t)
  ast-node)

(defun walk-lambda-list (lambda-list parent env &key allow-specializers macro-p)
  (declare (ignore macro-p))
  (let ((result (list)))
    (flet ((extend-env (argument)
             (unless (typep argument 'allow-other-keys-function-argument-form)
               (augment-walkenv! env :variable (name-of argument) argument))))
      (parse-lambda-list lambda-list
                         (lambda (kind name argument &optional type-spec)
                           (declare (ignore name))
                           (let ((parsed
                                  (case kind
                                    ((nil)
                                     (if allow-specializers
                                         (walk-specialized-argument-form argument parent env)
                                         (make-form-object 'required-function-argument-form  parent
                                                           :name argument
							   :type-spec type-spec)))
                                    (&optional
                                     (walk-optional-argument argument parent env))
                                    (&allow-other-keys
                                     (make-form-object 'allow-other-keys-function-argument-form parent))
                                    (&rest (make-form-object 'rest-function-argument-form parent :name argument))
                                    (&key
                                     (walk-keyword-argument argument parent env)))))
                             (when parsed
                               (push parsed result)
                               (extend-env parsed)))))
      (values (nreverse result) env))))

(defclass type-spec-form ()
  ((type-spec :accessor type-spec :initarg :type-spec)))

(defclass function-argument-form (walked-form binding-entry-mixin type-spec-form)
  ())

(defprint-object function-argument-form
  (format t "~S" (name-of -self-)))

(defclass required-function-argument-form (function-argument-form)
  ())

(defunwalker-handler required-function-argument-form (name)
  name)

(defclass specialized-function-argument-form (required-function-argument-form)
  ((specializer :accessor specializer-of :initarg :specializer)))

(defun walk-specialized-argument-form (form parent env)
  (declare (ignore env))
  (make-form-object 'specialized-function-argument-form parent
                    :name (if (listp form)
                              (first form)
                              form)
                    :specializer (if (listp form)
                                     (second form)
                                     t)))

(defunwalker-handler specialized-function-argument-form (name specializer)
  (if (eq specializer t)
      name
      `(,name ,specializer)))

(defclass optional-function-argument-form (function-argument-form)
  ((default-value :initform nil :accessor default-value-of :initarg :default-value)
   (supplied-p-parameter :accessor supplied-p-parameter :initarg :supplied-p-parameter)))

(defun walk-optional-argument (form parent env)
  (destructuring-bind (name &optional (default-value nil default-value-supplied?) supplied-p-parameter)
      (ensure-list form)
    (with-form-object (arg 'optional-function-argument-form parent
                           :name name
                           :supplied-p-parameter supplied-p-parameter)
      (when default-value-supplied?
        (setf (default-value-of arg) (walk-form default-value arg env))))))

(defunwalker-handler optional-function-argument-form (name supplied-p-parameter)
  (let ((default-value (awhen (default-value-of -form-)
                         (unwalk-form it))))
    (cond ((and name supplied-p-parameter)
           `(,name ,default-value ,supplied-p-parameter))
          ((and name default-value)
           `(,name ,default-value))
          (name name)
          (t (error "Invalid optional argument")))))

(defclass keyword-function-argument-form (function-argument-form)
  ((keyword-name :accessor keyword-name-of :initarg :keyword-name)
   (default-value :initform nil :accessor default-value-of :initarg :default-value)
   (supplied-p-parameter :accessor supplied-p-parameter :initarg :supplied-p-parameter)))

(defun effective-keyword-name-of (k)
  (or (keyword-name-of k)
      (intern (symbol-name (name-of k)) :keyword)))

(defun walk-keyword-argument (form parent env)
  (destructuring-bind (name &optional (default-value nil default-value-supplied?) supplied-p-parameter)
      (ensure-list form)
    (let ((name (if (consp name)
                    (second name)
                    name))
          (keyword (if (consp name)
                       (first name)
                       nil)))
      (with-form-object (arg 'keyword-function-argument-form parent
                             :name name
                             :keyword-name keyword
                             :supplied-p-parameter supplied-p-parameter)
        (when default-value-supplied?
          (setf (default-value-of arg) (walk-form default-value arg env)))))))

(defunwalker-handler keyword-function-argument-form (keyword-name name default-value supplied-p-parameter)
  (let ((default-value (awhen (default-value-of -form-)
                         (unwalk-form it))))
    (cond ((and keyword-name name supplied-p-parameter)
           `((,keyword-name ,name) ,default-value ,supplied-p-parameter))
          ((and name supplied-p-parameter)
           `(,name ,default-value ,supplied-p-parameter))
          ((and name default-value)
           `(,name ,default-value))
          (name name)
          (t (error "Invalid keyword argument")))))

(defclass allow-other-keys-function-argument-form (function-argument-form)
  ())

(defunwalker-handler allow-other-keys-function-argument-form ()
  '&allow-other-keys)

(defclass rest-function-argument-form (function-argument-form)
  ())

(defunwalker-handler rest-function-argument-form (name)
  name)

(defun unwalk-lambda-list (arguments)
  (let (optional-p rest-p keyword-p)
    (mapcan #'(lambda (form)
                (append
                 (typecase form
                   (optional-function-argument-form
                    (unless optional-p
                      (assert (not keyword-p))
                      (assert (not rest-p))
                      (setq optional-p t)
                      '(&optional)))
                   (rest-function-argument-form
                    (unless rest-p
                      (assert (not keyword-p))
                      (setq rest-p t)
                      '(&rest)))
                   (keyword-function-argument-form
                    (unless keyword-p
                      (setq keyword-p t)
                      '(&key))))
                 (list (unwalk-form form))))
            arguments)))

;;;; FLET/LABELS

(defclass function-binding-form (walked-form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defclass flet-form (function-binding-form)
  ())

(defwalker-handler flet (form parent env)
  (destructuring-bind (binds &body body)
      (cdr form)
    (with-form-object (flet 'flet-form parent)
      ;; build up the objects for the bindings in the original env
      (loop
         :for binding :in binds
         :for (name args . body) = binding
         :collect (with-current-form binding
                    (with-form-object
                        (lambda-node 'labels-function-form flet :name name)
                      (walk-lambda-like lambda-node args body env)))
         :into bindings
         :finally (setf (bindings-of flet) bindings))
      ;; walk the body in the new env
      (walk-implict-progn flet
                          body
                          (reduce #'env-with-function
                                  (bindings-of flet)
                                  :initial-value env)
                          :declare t))))

;; TODO factor out stuff in flet-form and labels-form
(defunwalker-handler flet-form (bindings body declares)
  `(flet ,(mapcar #'unwalk-form bindings)
     ,@(unwalk-declarations declares)
     ,@(unwalk-forms body)))

(defclass labels-form (function-binding-form)
  ())

(defwalker-handler labels (form parent env)
  (destructuring-bind (binds &body body)
      (cdr form)
    (with-form-object (labels 'labels-form parent)
      ;; we need to walk over the bindings twice. the first pass
      ;; creates some 'empty' lambda objects in the environment so
      ;; that walked-lexical-application-form and walked-lexical-function-object-form
      ;; have something to point to. the second pass then walks the
      ;; actual bodies of the form filling in the previously created
      ;; objects.
      (setf (bindings-of labels)
            (loop
               :for entry :in binds
               :for lambda-node = (with-current-form entry
                               (make-form-object 'labels-function-form
                                                 labels
                                                 :name (car entry)))
               :collect lambda-node
               :do (setf env (env-with-function env lambda-node))))
      (loop
         :for (nil args . body) :in binds
         :for lambda-node :in (bindings-of labels)
         :do (walk-lambda-like lambda-node args body env))
      (walk-implict-progn labels body env :declare t))))

(defunwalker-handler labels-form (bindings body declares)
  `(labels ,(mapcar #'unwalk-form bindings)
     ,@(unwalk-declarations declares)
     ,@(unwalk-forms body)))

