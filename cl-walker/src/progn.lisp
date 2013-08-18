;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

(defclass implicit-progn-mixin ()
  ((body :accessor body-of :initarg :body)))

(defprint-object implicit-progn-mixin
  (format t "~A" (body-of -self-)))

(defclass implicit-progn-with-declare-mixin (implicit-progn-mixin)
  ((declares :initform nil :accessor declares-of :initarg :declares)))

(defclass binding-form-mixin ()
  ((bindings :accessor bindings-of :initarg :bindings)))


(defun find-by-name (name list &key (type 't))
  (find-if (lambda (item)
             (and item
                  (or (eql type t)
                      (typep item type))
                  (eql (name-of item) name)))
           list))

(defclass binding-entry-mixin ()
  ((name :accessor name-of :initarg :name)
   (usages :accessor usages-of :initarg :usages)))

(defclass variable-binding-entry-form (walked-form binding-entry-mixin)
  ((value :accessor value-of :initarg :value)
   (specialp :accessor special-binding? :initform nil)))

(defunwalker-handler variable-binding-entry-form (name value)
  (if value
      `(,name ,(unwalk-form value))
      name))

;; Macros are expanded right away, so no reference links.
;; Used only so that binding-of always contains a list
;; of binding-entry-mixin objects.
(defclass macro-binding-entry-form (walked-form binding-entry-mixin)
  ((value :accessor value-of :initarg :value)))

(defclass symbol-macro-binding-entry-form (walked-form binding-entry-mixin)
  ((value :accessor value-of :initarg :value)))


(defclass declaration-form (walked-form)
  ())

(defclass optimize-declaration-form (declaration-form)
  ((specification :accessor specification-of :initarg :specification)))

(defunwalker-handler optimize-declaration-form (specification)
  `(optimize ,specification))

(defclass variable-declaration-form (declaration-form)
  ((name :accessor name-of :initarg :name)))

(defclass function-declaration-form (declaration-form)
  ((name :accessor name-of :initarg :name)))

(defclass dynamic-extent-declaration-form (variable-declaration-form)
  ())

(defunwalker-handler dynamic-extent-declaration-form (name)
  `(dynamic-extent ,name))

(defclass ignorable-declaration-form-mixin (declaration-form)
  ())

(defclass variable-ignorable-declaration-form (variable-declaration-form ignorable-declaration-form-mixin)
  ())

(defunwalker-handler variable-ignorable-declaration-form (name)
  `(ignorable ,name))

(defclass function-ignorable-declaration-form (function-declaration-form ignorable-declaration-form-mixin)
  ())

(defunwalker-handler function-ignorable-declaration-form (name)
  `(ignorable (function ,name)))

(defclass special-variable-declaration-form (variable-declaration-form)
  ())

(defunwalker-handler special-variable-declaration-form (name)
  `(special ,name))

(defclass type-declaration-form (variable-declaration-form)
  ((type :accessor type-of :initarg :type)))

(defunwalker-handler type-declaration-form (type name)
  `(type ,type ,name))

(defclass ftype-declaration-form (function-declaration-form)
  ((type :accessor type-of :initarg :type)))

(defunwalker-handler ftype-declaration-form (type name)
  `(ftype ,type ,name))

(defclass notinline-declaration-form (function-declaration-form)
  ())

(defunwalker-handler notinline-declaration-form (name)
  `(notinline ,name))

(defclass unknown-declaration-form (declaration-form)
  ((declaration-form :initarg :declaration-form :accessor declaration-form-of)))

(defunwalker-handler unknown-declaration-form (declaration-form)
  declaration-form)

(defvar *known-declaration-types* (append
                                   #+sbcl
                                   '(sb-ext:muffle-conditions
                                     )
                                   ))

(defun walk-declaration (declaration environment parent)
  (let ((declares nil))
    (flet ((function-name (form)
             (if (and (consp form)
                      (eql (car form) 'function))
                 (second form)
                 nil)))
      (macrolet ((make-declaration (formclass &rest rest)
                   `(make-form-object ,formclass parent ,@rest))
                 (extend-env ((var list) newdeclare &rest datum)
                   `(dolist (,var ,list)
                      (push ,newdeclare declares)
                      (augment-walkenv! environment :declare ,@datum))))
        (destructuring-bind (type &rest arguments)
            declaration
          (case type
            (dynamic-extent
             (extend-env (var arguments)
                         (make-declaration 'dynamic-extent-declaration-form :name var)
                         var `(dynamic-extent)))
            (ftype
             (extend-env (function-name (cdr arguments))
                         (make-form-object 'ftype-declaration-form parent
                                           :name function-name
                                           :type (first arguments))
                         function-name `(ftype ,(first arguments))))
            ((ignore ignorable)
             (extend-env (var arguments)
                         (aif (function-name var)
                              (make-declaration 'function-ignorable-declaration-form :name it)
                              (make-declaration 'variable-ignorable-declaration-form :name var))
                         var `(,type)))
            (inline
              (extend-env (function arguments)
                          (make-declaration 'function-ignorable-declaration-form :name function)
                          function `(inline)))
            (notinline
             (extend-env (function arguments)
                         (make-declaration 'notinline-declaration-form :name function)
                         function `(notinline)))
            (optimize
             (extend-env (optimize-spec arguments)
                         (make-declaration 'optimize-declaration-form :specification optimize-spec)
                         'optimize optimize-spec))
            (special
             (extend-env (var arguments)
                         (make-declaration 'special-variable-declaration-form :name var)
                         var `(special)))
            (type
             (extend-env (var (rest arguments))
                         (make-form-object 'type-declaration-form parent
                                           :name var
                                           :type (first arguments))
                         var `(type ,(first arguments))))
            (t
             (unless (member type *known-declaration-types* :test #'eq)
               (simple-style-warning "Ignoring unknown declaration ~S while walking forms. If it's a type declaration, then use the full form to avoid this warning: `(type ,type ,@variables), or you can also (pushnew ~S ~S)."
                                     declaration type '*known-declaration-types*))
             (push (make-form-object 'unknown-declaration-form parent
                                     :declaration-form declaration)
                   declares))))))
    (values environment declares)))

(defun unwalk-declarations (decls)
  ;; Return a list so declarations can be easily spliced.
  (if (null decls)
      nil
      (list `(declare ,@(unwalk-forms decls)))))

(defun walk-implict-progn (parent forms env &key docstring declare)
  (assert (and (typep parent 'implicit-progn-mixin)
               (or (not declare)
                   (typep parent 'implicit-progn-with-declare-mixin))))
  (handler-bind ((undefined-reference
                  (lambda (condition)
                    (unless (enclosing-code-of condition)
                      (setf (enclosing-code-of condition) `(some-implicit-progn-form ,@forms))))))
    (multiple-value-bind (body env docstring declarations)
        (split-body forms env :parent parent :docstring docstring :declare declare)
      (when declare
        (setf (declares-of parent) declarations))
      (setf (body-of parent) (mapcar (lambda (form)
                                       (walk-form form parent env))
                                     body))
      docstring)))

