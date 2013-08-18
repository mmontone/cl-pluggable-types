;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

;;;; Atoms

(defclass constant-form (walked-form)
  ((value :accessor value-of :initarg :value)))

(defunwalker-handler constant-form (value)
  (if (or (eq value t)
          (eq value nil)
          (keywordp value))
      value
      (typecase value
        (symbol `(quote ,value))
        (cons   `(quote ,value))
        (t value))))

(defprint-object constant-form
  (format t "!~S" (value-of -self-)))

(defclass variable-reference-form (walked-form)
  ((name :accessor name-of :initarg :name)))

(defunwalker-handler variable-reference-form (name)
  name)

(defprint-object variable-reference-form
  (format t "!~S" (name-of -self-)))

(defclass lexical-variable-reference-form (variable-reference-form)
  ())

(defclass walked-lexical-variable-reference-form (lexical-variable-reference-form)
  ((binding :accessor binding-of :initarg :binding))
  (:documentation "A reference to a local variable defined in the lexical environment inside the form passed to walk-form."))

(defclass unwalked-lexical-variable-reference-form (lexical-variable-reference-form)
  ()
  (:documentation "A reference to a local variable defined in the lexical environment outside of the form passed to walk-form."))

(defclass special-variable-reference-form (variable-reference-form)
  ())

(defclass free-variable-reference-form (special-variable-reference-form)
  ())

(defwalker-handler +atom-marker+ (form parent env)
  (let ((lexenv (cdr env)))
    (cond
      ((constant-name? form)
       (make-form-object 'constant-form parent :value form))
      ((lookup-in-walkenv :symbol-macro form env)
       (let ((*inside-macroexpansion* t))
         (walk-form (lookup-in-walkenv :symbol-macro form env) parent env)))
      ((symbol-macro-name? form lexenv)
       (walk-form (walker-macroexpand-1 form lexenv) parent env))
      ((or (special-variable-name? form lexenv)
           (loop
              :for node = parent :then (parent-of node)
              :while node
              :do (progn
                    (when (and (typep node 'implicit-progn-with-declare-mixin)
                               (find-by-name form (declares-of node)
                                             :type 'special-variable-declaration-form))
                      (return t)))))
       (make-form-object 'special-variable-reference-form parent :name form))
      ((lookup-in-walkenv :variable form env)
       (make-form-object 'walked-lexical-variable-reference-form parent :name form
                         :binding (lookup-in-walkenv :variable form env)))
      ((lookup-in-walkenv :unwalked-variable form env)
       (make-form-object 'unwalked-lexical-variable-reference-form parent :name form))
      (t
       (undefined-reference :variable form)
       (make-form-object 'free-variable-reference-form parent :name form)))))

;;;; BLOCK/RETURN-FROM

(defclass block-form (walked-form implicit-progn-mixin binding-entry-mixin)
  ())

(defwalker-handler block (form parent env)
  (destructuring-bind (block-name &rest body)
      (cdr form)
    (with-form-object (block 'block-form parent
                             :name block-name)
      (walk-implict-progn block
                          body
                          (augment-walkenv env :block block-name block)))))

(defunwalker-handler block-form (name body)
  `(block ,name ,@(unwalk-forms body)))

(defclass return-from-form (walked-form)
  ((target-block :initform nil :accessor target-block-of :initarg :target-block)
   (result :accessor result-of :initarg :result)))

(define-condition return-from-unknown-block (walker-error)
  ((block-name :accessor block-name :initarg :block-name))
  (:report (lambda (condition stream)
             (format stream "Unable to return from block named ~S." (block-name condition)))))

(defun walk-return (block-name value form parent env)
  (if (lookup-in-walkenv :block block-name env)
      (with-form-object (return-from 'return-from-form parent
                                     :target-block (lookup-in-walkenv :block block-name env))
        (setf (result-of return-from) (walk-form value return-from env)))
      (restart-case
          (error 'return-from-unknown-block :block-name block-name)
        (add-block ()
          :report "Add this block and continue."
          (walk-form form parent (augment-walkenv env :block block-name :unknown-block))))))

(defwalker-handler return-from (form parent env)
  (destructuring-bind (block-name &optional (value '(values)))
      (cdr form)
    (walk-return block-name value form parent env)))

(defunwalker-handler return-from-form (target-block result)
  (let* ((unwalked-result (unwalk-form result))
         (result-form (if (equal unwalked-result '(values))
                          '()
                          (list unwalked-result))))
    (if (null (name-of target-block))
        `(return ,@result-form)
        `(return-from ,(name-of target-block) ,@result-form))))

;;;; CATCH/THROW

(defclass catch-form (walked-form implicit-progn-mixin)
  ((tag :accessor tag-of :initarg :tag)))

(defwalker-handler catch (form parent env)
  (destructuring-bind (tag &body body)
      (cdr form)
    (with-form-object (catch 'catch-form parent)
      (setf (tag-of catch) (walk-form tag catch env))
      (walk-implict-progn catch body env))))

(defunwalker-handler catch-form (tag body)
  `(catch ,(unwalk-form tag) ,@(unwalk-forms body)))

(defclass throw-form (walked-form)
  ((tag :accessor tag-of :initarg :tag)
   (value :accessor value-of :initarg :value)))

(defwalker-handler throw (form parent env)
  (destructuring-bind (tag &optional (result '(values)))
      (cdr form)
    (with-form-object (throw 'throw-form parent)
      (setf (tag-of throw) (walk-form tag throw env)
            (value-of throw) (walk-form result throw env)))))

(defunwalker-handler throw-form (tag value)
  `(throw ,(unwalk-form tag) ,(unwalk-form value)))

;;;; EVAL-WHEN

(defclass eval-when-form (walked-form implicit-progn-mixin)
  ((eval-when-times :accessor eval-when-times :initarg :eval-when-times)))

(defwalker-handler eval-when (form parent env)
  (destructuring-bind (times &body body)
      (cdr form)
    (with-form-object (eval-when 'eval-when-form parent)
      (setf (eval-when-times eval-when) times)
      (walk-implict-progn eval-when body env))))

(defunwalker-handler eval-when-form (body eval-when-times)
  `(eval-when ,eval-when-times
     ,@(unwalk-forms body)))

;;;; IF

(defclass if-form (walked-form)
  ((condition :accessor condition-of :initarg :condition)
   (then :accessor then-of :initarg :then)
   (else :accessor else-of :initarg :else)))

(defwalker-handler if (form parent env)
  (with-form-object (if 'if-form parent)
    (setf (condition-of if) (walk-form (second form) if env)
          (then-of if) (walk-form (third form) if env)
          (else-of if) (walk-form (fourth form) if env))))

(defunwalker-handler if-form (condition then else)
  `(if ,(unwalk-form condition) ,(unwalk-form then) ,(unwalk-form else)))

;;;; LET/LET*

(defclass variable-binding-form (walked-form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defclass let-form (variable-binding-form)
  ())

(defun parse-var-binding (binding parent env)
  (with-current-form binding
    (destructuring-bind (var &optional (initial-value nil init-set-p))
        (ensure-list binding)
      (with-form-object (bind 'variable-binding-entry-form parent
                              :name var :value nil)
        (when init-set-p
          (setf (value-of bind) (walk-form initial-value bind env)))
        bind))))

(defun add-var-binding-to-env (env bind declarations)
  (let ((var (name-of bind)))
    (if (or (special-variable-name? var (cdr env))
            (find-by-name var declarations
                          :type 'special-variable-declaration-form))
        (prog1 env
          (setf (special-binding? bind) t))
        (augment-walkenv env :variable var bind))))

(defwalker-handler let (form parent env)
  (with-form-object (let 'let-form parent)
    (setf (bindings-of let) (mapcar (lambda (binding)
                                      (parse-var-binding binding let env))
                                    (remove-if #'null (second form))))
    (multiple-value-bind (b e d declarations)
        (split-body (cddr form) env :parent let :declare t)
      (declare (ignore b e d))
      (let ((env (reduce (lambda (env bind)
                            (add-var-binding-to-env env bind declarations))
                         (bindings-of let) :initial-value env)))
        (walk-implict-progn let (cddr form) env :declare t)))))

(defunwalker-handler let-form (bindings body declares)
  `(let ,(mapcar #'unwalk-form bindings)
     ,@(unwalk-declarations declares)
     ,@(unwalk-forms body)))

(defclass let*-form (variable-binding-form)
  ())

(defwalker-handler let* (form parent env)
  (with-form-object (let* 'let*-form parent
                          :bindings '())
    (multiple-value-bind (b e d declarations)
        (split-body (cddr form) env :parent let* :declare t)
      (declare (ignore b e d))
      (dolist (binding (remove-if #'null (second form)))
        (let ((bind (parse-var-binding binding let* env)))
          (push bind (bindings-of let*))
          (setf env (add-var-binding-to-env env bind declarations))))
      (setf (bindings-of let*) (nreverse (bindings-of let*)))
      (walk-implict-progn let* (cddr form) env :declare t))))

(defunwalker-handler let*-form (bindings body declares)
  `(let* ,(mapcar #'unwalk-form bindings)
     ,@(unwalk-declarations declares)
     ,@(unwalk-forms body)))

;;;; LOCALLY

(defclass locally-form (walked-form implicit-progn-with-declare-mixin)
  ())

(defwalker-handler locally (form parent env)
  (with-form-object (locally 'locally-form parent)
    (walk-implict-progn locally (cdr form) env :declare t)))

(defunwalker-handler locally-form (body declares)
  `(locally ,@(unwalk-declarations declares)
     ,@(unwalk-forms body)))

;;;; MACROLET

(defclass macrolet-form (walked-form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defwalker-handler macrolet (form parent env)
  ;; TODO is there any point in constructing a macrolet form if we macroexpand the body anyways?
  (with-form-object (macrolet 'macrolet-form parent
                              :bindings '())
    (dolist* ((name args &body body) (second form))
      (let ((handler (parse-macro-definition name args body (cdr env))))
        (augment-walkenv! env :macro name handler)
        (push (make-form-object 'macro-binding-entry-form
                                macrolet
                                :name name :value handler)
              (bindings-of macrolet))))
    (setf (bindings-of macrolet) (nreverse (bindings-of macrolet)))
    (walk-implict-progn macrolet (cddr form) env :declare t)))

(defunwalker-handler macrolet-form (body bindings declares)
  ;; We ignore the bindings, because the expansion has already taken
  ;; place at walk-time.
  (declare (ignore bindings))
  `(locally ,@(unwalk-declarations declares) ,@(unwalk-forms body)))

;;;; MULTIPLE-VALUE-CALL

(defclass multiple-value-call-form (walked-form)
  ((function-designator :accessor function-designator-of :initarg :function-designator)
   (arguments :accessor arguments-of :initarg :arguments)))

(defwalker-handler multiple-value-call (form parent env)
  (with-form-object (m-v-c 'multiple-value-call-form parent)
    (setf (function-designator-of m-v-c) (walk-form (second form) m-v-c env)
          (arguments-of m-v-c) (mapcar (lambda (f) (walk-form f m-v-c env))
                                       (cddr form)))))

(defunwalker-handler multiple-value-call-form (function-designator arguments)
  `(multiple-value-call ,(unwalk-form function-designator) ,@(unwalk-forms arguments)))

;;;; MULTIPLE-VALUE-PROG1

(defclass multiple-value-prog1-form (walked-form)
  ((first-form :accessor first-form-of :initarg :first-form)
   (other-forms :accessor other-forms-of :initarg :other-forms)))

(defwalker-handler multiple-value-prog1 (form parent env)
  (with-form-object (m-v-p1 'multiple-value-prog1-form parent)
    (setf (first-form-of m-v-p1) (walk-form (second form) m-v-p1 env)
          (other-forms-of m-v-p1) (mapcar (lambda (f) (walk-form f m-v-p1 env))
                                       (cddr form)))))

(defunwalker-handler multiple-value-prog1-form (first-form other-forms)
  `(multiple-value-prog1 ,(unwalk-form first-form) ,@(unwalk-forms other-forms)))

;;;; PROGN

(defclass progn-form (walked-form implicit-progn-mixin)
  ())

(defwalker-handler progn (form parent env)
  (with-form-object (progn 'progn-form parent)
    (walk-implict-progn progn (cdr form) env)))

(defunwalker-handler progn-form (body)
  `(progn ,@(unwalk-forms body)))

(defprint-object progn-form
  (let ((body (body-of -self-)))
    (pprint-logical-block (*standard-output* body :prefix "(" :suffix ")")
      (princ "progn")
      (pprint-indent :block 1)
      (pprint-newline :mandatory)
      (pprint-exit-if-list-exhausted)
      (loop
         :with first? = t
         :for el = (pprint-pop)
         :do
         (unless first?
           (pprint-newline :mandatory))
         (princ el)
         (pprint-exit-if-list-exhausted)
         (setf first? nil)))))

;;;; PROGV

(defclass progv-form (walked-form implicit-progn-mixin)
  ((variables-form :accessor variables-form-of :initarg :variables-form)
   (values-form :accessor values-form-of :initarg :values-form)))

(defwalker-handler progv (form parent env)
  (with-form-object (progv 'progv-form parent)
    (setf (variables-form-of progv) (walk-form (cadr form) progv env))
    (setf (values-form-of progv) (walk-form (caddr form) progv env))
    (walk-implict-progn progv (cdddr form) env)
    progv))

(defunwalker-handler progv-form (body variables-form values-form)
  `(progv ,(unwalk-form variables-form) ,(unwalk-form values-form) ,@(unwalk-forms body)))

;;;; QUOTE

(defwalker-handler quote (form parent env)
  (make-form-object 'constant-form parent
                    :value (second form)))

;;;; SETQ

(defclass setq-form (walked-form)
  ((variable
    :accessor variable-of
    :initarg :variable)
   (value
    :accessor value-of
    :initarg :value)))

(defwalker-handler setq (form parent env)
  ;; the SETQ handler needs to be able to deal with symbol-macrolets
  ;; which haven't yet been expanded and may expand into something
  ;; requiring setf and not setq.
  (let ((effective-code '()))
    (loop
       :for (name value) :on (cdr form) :by #'cddr
       :do (push (aif (lookup-in-walkenv :symbol-macro name env)
                      `(setf ,it ,value)
                      `(setq ,name ,value))
                 effective-code))
    (if (= 1 (length effective-code))
        ;; only one form, the "simple case"
        (destructuring-bind (type variable value)
            (first effective-code)
          (ecase type
            (setq (with-form-object (setq 'setq-form parent)
                    (setf (variable-of setq) (walk-form variable setq env))
                    (setf (value-of setq) (walk-form value setq env))))
            (setf (walk-form (first effective-code) parent env))))
        ;; multiple forms
        (with-form-object (progn 'progn-form parent)
          (walk-implict-progn progn effective-code env)))))

(defunwalker-handler setq-form (variable value)
  `(setq ,(unwalk-form variable) ,(unwalk-form value)))

;;;; SYMBOL-MACROLET

(defclass symbol-macrolet-form (walked-form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defwalker-handler symbol-macrolet (form parent env)
  (with-form-object (symbol-macrolet 'symbol-macrolet-form parent
                                     :bindings '())
    (dolist* ((symbol expansion) (second form))
      (augment-walkenv! env :symbol-macro symbol expansion)
      (push (make-form-object 'symbol-macro-binding-entry-form
                              symbol-macrolet
                              :name symbol :value expansion)
            (bindings-of symbol-macrolet)))
    (setf (bindings-of symbol-macrolet) (nreverse (bindings-of symbol-macrolet)))
    (walk-implict-progn symbol-macrolet (cddr form) env :declare t)))

(defunwalker-handler symbol-macrolet-form (body bindings declares)
  ;; We ignore the bindings, because the expansion has already taken
  ;; place at walk-time.
  (declare (ignore bindings))
  `(locally ,@(unwalk-declarations declares) ,@(unwalk-forms body)))

;;;; TAGBODY/GO

(defclass tagbody-form (walked-form implicit-progn-mixin)
  ())

(defwalker-handler tagbody (form parent env)
  (with-form-object (tagbody 'tagbody-form parent
                             :body (cdr form))
    (augment-walkenv! env :tagbody 'enclosing-tagbody tagbody)
    (flet ((go-tag-p (form)
             (or (symbolp form) (integerp form))))
      ;; the loop below destructuivly modifies the body of tagbody,
      ;; since it's the same object as the source we need to copy it.
      (setf (body-of tagbody) (copy-list (body-of tagbody)))
      (loop
         :for part :on (body-of tagbody)
         :if (go-tag-p (car part))
         :do (progn
               (augment-walkenv! env :tag (car part) part)
               (setf (car part)
                     (with-current-form (car part)
                       (make-form-object 'go-tag-form tagbody
                                         :name (car part))))))
      (loop
         :for part :on (body-of tagbody)
         :unless (typep (car part) 'walked-form)
         :do (setf (car part) (walk-form (car part) tagbody env))))))

(defunwalker-handler tagbody-form (body)
  `(tagbody ,@(unwalk-forms body)))

(defclass go-tag-form (walked-form binding-entry-mixin)
  ())

(defunwalker-handler go-tag-form (name)
  name)

(defclass go-form (walked-form)
  ((jump-target :accessor jump-target-of :initarg :jump-target)
   (jump-tag :accessor jump-tag-of :initarg :jump-tag)
   (name :accessor name-of :initarg :name)
   (enclosing-tagbody :accessor enclosing-tagbody-of :initarg :enclosing-tagbody)))

(defwalker-handler go (form parent env)
  (let ((info (lookup-in-walkenv :tag (second form) env)))
    (make-form-object 'go-form parent
                      :name (second form)
                      :jump-tag (car info)
                      :jump-target (cdr info)
                      :enclosing-tagbody (parent-of (car info)))))

(defunwalker-handler go-form (name)
  `(go ,name))

;;;; THE

(defclass the-form (walked-form)
  ((type :accessor type-of :initarg :type)
   (value :accessor value-of :initarg :value)))

(defwalker-handler the (form parent env)
  (with-form-object (the 'the-form parent
                         :type (second form))
    (setf (value-of the) (walk-form (third form) the env))))

(defunwalker-handler the-form (type value)
  `(the ,type ,(unwalk-form value)))

;;;; UNWIND-PROTECT

(defclass unwind-protect-form (walked-form)
  ((protected-form :accessor protected-form-of :initarg :protected-form)
   (cleanup-form :accessor cleanup-form-of :initarg :cleanup-form)))

(defwalker-handler unwind-protect (form parent env)
  (with-form-object (unwind-protect 'unwind-protect-form parent)
    (setf (protected-form-of unwind-protect) (walk-form (second form) unwind-protect env)
          (cleanup-form-of unwind-protect) (mapcar (lambda (form)
                                                     (walk-form form unwind-protect env))
                                                   (cddr form)))))

(defunwalker-handler unwind-protect-form (protected-form cleanup-form)
  `(unwind-protect ,(unwalk-form protected-form) ,@(unwalk-forms cleanup-form)))

;;;; LOAD-TIME-VALUE

(defclass load-time-value-form (walked-form)
  ((body :accessor body-of :initarg :body)
   (read-only :initform nil :accessor read-only-p :initarg :read-only)
   (value :accessor value-of)))

(defmethod initialize-instance :after ((self load-time-value-form) &key)
  (setf (value-of self) (eval (body-of self))))

(defwalker-handler load-time-value (form parent env)
  (assert (<= (length form) 3))
  (with-form-object (load-time-value 'load-time-value-form parent
                                     :body form
                                     :read-only (third form))
    (setf (body-of load-time-value) (walk-form (second form)))))

(defunwalker-handler load-time-value-form (body read-only)
  `(load-time-value ,(unwalk-form body) ,@(if read-only '(t))))
