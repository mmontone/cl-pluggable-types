;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(cl:in-package :common-lisp-user)

(defpackage :cl-walker
  (:documentation "A code walker for Common Lisp")

  (:use :common-lisp :alexandria)

  (:shadow
   #:type-of
   #:eval
   )

  (:export

   ;;;
   ;;; environment
   ;;;
   #:make-empty-lexical-environment
   #:lookup-in-lexenv
   #:macroexpand-all
   #:with-walker-configuration

   #:do-variables-in-lexenv
   #:do-functions-in-lexenv
   #:do-macros-in-lexenv
   #:do-symbol-macros-in-lexenv
   #:do-blocks-in-lexenv
   #:do-tags-in-lexenv

   #:iterate-variables-in-lexenv
   #:iterate-functions-in-lexenv
   #:iterate-macros-in-lexenv
   #:iterate-symbol-macros-in-lexenv
   #:iterate-blocks-in-lexenv
   #:iterate-tags-in-lexenv

   #:collect-variables-in-lexenv
   #:collect-functions-in-lexenv
   #:collect-macros-in-lexenv
   #:collect-symbol-macros-in-lexenv
   #:collect-blocks-in-lexenv
   #:collect-tags-in-lexenv

   #:find-variable-in-lexenv
   #:find-function-in-lexenv
   #:find-macro-in-lexenv
   #:find-symbol-macro-in-lexenv
   #:find-block-in-lexenv
   #:find-tag-in-lexenv

   ;;;
   ;;; some utils
   ;;;
   #:collect-variable-references
   #:compute-binding-usages
   #:special-variable-name?

   ;;;
   ;;; conditions
   ;;;
   #:walker-error
   #:simple-walker-error
   #:undefined-reference
   #:undefined-variable-reference
   #:undefined-function-reference
   #:return-from-unknown-block
   #:illegal-lambda-list

   ;;;
   ;;; walker
   ;;;
   #:walked-form
   #:form-attr
   #:map-ast
   #:walk-form
   #:walk-lambda
   #:walk-lambda-like
   #:unwalk-form
   #:unwalk-forms
   #:unwalk-lambda-list
   #:make-walk-environment

   #:defwalker-handler
   #:find-walker-handler
   #:defunwalker-handler
   #:with-form-object

   #:implicit-progn-mixin
   #:implicit-progn-with-declare-mixin
   #:binding-form-mixin
   #:bindings-of
   #:binding-entry-mixin
   #:usages-of
   #:variable-binding-entry-form
   #:declaration-form
   #:optimize-declaration-form
   #:variable-declaration-form
   #:function-declaration-form
   #:dynamic-extent-declaration-form
   #:variable-ignorable-declaration-form
   #:function-ignorable-declaration-form
   #:special-variable-declaration-form
   #:type-declaration-form
   #:ftype-declaration-form
   #:notinline-declaration-form
   #:unknown-declaration-form

   #:constant-form
   #:variable-reference-form
   #:lexical-variable-reference-form
   #:walked-lexical-variable-reference-form
   #:unwalked-lexical-variable-reference-form
   #:special-variable-reference-form
   #:free-variable-reference-form

   #:application-form
   #:lexical-application-form
   #:walked-lexical-application-form
   #:unwalked-lexical-application-form
   #:free-application-form
   #:lambda-application-form

   #:function-object-form
   #:function-definition-form
   #:lexical-function-object-form
   #:walked-lexical-function-object-form
   #:unwalked-lexical-function-object-form
   #:free-function-object-form

   #:function-form
   #:lambda-function-form
   #:labels-function-form
   #:function-argument-form
   #:required-function-argument-form
   #:specialized-function-argument-form
   #:optional-function-argument-form
   #:keyword-function-argument-form
   #:allow-other-keys-function-argument-form
   #:rest-function-argument-form

   #:block-form
   #:return-from-form
   #:catch-form
   #:throw-form
   #:eval-when-form
   #:if-form
   #:function-binding-form
   #:flet-form
   #:labels-form
   #:variable-binding-form
   #:let-form
   #:let*-form
   #:locally-form
   #:macrolet-form
   #:macro-binding-entry-form
   #:multiple-value-call-form
   #:multiple-value-prog1-form
   #:progn-form
   #:progv-form
   #:setq-form
   #:symbol-macrolet-form
   #:symbol-macro-binding-entry-form
   #:tagbody-form
   #:go-tag-form
   #:go-form
   #:the-form
   #:unwind-protect-form
   #:load-time-value-form

   #:walk-lambda-list
   #:walk-implict-progn
   #:body-of
   #:binding-of
   #:cleanup-form-of
   #:code-of
   #:condition-of
   #:declares-of
   #:default-value-of
   #:else-of
   #:enclosing-tagbody-of
   #:eval-when-times
   #:first-form-of
   #:function-designator-of
   #:keyword-name-of
   #:effective-keyword-name-of
   #:name-of
   #:other-forms-of
   #:parent-of
   #:protected-form-of
   #:read-only-p
   #:result-of
   #:source-of
   #:specializer-of
   #:special-binding?
   #:supplied-p-parameter
   #:tag-of
   #:target-block-of
   #:jump-target-of
   #:jump-tag-of
   #:then-of
   ;;#:type-of
   #:value-of
   #:values-form-of
   #:variable-of
   #:variables-form-of
   #:operator-of
   #:arguments-of

   #:collect-standard-walked-form-subclasses
   ))

(in-package :cl-walker)

(defun eval (form)
  (let (#+sbcl(sb-ext:*evaluator-mode* :interpret))
    (cl:eval form)))
