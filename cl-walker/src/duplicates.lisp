;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

(defmacro if-bind (var test &body then/else)
  (assert (first then/else)
          (then/else)
          "IF-BIND missing THEN clause.")
  (destructuring-bind (then &optional else)
      then/else
    `(let ((,var ,test))
       (if ,var ,then ,else))))

(defmacro aif (test then &optional else)
  `(if-bind it ,test ,then ,else))

(defmacro when-bind (var test &body body)
  `(if-bind ,var ,test (progn ,@body)))

(defmacro awhen (test &body body)
  `(when-bind it ,test ,@body))

(defmacro prog1-bind (var ret &body body)
  `(let ((,var ,ret))
    ,@body
    ,var))

(defmacro aprog1 (ret &body body)
  `(prog1-bind it ,ret ,@body))


;; from arnesi
(defmacro dolist* ((iterator list &optional return-value) &body body)
  "Like DOLIST but destructuring-binds the elements of LIST.

If ITERATOR is a symbol then dolist* is just like dolist EXCEPT
that it creates a fresh binding."
  (if (listp iterator)
      (let ((i (gensym "DOLIST*-I-")))
        `(dolist (,i ,list ,return-value)
           (destructuring-bind ,iterator ,i
             ,@body)))
      `(dolist (,iterator ,list ,return-value)
         (let ((,iterator ,iterator))
           ,@body))))

;; from stefil
(define-condition illegal-lambda-list (error)
  ((lambda-list :accessor lambda-list-of :initarg :lambda-list)))

(defun illegal-lambda-list (lambda-list)
  (error 'illegal-lambda-list :lambda-list lambda-list))

(defun parse-lambda-list (lambda-list visitor &key macro)
  ;; TODO finish macro lambda list parsing
  (declare (optimize (speed 3))
           (type list lambda-list)
           (type (or symbol function) visitor))
  (let ((args lambda-list))
    (labels
        ((fail ()
           (illegal-lambda-list lambda-list))
	 (process-&whole ()
           (assert (eq (first args) '&whole))
           (pop args)
           (unless macro
             (fail))
           (let ((whole (pop args)))
             (unless whole
               (fail))
             (funcall visitor '&whole whole whole))
           (case (first args)
             (&key          (entering-&key))
             (&rest         (process-&rest))
             (&optional     (entering-&optional))
             (&body         (process-&body))
             (&environment  (process-&environment))
             ((&whole &aux &allow-other-keys) (fail))
             (t             (process-required))))
         (process-&body ()
           (assert (eq (first args) '&body))
           (pop args)
           (unless macro
             (fail))
           (let ((body (pop args)))
             (unless (null args)
               (fail))
             (unless body
               (fail))
             (funcall visitor '&body body body)))
         (process-&environment ()
           (assert (eq (first args) '&environment))
           (pop args)
           (unless macro
             (fail))
           (let ((env (pop args)))
             (unless env
               (fail))
             (funcall visitor '&environment env env))
           (case (first args)
             (&key          (entering-&key))
             (&rest         (process-&rest))
             (&optional     (entering-&optional))
             (&body         (process-&body))
             (&aux          (process-&aux))
             ((&whole &environment &allow-other-keys) (fail))
             (t             (process-required))))
         (process-required ()
           (unless args
             (done))
           (case (first args)
             (&key          (entering-&key))
             (&rest         (process-&rest))
             (&optional     (entering-&optional))
             (&body         (process-&body))
             (&environment  (process-&environment))
             ((&whole &allow-other-keys) (fail))
             (&aux          (entering-&aux))
             (t
              (let ((arg (pop args)))
		(if (listp arg)
		    ;; We assume an argument type declaration
		    (destructuring-bind (name type) arg
		      (funcall visitor nil name name type))
		    ; else
		    (funcall visitor nil arg arg)))
              (process-required))))
         (process-&rest ()
           (assert (eq (first args) '&rest))
           (pop args)
           (let ((rest (pop args)))
             (unless rest
               (fail))
             (funcall visitor '&rest rest rest))
           (unless args
             (done))
           (case (first args)
             (&key               (entering-&key))
             (&environment       (process-&environment))
             ((&whole &optional &rest &body &allow-other-keys) (fail))
             (&aux               (entering-&aux))
             (t                  (fail))))
         (entering-&optional ()
           (assert (eq (first args) '&optional))
           (pop args)
           (process-&optional))
         (process-&optional ()
           (unless args
             (done))
           (case (first args)
             (&key               (entering-&key))
             (&rest              (process-&rest))
             (&body              (process-&body))
             ((&whole &optional &environment &allow-other-keys) (fail))
             (&aux               (entering-&aux))
             (t
              (let ((arg (ensure-list (pop args))))
                (funcall visitor '&optional (first arg) arg))
              (process-&optional))))
         (entering-&key ()
           (assert (eq (first args) '&key))
           (pop args)
           (process-&key))
         (process-&key ()
           (unless args
             (done))
           (case (first args)
             (&allow-other-keys       (funcall visitor '&allow-other-keys nil nil))
             ((&key &optional &whole &environment &body) (fail))
             (&aux                    (entering-&aux))
             (t
              (let ((arg (ensure-list (pop args))))
                (funcall visitor '&key (first arg) arg))
              (process-&key))))
         (entering-&aux ()
           (assert (eq (first args) '&aux))
           (pop args)
           (process-&aux))
         (process-&aux ()
           (unless args
             (done))
           (case (first args)
             ((&whole &optional &key &environment &allow-other-keys &aux &body) (fail))
             (t
              (let ((arg (ensure-list (pop args))))
                (funcall visitor '&aux (first arg) arg))
              (process-&aux))))
         (done ()
           (return-from parse-lambda-list (values))))
      (when args
        (case (first args)
          (&whole (process-&whole))
          (t      (process-required)))))))

(defun lambda-list-to-variable-name-list (args &key macro include-specials)
  (let ((result (list))
        (rest-variable-name nil)
        (whole-variable-name nil)
        (env-variable-name nil))
    (parse-lambda-list args
                       (lambda (kind name entry)
                         (declare (ignore entry))
                         (case kind
                           (&allow-other-keys )
                           (&environment      (setf env-variable-name name)
                                              (when include-specials
                                                (push name result)))
                           (&whole            (setf whole-variable-name name)
                                              (when include-specials
                                                (push name result)))
                           ((&rest &body)     (setf rest-variable-name name)
                                              (when include-specials
                                                (push name result)))
                           (t                 (push name result))))
                       :macro macro)
    (values (nreverse result)
            rest-variable-name
            whole-variable-name
            env-variable-name)))

;; from cl-def
(defmacro defprint-object (&whole whole class-name* &body body)
  "Define a PRINT-OBJECT method using PRINT-UNREADABLE-OBJECT.
  An example:
  (def print-object parenscript-dispatcher ; could be (parenscript-dispatcher :identity nil)
    (when (cachep self)
      (princ \"cached\")
      (princ \" \"))
    (princ (parenscript-file self)))"
  (with-unique-names (stream printing)
    (let ((args (ensure-list class-name*)))
      (destructuring-bind (class-name &key (identity t) (type t) with-package (muffle-errors t))
          args
        (multiple-value-bind (body declarations documentation)
            (parse-body body :documentation t :whole whole)
          `(defmethod print-object ((-self- ,class-name) ,stream)
             ,@(when documentation
                     (list documentation))
             ,@declarations
             (print-unreadable-object (-self- ,stream :type ,type :identity ,identity)
               (let ((*standard-output* ,stream))
                 (block ,printing
                   (,@(if muffle-errors
                          `(handler-bind ((error (lambda (error)
                                                   (declare (ignore error))
                                                   (write-string "<<error printing object>>")
                                                   (return-from ,printing)))))
                          `(progn))
                      (let (,@(when with-package `((*package* ,(find-package with-package)))))
                        ,@body)))))
             ;; primary PRINT-OBJECT methods are supposed to return the object
             -self-))))))
