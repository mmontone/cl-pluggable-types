(in-package :pluggable-types/const)

(defun assign-types-from-function-type (function-type args)
  "Assign types to the ARGS being passed to the function."
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
                 (error "Not enough arguments"))
             (push (cons arg arg-type) assignments)))
            (&optional
             (let ((arg (pop args-queue)))
               (when (null arg)
                 (return))
               (push (cons arg arg-type) assignments)))
            (&key
             (if (eql arg-type '&allow-other-keys)
                 (setq lambda-section '&allow-other-keys)
                 (destructuring-bind (key type) arg-type
                   (let ((arg-val (getf args-queue key)))
                     (when arg-val
                       (push (cons arg-val type) assignments)))
                   (alexandria:remove-from-plistf args-queue key))))
            (&allow-other-keys)
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

#|

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

(assign-types-from-function-type '(function (&rest number) t) '(x y z))

|#

;; Works over a walked application-form
(declaim (ftype (function (list walked-form) t) assign-types-from-function-type-2))
(defun assign-types-from-function-type-2 (function-type application-form)
  (declare (type list function-type)
           (type application-form application-form))
  (assert (eql (first function-type) 'function))
  (destructuring-bind (_ arg-types return-type) function-type
    (declare (ignore _ return-type))
    (let ((lambda-section '&required)
          (assignments (list))
          (args-queue (arguments-of application-form))
          (keys-in-plist-format nil))
      (dolist (arg-type arg-types)
        (block nil
          (when (member arg-type '(&optional &key &rest &aux))
            (setf lambda-section arg-type)
            (return))
          (case lambda-section
            (&required
             (let ((arg (pop args-queue)))
               (when (null arg)
                 (error "Not enough arguments"))
               (push (cons arg arg-type) assignments)))
            (&optional
             (let ((arg (pop args-queue)))
               (when (null arg)
                 (return))
               (push (cons arg arg-type) assignments)))
            (&key
             (if (eql arg-type '&allow-other-keys)
                 (setq lambda-section '&allow-other-keys)
                 (progn
                 ;; Convert the list of walked-forms to a plist
                 (when (not keys-in-plist-format)
                   (setf args-queue (loop for key in args-queue by #'cddr
                                          for value in (rest args-queue) by #'cddr
                                          collect (value-of key)
                                          collect value))
                   (setf keys-in-plist-format t))
                 (destructuring-bind (key type) arg-type
                   (let ((arg-val (getf args-queue key)))
                     (when arg-val
                       (push (cons arg-val type) assignments)))
                   (alexandria:remove-from-plistf args-queue key)))))
            (&allow-other-keys)
            (&rest
             ;; Consume all the passed args
             (dolist (arg args-queue)
               (push (cons arg arg-type) assignments))
             (setf args-queue nil)))))
      (unless (null args-queue)
        (if (eql lambda-section '&key)
            (error "Invalid key arguments in: ~s" args-queue)
            (error "Too many arguments")))
      (nreverse assignments))))

#|

(assign-types-from-function-type-2 '(function () t) (walk-form '(foo)))
(assign-types-from-function-type-2 '(function (number) t) (walk-form '(foo x)))
(assign-types-from-function-type-2 '(function (string &optional number) t)
                                      (walk-form '(foo x)))
(assign-types-from-function-type-2 '(function (string &optional number) t)
                                    (walk-form '(foo)))

(assign-types-from-function-type-2 '(function (string &optional number) t)
                                    (walk-form '(foo x y)))

(assign-types-from-function-type-2 '(function (string &optional number) t)
                                 (walk-form '(foo x y z)))

(assign-types-from-function-type-2 '(function (string &key (:y number)) t)
                                 (walk-form '(foo x)))

(assign-types-from-function-type-2 '(function (string &key (:y number)) t)
                                 (walk-form '(foo x :y y)))

(assign-types-from-function-type-2 '(function (string &key (:y number)) t)
                                  (walk-form '(foo x :y y :z "lala")))

(assign-types-from-function-type '(function (string &key (:y number)) t)
                                 '(x :y y :z "lala"))

(assign-types-from-function-type-2 '(function (&rest number) t)
                                  (walk-form '(foo x y z)))

|#

(defun some-tree (predicate tree)
  (cond
    ((atom tree) (funcall predicate tree))
    ((listp tree)
     (or (funcall predicate tree)
         (some (curry #'some-tree predicate) tree)))))

;; (some-tree (lambda (x)
;;                 (and (listp x)
;;                      (eql (car x) 'var)))
;;               '(or (var x)))

;; (some-tree (lambda (x)
;;                 (and (listp x)
;;                      (eql (car x) 'var)))
;;            '(or x z))

;; (defun tree-find-if (predicate tree)
;;   (cond
;;     ((atom tree)
;;      (when (funcall predicate tree)
;;        tree))
;;     ((listp tree)
;;      (when (funcall predicate tree)
;;        (return-from tree-find-if tree))
;;      (dolist (x tree)
;;        (when (tree-find-if predicate x)
;;          (return-from tree-find-if x))))
;;     (t nil)))

;; (tree-find-if (lambda (x)
;;                 (and (listp x)
;;                      (eql (car x) 'var)))
;;               '(or (var x)))

;; (tree-find-if (lambda (x)
;;                 (and (listp x)
;;                      (eql (car x) 'var)))
;;            '(or x z))

(defun type-equalp (t1 t2)
  (and (subtypep t1 t2)
       (subtypep t2 t1)))

(defun type-coerceablep (t1 t2)
  (or (subtypep t1 t2)
      (subtypep t2 t1)))

(defun tree-find (what tree)
  (cond
    ((eql what tree)
     t)
    ((atom tree)
     nil)
    ((consp tree)
     (or (tree-find what (car tree))
         (tree-find what (cdr tree))))
    (t nil)))

(tree-find 'a '(b (a)))

(defun tree-find-if (predicate tree)
  (cond
    ((atom tree)
     (funcall predicate tree))
    ((consp tree)
     (or (tree-find-if predicate (car tree))
         (tree-find-if predicate (cdr tree))))))

