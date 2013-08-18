;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker-test)

(defsuite* (test/walk-unwalk :in test))

(defvar *foo*)

(deftest test/walk-unwalk/special-variable-name? ()
  (is (special-variable-name? '*foo*)
      "Unbound special variables are not properly detected! It needs platform dependent support..."))

(deftest check-walk-unwalk (form &optional (expected form) env)
  (declare (optimize debug))
  (unless expected
    (setf expected form))
  (unless env
    (setf env (make-empty-lexical-environment)))
  (let ((walked-form (unwalk-form (walk-form form nil (make-walk-environment env)))))
    (is (equal walked-form expected))))

(defmacro define-walk-unwalk-test (name &body body)
  `(deftest ,name ()
     (with-walker-configuration (:undefined-reference-handler nil)
       ,@(loop
            :for entry :in body
            :collect (if (and (consp entry)
                              (eq (first entry) 'with-expected-failures))
                         `(with-expected-failures
                            ,@(mapcar (lambda (entry)
                                        `(check-walk-unwalk ',entry))
                                      (rest entry)))
                         `(check-walk-unwalk ',entry))))))

(define-walk-unwalk-test test/constant
  1 'a "a" (1 2 3) #(1 2 3))

(deftest test/constant/nil-and-t ()
  (is (typep (walk-form 't) 'constant-form))
  (is (typep (walk-form 'nil) 'constant-form)))

(define-walk-unwalk-test test/variable
  var
  :var)

(define-walk-unwalk-test test/application
  (* 2 3)
  (+ (* 3 3) (* 4 4)))

(define-walk-unwalk-test test/lambda-application
  ((lambda (x) (x x))
   #'(lambda (x) (x x)))
  ((lambda (x k) (k x))
   (if p x y)
   id))

(define-walk-unwalk-test test/declare/1
  (locally (declare (zork)))
  (locally (declare (optimize speed) (optimize (debug 2))))
  (locally (declare (ignorable a) (ignorable b)))
  (locally (declare (dynamic-extent a) (ignorable b))))

(deftest test/declare/2 ()
  (signals style-warning
    (walk-form '(locally (declare (zork)))))
  (signals style-warning
    (walk-form '(locally (declare (integer x))))))

(deftest test/declare/3 ()
  (check-walk-unwalk
   '(lambda () (declare))
   '#'(lambda ()))
  (check-walk-unwalk
   '(macrolet ((x (&body body)
                `(locally
                     (declare (unknown abc))
                   ,@body)))
     (x 42))
   '(locally (locally (declare (unknown abc)) 42)))
  (check-walk-unwalk
   '(lambda () (declare (ignorable)))
   '#'(lambda ())))

(deftest test/macro/1 ()
  (finishes
    (walk-form '(macrolet ((foo ((some-complex-args &optional (even-more-compelx 42)))
                            `(bar ,some-complex-args ,even-more-compelx)))
                 (foo (1 2)))
               nil (make-walk-environment))))

(define-walk-unwalk-test test/lambda-function
  #'(lambda (x y) (y x))
  #'(lambda (x &key y z) (z (y x)))
  #'(lambda (&optional x y) (list x y))
  #'(lambda (x &rest args) (apply x args))
  #'(lambda (object &key (a nil a?)) (values))
  #'(lambda (object &key a b &allow-other-keys) (values))
  #'(lambda (&optional x y &rest args &key a b &allow-other-keys) 42))

(deftest test/invalid-lambda-list ()
  (signals illegal-lambda-list
    (walk-form '(lambda (&rest args &key a b &optional x y &allow-other-keys) 42))))

(define-walk-unwalk-test test/walk-unwalk/block
  (block label (get-up) (eat-food) (go-to-sleep))
  (block label ((lambda (f x) (f (f x))) #'car))
  (block label (reachable) (return-from label 'done) (unreachable)))

(define-walk-unwalk-test test/walk-unwalk/catch
  (catch 'done (with-call/cc* (* 2 3)))
  (catch 'scheduler
    (tagbody start
       (funcall thunk)
       (if (done-p) (throw 'scheduler 'done) (go start))))
  (catch 'c
    (flet ((c1 () (throw 'c 1)))
      (catch 'c (c1) (print 'unreachable))
      2)))

(define-walk-unwalk-test test/walk-unwalk/if
  (if p x y)
  (if (pred x) (f x) (f-tail y #(1 2 3))))

(define-walk-unwalk-test test/walk-unwalk/flet
  (flet ((sq (x)
           (* x x)))
    (+ (sq 3) (sq 4)))
  (flet ((prline (s)
           (princ s)
           (terpri)))
    (prline "hello")
    (prline "world")))

(define-walk-unwalk-test test/walk-unwalk/labels
  (labels ((fac-acc (n acc)
             (if (zerop n)
                 (land acc)
                 (bounce
                  (fac-acc (1- n) (* n acc))))))
    (fac-acc (fac-acc 10 1) 1))
  (labels ((evenp (n)
             (if (zerop n) t (oddp (1- n))))
           (oddp (n)
             (if (zerop n) nil (evenp (1- n)))))
    (oddp 666)))

(define-walk-unwalk-test test/walk-unwalk/let
  (let ((a 2) (b 3) (c 4))
    (+ (- a b) (- b c) (- c a)))
  (let ((a b) (b a)) (format t "side-effect~%") (f a b)))

(define-walk-unwalk-test test/walk-unwalk/let*
  (let* ((a (random 100)) (b (* a a))) (- b a))
  (let* ((a b) (b a)) (equal a b)))

(define-walk-unwalk-test test/walk-unwalk/load-time-value
  (load-time-value *load-pathname* #-ecl t))

(define-walk-unwalk-test test/walk-unwalk/locally
  (locally (setq *global* (whoops))))

(define-walk-unwalk-test test/walk-unwalk/multiple-value-call
  (multiple-value-call #'list 1 '/ (values 2 3) '/ (values) '/ (floor 2.5))
  (multiple-value-call #'+ (floor 5 3) (floor 19 4)))

(define-walk-unwalk-test test/walk-unwalk/multiple-value-prog1
  (multiple-value-prog1
      (values-list temp)
    (setq temp nil)
    (values-list temp)))

(define-walk-unwalk-test test/walk-unwalk/progn
  (progn (f a) (f-tail b) c)
  (progn #'(lambda (x) (x x)) 2 'a))

(define-walk-unwalk-test test/walk-unwalk/progv
  (progv '(*x*) '(2) *x*))

(define-walk-unwalk-test test/walk-unwalk/setq
  (setq x '(2 #(3 5 7) 11 "13" '17))
  (setq *global* 'symbol))

(define-walk-unwalk-test test/walk-unwalk/tagbody
  (tagbody
     (setq val 1)
     (go point-a)
     (setq val (+ val 16))
   point-c
     (setq val (+ val 4))
     (go point-b)
     (setq val (+ val 32))
   point-a
     (setq val (+ val 2))
     (go point-c)
     (setq val (+ val 64))
   point-b
     (setq val (+ val 8)))
  (tagbody
     (setq n (f2 flag #'(lambda () (go out))))
   out
     (prin1 n)))

(define-walk-unwalk-test test/walk-unwalk/the
  (the number (reverse "naoh"))
  (the string 1))

(define-walk-unwalk-test test/walk-unwalk/unwind-protect
  (unwind-protect
       (progn (setq count (+ count 1))
              (perform-access))
    (setq count (- count 1)))
  (unwind-protect
       (progn (with-call/cc* (walk-the-plank))
              (pushed-off-the-plank))
    (save-life)))
