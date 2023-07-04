(in-package :gradual.test)

(fiveam:in-suite* :gradual-tests)

(def-test constant-inference-test ()
  (is (subtypep (gradual:infer-type '2 'gradual-type-system) 'number))
  (is (subtypep (gradual:infer-type "hello" 'gradual-type-system) 'string))
  (is (eql (gradual:infer-type ''foo 'gradual-type-system) 'symbol)))

(def-test the-inference-test ()
  (is (eql (gradual:infer-type '(the integer (foo)) 'gradual-type-system) 'integer))
  (is (eql (gradual:infer-type '(the string (foo)) 'gradual-type-system) 'string)))

(def-test let-inference-test ()
  (is (subtypep
       (gradual:infer-type '(let ((x 2)) x) 'gradual-type-system)
       'number)))

(def-test make-instance-inference-test ()
  (is (eql (gradual:infer-type '(make-instance 'foo) 'gradual-type-system)
           'foo))
  (is (eql (gradual:infer-type '(apply #'make-instance 'foo) 'gradual-type-system)
           'foo)))

(def-test apply-inference-test ()
  (is (subtypep
       (gradual:infer-type '(apply '+ 1 2) 'gradual-type-system)
       'number))
  (is (subtypep
       (gradual:infer-type '(apply #'+ 1 2) 'gradual-type-system)
       'number))
  (is (eql (gradual:infer-type '(apply #'make-instance 'foo) 'gradual-type-system)
           'foo)))

(def-test progn-inference-test ()
  (as-> (gradual:infer-type '(progn (foo) 22) 'gradual-type-system) type
        (is (subtypep type 'integer))))

(def-test block-inference-test ()
  (gradual:infer-type '(block nil (foo) 22) 'gradual-type-system))

(def-test prog1-inference-test ()
  (as-> (gradual:infer-type '(prog1 22 "hello") 'gradual-type-system) type
        (is (subtypep type 'number))))

(def-test return-from-inference-test ()
  (as-> (gradual:infer-type '(block nil
                              (when t (return-from nil "hello"))
                              (when nil (return-from nil 222)))
                            'gradual-type-system)))
