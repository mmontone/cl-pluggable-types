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
