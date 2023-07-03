(in-package :gradual.test)

(fiveam:in-suite* :gradual-tests)

(def-test constant-inference-test ()
  (is (subtypep (gradual:infer-type '2) 'number))
  (is (subtypep (gradual:infer-type "hello") 'string))
  (is (eql (gradual:infer-type ''foo) 'symbol)))

(def-test the-inference-test ()
  (is (eql (gradual:infer-type '(the integer (foo))) 'integer))
  (is (eql (gradual:infer-type '(the string (foo))) 'string)))

(def-test let-inference-test ()
  (is (subtypep
       (gradual:infer-type '(let ((x 2)) x))
       'number)))
