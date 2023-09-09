(asdf:defsystem :pluggable-types-tests
  :author "Mariano Montone <marianomontone@gmail.com>"
  :version "0.1"
  :maintainer "Mariano Montone <marianomontone@gmail.com>"
  :licence "MIT"
  :components
  ((:module "tests"
            :components ((:file "tests"))))
  :depends-on (:pluggable-types :pluggable-types-const :fiasco))
