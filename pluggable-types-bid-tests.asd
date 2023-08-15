(asdf:defsystem :pluggable-types-bid-tests
  :author "Mariano Montone <marianomontone@gmail.com>"
  :version "0.1"
  :maintainer "Mariano Montone <marianomontone@gmail.com>"
  :licence "MIT"
  :components
  ((:module "type-systems"
    :components
    ((:module "bidirectional"
      :components
      ((:file "tests"))))))
  :depends-on (:pluggable-types-bid :fiasco))
