(asdf:defsystem :pluggable-types-const
  :author "Mariano Montone <marianomontone@gmail.com>"
  :version "0.1"
  :maintainer "Mariano Montone <marianomontone@gmail.com>"
  :licence "MIT"
  :components
  ((:file "trivia-functions")
   (:module "type-checkers"
    :components
    ((:module "constraints"
      :components
      ((:file "package")
       (:file "util")
       (:file "type-checker"))))))
  :depends-on (:pluggable-types
               :trivia
               :hu.dwim.walker
               :cl-algebraic-data-type
               :arrows))
