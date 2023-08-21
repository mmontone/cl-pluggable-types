(asdf:defsystem :pluggable-types-bid
  :author "Mariano Montone <marianomontone@gmail.com>"
  :version "0.1"
  :maintainer "Mariano Montone <marianomontone@gmail.com>"
  :licence "MIT"
  :components
  ((:file "trivia-functions")
   (:module "type-systems"
    :components
    ((:module "bidirectional"
      :components
      ((:file "package")
       (:file "util")
       (:file "type-system")
       (:file "bidirectional"))))))
  :depends-on (:pluggable-types
               :hu.dwim.walker
               :trivia
               :alexandria
               :anaphora
               :arrows
               :cl-algebraic-data-type))
