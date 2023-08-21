(asdf:defsystem :pluggable-types
  :author "Mariano Montone <marianomontone@gmail.com>"
  :version "0.1"
  :maintainer "Mariano Montone <marianomontone@gmail.com>"
  :licence "MIT"
  :components
  ((:file "package")
   (:file "pluggable-types")
   (:file "util")
   (:file "read")
   (:file "compiler-hooks"))
  :depends-on (:mutils
               (:require :mutils-utils)
               (:require :compiler-hooks)
               (:require :compiler-info)
               :trivia
               :polymorphic-types
               :polymorphic-cl-types))
