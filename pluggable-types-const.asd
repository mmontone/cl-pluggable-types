(asdf:defsystem :pluggable-types-const
  :author "Mariano Montone <marianomontone@gmail.com>"
  :version "0.1"
  :maintainer "Mariano Montone <marianomontone@gmail.com>"
  :licence "MIT"
  :components
  ((:file "trivia-functions")
   (:module "type-systems"
    :components
    ((:module "constraints"
      :components
      ((:file "package")
       (:file "util")
       (:file "read")
       (:file "constraints")
       (:file "compiler-hooks"))))))
  :depends-on (:mutils
               (:require :mutils-utils)
               (:require :compiler-hooks)
               (:require :compiler-info)
               :polymorphic-types
               :polymorphic-cl-types
               ;;:polymorphic-cl-types
               :hu.dwim.walker :trivia :alexandria
               :anaphora :arrows :cl-algebraic-data-type))
