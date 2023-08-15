(asdf:defsystem :pluggable-types-bid
  :author "Mariano Montone <marianomontone@gmail.com>"
  :version "0.1"
  :maintainer "Mariano Montone <marianomontone@gmail.com>"
  :licence "MIT"
  :components
  ((:module "type-systems"
    :components
    ((:module "decl"
      :components
      ((:file "trivia-functions")
       (:file "package")
       (:file "util")
       (:file "read")
       (:file "bidirectional")
       (:file "compiler-hooks"))))))
  :depends-on (:mutils
               (:require :mutils-utils)
               (:require :compiler-hooks)
               (:require :compiler-info)
               :polymorphic-types
               ;;:polymorphic-cl-types
               :hu.dwim.walker :trivia :alexandria
               :anaphora :arrows :cl-algebraic-data-type))
