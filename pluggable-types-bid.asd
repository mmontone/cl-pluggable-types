(asdf:defsystem :pluggable-types-bid
  :name "gradual"
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
       (:file "types")
       (:file "util")
       (:file "read")
       (:file "bidirectional")
       (:file "compiler-hooks"))))))
  :depends-on (:mutils
               (:require :mutils-utils)
               (:require :compiler-hooks)
               (:require :compiler-info)
               :hu.dwim.walker :trivia :alexandria
               :anaphora :arrows :cl-algebraic-data-type))
