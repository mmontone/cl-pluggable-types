(asdf:defsystem :pluggable-types-decl
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
       (:file "compiler-hooks")
       (:file "compiler-info")
       (:file "package")
       (:file "util")
       (:file "read")
       (:file "types")
       (:file "unify"))))))
  :depends-on (:hu.dwim.walker :trivia :alexandria :anaphora :arrows :cl-algebraic-data-type :cl-package-locks))
