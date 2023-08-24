(defpackage pluggable-types
  (:use :cl :anaphora :alexandria
        :polymorphic-types :polymorphic-cl-types)
  (:export #:typecheck
	   #:check-form
           #:type-checker-check-form
	   #:type-checker
           #:*compile-checks*
           #:*debug-compile-checks*
           #:*type-error-reporter*
           #:type-checking-error
           #:type-inconsistency-error))
