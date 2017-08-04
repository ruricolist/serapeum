;;;; serapeum.asd

(asdf:defsystem #:serapeum
  :description "Utilities beyond Alexandria."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op #:serapeum/tests)))
  :depends-on (#:alexandria
               #:trivia
               #:trivia.quasiquote
               #:uiop
               #:split-sequence
               #:string-case
               #:parse-number
               #:trivial-garbage
               #:bordeaux-threads
               #:named-readtables
               #:fare-quasiquote-extras
               #:parse-declarations-1.0
               #:introspect-environment
               #:global-vars
               #:cl-algebraic-data-type)
  ;; Having had to untangle the dependencies once it seems worthwhile
  ;; to be explicit about them, if only to keep them under control.
  :serial t
  :components ((:file "package")
               ;; The basics: these files can use CL and Alexandria.
               (:module "level0"
                :serial nil
                :pathname ""
                :components
                ((:file "macro-tools")
                 (:file "types")
                 (:file "definitions"
                  :depends-on ("macro-tools" "iter"))
                 (:file "internal-definitions"
                  :depends-on ("definitions" "clos" "op" "control-flow"))
                 (:file "binding"
                  :depends-on ("macro-tools"))
                 (:file "control-flow"
                  :depends-on ("macro-tools"))
                 (:file "threads")
                 (:file "iter"
                  :depends-on ("macro-tools"))
                 (:file "conditions")
                 (:file "op")
                 (:file "functions"
                  :depends-on ("macro-tools" "types" "hash-tables"))
                 (:file "trees"
                  :depends-on ("macro-tools"))
                 (:file "hash-tables"
                  :depends-on ("iter" "types" "control-flow"))
                 (:file "files")
                 (:file "symbols")
                 (:file "arrays")
                 (:file "queue"
                  :depends-on ("types"))
                 (:file "box"
                  :depends-on ("types" "definitions"))
                 (:file "vectors")
                 (:file "numbers"
                  :depends-on ("macro-tools" "types"))
                 (:file "octets"
                  :depends-on ("types"))
                 (:file "time")
                 (:file "clos")
                 (:file "mop")
                 (:file "hooks")
                 (:file "fbind"
                  :depends-on ("binding" "control-flow"))))
               ;; Level 1 files can use CL, Alexandria, and any
               ;; Serapeum utilities defined at level 0. Intended for
               ;; functions on sequences.
               (:module "level1"
                :pathname ""
                :serial nil
                :components
                ((:file "lists")
                 (:file "strings")
                 (:file "sequences")))
               ;; Level 2 files can use CL, Alexandria, and the rest
               ;; of Serapeum. Anything at this level could, in
               ;; principle, be its own separate library that depends
               ;; on Serapeum.
               (:module "level2"
                :pathname ""
                :serial nil
                :components
                ((:file "internal-definitions")
                 (:file "tree-case")))))

(asdf:defsystem "serapeum/tests"
  :description "Test suite for Serapeum."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on ("serapeum" "fiveam")
  :perform (asdf:test-op (o c) (uiop:symbol-call :serapeum.tests :run-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "tests"        :depends-on ("package"))
               (:module "test suites"
                :pathname ""
                :serial nil
                :components
                ((:file "macro-tools")
                 (:file "types")
                 (:file "definitions")
                 (:file "binding")
                 (:file "control-flow")
                 (:file "threads")
                 (:file "iteration")
                 (:file "conditions")
                 (:file "op")
                 (:file "functions")
                 (:file "trees")
                 (:file "hash-tables")
                 (:file "files")
                 (:file "symbols")
                 (:file "arrays")
                 (:file "queue")
                 (:file "box")
                 (:file "vectors")
                 (:file "numbers")
                 (:file "octets")
                 (:file "time")
                 (:file "clos")
                 (:file "hooks")
                 (:file "fbind")
                 (:file "lists")
                 (:file "strings")
                 (:file "sequences")
                 (:file "tree-case")
                 (:file "quicklisp")))))
