;;;; serapeum.asd

(defsystem "serapeum"
  :description "Utilities beyond Alexandria."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :in-order-to ((test-op (test-op "serapeum/tests")))
  :depends-on (;; Support for extensible sequences on Lisps that
               ;; provide it.
               (:feature :abcl (:require :extensible-sequences))
               ;; Existing utilities Serapeum builds on.
               "alexandria"
               "trivia"
               "split-sequence"
               "string-case"
               "parse-number"
               ;;; Portability libraries.
               "trivial-garbage"
               "bordeaux-threads"
               "parse-declarations-1.0"
               "introspect-environment"
               "trivial-cltl2"
               "global-vars"
               "trivial-file-size"
               "trivial-macroexpand-all")
  :serial t
  :components ((:file "package")
               ;; The basics: these files can use CL and Alexandria.
               (:file "portability")     ;Anything not worth using a portability layer for.
               (:file "macro-tools")     ;Very early.
               (:module "level0"
                :serial nil
                :pathname ""
                :components
                ((:file "types")
                 (:file "definitions"
                  :depends-on ("iter"))
                 (:file "defining-types"
                  :depends-on ("iter" "threads"))
                 (:file "binding")
                 (:file "control-flow"
                  :depends-on ("definitions"))
                 (:file "threads")
                 (:file "iter")
                 (:file "conditions")
                 (:file "op")
                 (:file "functions"
                  :depends-on ("types" "hash-tables" "iter"))
                 (:file "trees")
                 (:file "hash-tables"
                  :depends-on ("iter" "types" "control-flow"))
                 (:file "files"
                  :depends-on ("types"))
                 (:file "symbols")
                 (:file "arrays")
                 (:file "queue"
                  :depends-on ("types"))
                 (:file "box"
                  :depends-on ("types" "definitions"))
                 (:file "numbers"
                  :depends-on ("types" "op"))
                 (:file "octets"
                  :depends-on ("types"))
                 (:file "time")
                 (:file "clos"
                  :depends-on ("binding" "types"))
                 (:file "hooks"
                  :depends-on ("threads"))
                 (:file "fbind"
                  :depends-on ("binding" "control-flow" "op" "iter" "trees"))
                 (:file "reader"
                  :depends-on ("definitions"))
                 (:file "packages")
                 (:file "heap"
                  :depends-on ("types" "fbind"))))
               ;; Level 1 files can use CL, Alexandria, and any
               ;; Serapeum utilities defined at level 0. Intended for
               ;; functions on sequences.
               (:module "level1"
                :pathname ""
                :serial nil
                :components
                ((:file "lists")
                 (:file "sequences")
                 (:file "strings" :depends-on ("sequences"))
                 (:file "vectors")))
               ;; Level 2 files can use CL, Alexandria, and the rest
               ;; of Serapeum. Anything at this level could, in
               ;; principle, be its own separate library that depends
               ;; on Serapeum.
               (:module "level2"
                :pathname ""
                :serial nil
                :components
                ((:file "exporting")
                 (:file "vector=")
                 (:file "mop")
                 (:file "internal-definitions")
                 (:file "tree-case")
                 (:file "dispatch-case")
                 (:file "range" :depends-on ("dispatch-case"))
                 (:file "generalized-arrays" :depends-on ("range"))
                 (:file "units")))
               (:module "contrib"
                :components
                ((:file "hooks")))))

(defsystem "serapeum/tests"
  :description "Test suite for Serapeum."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on ("serapeum"
               "fiveam"
               "local-time"
               (:feature
                (:or :allegro :ccl :clasp :ecl :lispworks :mezzano :sbcl)
                "atomics"))
  :perform (test-op (o c) (symbol-call :serapeum.tests :run-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "tests"
                :depends-on ("package"))
               (:module "test suites"
                :pathname ""
                :serial nil
                :components
                ((:file "macro-tools")
                 (:file "types")
                 (:file "definitions")
                 (:file "defining-types-aux"
                  :if-feature (:not :abcl))
                 (:file "defining-types"
                  :if-feature (:not :abcl)
                  :depends-on ("defining-types-aux"))
                 (:file "internal-definitions")
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
                 (:file "vector=")
                 (:file "numbers")
                 (:file "octets")
                 (:file "time")
                 (:file "clos")
                 (:file "mop")
                 (:file "hooks")
                 (:file "fbind")
                 (:file "lists")
                 (:file "strings")
                 (:file "sequences")
                 (:file "tree-case")
                 (:file "dispatch-case")
                 (:file "range")
                 (:file "generalized-arrays")
                 (:file "quicklisp")
                 (:file "units")
                 (:module "contrib"
                  :components ((:file "hooks")))))))

(defsystem "serapeum/docs"
  :description "Serapeum's documentation generator."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on ("serapeum" "cl-ppcre" "swank")
  :serial t
  :components ((:file "docs")))
