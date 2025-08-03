;;;; serapeum.asd

(defsystem "serapeum/portability"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :components ((:file "portability"))
  :depends-on
  ("alexandria"
   "trivia"))

(defsystem "serapeum/macro-tools"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :components ((:file "macro-tools"))
  :depends-on
  ("alexandria"
   "trivia"
   "parse-declarations-1.0"
   "introspect-environment"
   "trivial-cltl2"))

(defsystem "serapeum/types"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :components ((:file "types"))
  :depends-on
  ("serapeum/portability"
   "serapeum/macro-tools"))

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
               ;; Portability libraries.
               "trivial-garbage"
               "bordeaux-threads"
               "global-vars"
               "trivial-file-size"
               "trivial-macroexpand-all"
               ;; Subsystems
               "serapeum/portability"
               "serapeum/macro-tools"
               "serapeum/types")
  :serial t
  :components (;; The basics: these files can use CL and Alexandria.
               (:file "package")
               (:module "level0"
                :serial nil
                :pathname ""
                :components
                ((:file "definitions"
                  :depends-on ("iter" "defining-types"))
                 (:file "defining-types"
                  :depends-on ("iter"))
                 (:file "binding")
                 (:file "control-flow"
                  :depends-on ("definitions"))
                 (:file "threads")
                 (:file "iter")
                 (:file "conditions")
                 (:file "op")
                 ;; Depends on types.
                 (:file "functions"
                  :depends-on ("iter"))
                 (:file "trees")
                 ;; Depends on types.
                 (:file "hash-tables"
                  :depends-on ("iter" "control-flow" "binding"))
                 ;; Depends on types.
                 (:file "files")
                 (:file "symbols")
                 (:file "arrays")
                 ;; Depends on types.
                 (:file "queue")
                 ;; Depends on types.
                 (:file "box"
                  :depends-on ("definitions"))
                 ;; Depends on types.
                 (:file "numbers"
                  :depends-on ("op"))
                 ;; Depends on types.
                 (:file "octets")
                 (:file "time")
                 ;; Depends on types.
                 (:file "clos"
                  :depends-on ("binding"))
                 (:file "hooks"
                  :depends-on ("threads"))
                 (:file "fbind"
                  :depends-on ("binding" "control-flow" "op" "iter" "trees"))
                 (:file "static-let"
                  :depends-on ("fbind"))
                 (:file "reader"
                  :depends-on ("definitions"))
                 (:file "packages")
                 ;; Depends on types.
                 (:file "heap")))
               ;; Level 1 files can use CL, Alexandria, and any
               ;; Serapeum utilities defined at level 0. Intended for
               ;; functions on sequences.
               (:module "level1"
                :pathname ""
                :serial nil
                :components
                ((:file "lists")
                 (:file "sequences" :depends-on ("lists"))
                 (:file "strings" :depends-on ("sequences"))
                 (:file "vectors" :depends-on ("lists"))))
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
                 (:file "units")))))

(defsystem "serapeum/tests"
  :description "Test suite for Serapeum."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on ("serapeum"
               "fiveam"
               "local-time"
               "trivial-macroexpand-all"
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
                ((:file "portability")
                 (:file "macro-tools")
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
                 (:file "static-let")
                 (:file "lists")
                 (:file "strings")
                 (:file "sequences")
                 (:file "tree-case")
                 (:file "dispatch-case")
                 (:file "range")
                 (:file "generalized-arrays")
                 (:file "units")))))

(defsystem "serapeum/docs"
  :description "Serapeum's documentation generator."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on ("serapeum" "cl-ppcre" "swank")
  :serial t
  :components ((:file "docs")))
