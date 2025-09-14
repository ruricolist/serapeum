;;;; serapeum.asd

(defsystem "serapeum/portability"
  :description "Subtrivial portability."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :components ((:file "portability"))
  :depends-on
  ("alexandria"
   "trivia"))

(defsystem "serapeum/macro-tools"
  :description "Tools for writing macros."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :components
  ((:file "macro-tools"))
  :depends-on
  ("parse-declarations-1.0"
   "introspect-environment"
   "trivia"
   "trivial-cltl2"))

(defsystem "serapeum/types"
  :description "Utility types and type utilities."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :components ((:file "types"))
  :depends-on
  ("serapeum/portability"
   "serapeum/macro-tools"))

(defsystem "serapeum/unlocked"
  :description "Helper system to export symbols from an unlocked package"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :serial t
  :components
  ((:file "unlocked")))

(defsystem "serapeum/iter"
  :description "Iteration constructs and utilities."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :serial t
  :components ((:file "iter"))
  :depends-on
  ("alexandria"
   "parse-declarations-1.0"
   "serapeum/unlocked"))

(defsystem "serapeum/binding"
  :description "BInding utilities."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :serial t
  :components ((:file "binding"))
  :depends-on
  ("alexandria"
   "serapeum/macro-tools"
   "trivia"))

(defsystem "serapeum/control-flow"
  :description "Control flow utilities."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :serial t
  :components ((:file "control-flow"))
  :depends-on
  ("alexandria"
   "introspect-environment"
   "serapeum/binding"
   "serapeum/macro-tools"
   "serapeum/types"
   "string-case"
   "trivia"))

(defsystem "serapeum/definitions"
  :description "Macros for definitions."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :serial t
  :components ((:file "definitions"))
  :depends-on
  ("alexandria"
   "global-vars"
   "serapeum/iter"
   "serapeum/macro-tools"
   "trivia"))

(defsystem "serapeum/defining-types"
  :description "Macros for defining types."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :serial t
  :components ((:file "defining-types"))
  :depends-on
  ("alexandria"
   "bordeaux-threads"
   "serapeum/control-flow"
   "serapeum/definitions"
   "serapeum/macro-tools"
   "serapeum/types"
   "serapeum/unlocked"
   "trivia"))

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
               "serapeum/types"
               "serapeum/iter"
               "serapeum/control-flow"
               "serapeum/binding"
               "serapeum/definitions"
               "serapeum/defining-types")
  :serial t
  :components (;; The basics: these files can use CL and Alexandria.
               (:file "package")
               (:module "level0"
                :serial nil
                :pathname ""
                :components
                ((:file "threads")
                 (:file "conditions")
                 (:file "op")
                 ;; Depends on types.
                 (:file "functions")
                 (:file "trees")
                 ;; Depends on types.
                 (:file "hash-tables")
                 ;; Depends on types.
                 (:file "files")
                 (:file "symbols")
                 (:file "arrays")
                 ;; Depends on types.
                 (:file "queue")
                 ;; Depends on types, definitions.
                 (:file "box")
                 ;; Depends on types.
                 (:file "numbers"
                  :depends-on ("op"))
                 ;; Depends on types.
                 (:file "octets")
                 (:file "time")
                 ;; Depends on types.
                 (:file "clos")
                 (:file "hooks"
                  :depends-on ("threads"))
                 (:file "fbind"
                  :depends-on ("op" "trees"))
                 (:file "static-let"
                  :depends-on ("fbind"))
                 ;; Depends on definitions.
                 (:file "reader")
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
