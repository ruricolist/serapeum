;;;; serapeum.asd

(asdf:defsystem #:serapeum
  :description "Utilities beyond Alexandria."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op #:serapeum-tests)))
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
  :components ((:file "package")
               ;; Macros.
               (:file "macro-tools"
                :depends-on ("package"))
               (:file "types"
                :depends-on ("package"))
               (:file "definitions"
                :depends-on ("macro-tools" "iter"))
               (:file "internal-definitions"
                :depends-on ("definitions" "clos" "op" "control-flow"))
               (:file "binding"
                :depends-on ("macro-tools"))
               (:file "control-flow"
                :depends-on ("macro-tools"))
               (:file "threads"
                :depends-on ("package"))
               (:file "iter"
                :depends-on ("macro-tools"))
               (:file "conditions"
                :depends-on ("package"))
               (:file "op"
                :depends-on ("package"))
               (:file "functions"
                :depends-on ("macro-tools"))
               (:file "trees"
                :depends-on ("macro-tools"))
               (:file "hash-tables"
                :depends-on ("iter" "types" "control-flow"))
               (:file "files"
                :depends-on ("package"))
               (:file "symbols"
                :depends-on ("package"))
               (:file "arrays"
                :depends-on ("package"))
               (:file "queue"
                :depends-on ("package"))
               (:file "box"
                :depends-on ("package"))
               (:file "vectors"
                :depends-on ("package"))
               (:file "numbers"
                :depends-on ("macro-tools" "types"))
               (:file "octets"
                :depends-on ("types"))
               (:file "time"
                :depends-on ("package"))
               (:file "clos"
                :depends-on ("package"))
               (:file "mop"
                :depends-on ("package"))
               (:file "hooks"
                :depends-on ("package"))
               (:file "fbind"
                :depends-on ("binding"
                             "control-flow"))
               (:file "lists"
                :depends-on ("definitions"
                             "types"
                             "binding"
                             "fbind"
                             "iter"
                             "control-flow"
                             "queue"))
               (:file "strings" :depends-on ("lists" "op"))
               (:file "sequences" :depends-on ("lists"))))

(asdf:defsystem #:serapeum-tests
  :description "Test suite for Serapeum."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (#:serapeum #:fiveam)
  :perform (asdf:test-op (o c) (uiop:symbol-call :serapeum.tests :run-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "tests")))
