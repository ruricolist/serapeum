;;;; serapeum.asd

(asdf:defsystem #:serapeum
  :description "Utilities beyond Alexandria."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:optima
               #:uiop
               #:split-sequence
               #:string-case
               #:parse-number
               #:trivial-garbage
               #:bordeaux-threads
               #:named-readtables
               #:fare-quasiquote-extras)
  ;; Having had to untangle the dependencies once it seems worthwhile
  ;; to be explicit about them, if only to keep them under control.
  :components ((:file "package")
               ;; Macros.
               (:file "macro-tools"
                :depends-on ("package"))
               (:file "types"
                :depends-on ("package"))
               (:file "definitions"
                :depends-on ("macro-tools"))
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
               (:file "fbind"
                :depends-on ("macro-tools" "binding"))
               ;; Runtime.
               (:file "functions"
                :depends-on ("types"))
               (:file "lists"
                :depends-on ("types"
                             "definitions"
                             "fbind"
                             "iter"))
               (:file "trees"
                :depends-on ("fbind"
                             "functions"))
               (:file "strings"
                :depends-on ("definitions"
                             "fbind"
                             "iter"
                             "types"))
               (:file "hash-tables"
                :depends-on ("iter"
                             "definitions"))
               (:file "files"
                :depends-on ("functions"
                             "conditions"))
               (:file "symbols"
                :depends-on ("package"))
               (:file "arrays"
                :depends-on ("package"))
               (:file "queue"
                :depends-on ("binding" "types"))
               (:file "box"
                :depends-on ("package"))
               (:file "sequences"
                :depends-on ("definitions"
                             "binding"
                             "fbind"
                             "iter"))
               (:file "numbers"
                :depends-on ("definitions"))
               (:file "octets"
                :depends-on ("types"))
               (:file "time"
                :depends-on ("package"))
               (:file "clos"
                :depends-on ("package"))
               (:file "hooks"
                :depends-on ("package"))))
