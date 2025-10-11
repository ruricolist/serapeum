;;;; package.lisp

(uiop:define-package #:serapeum
  (:use-reexport
   #:serapeum/iter
   #:serapeum/macro-tools
   #:serapeum/portability
   #:serapeum/unlocked
   #:serapeum/types
   #:serapeum/binding
   #:serapeum/control-flow
   #:serapeum/definitions
   #:serapeum/defining-types
   #:serapeum/conditions
   #:serapeum/op
   #:serapeum/box)
  (:use :cl)
  (:import-from #:alexandria
                ;; Binding constructs
                #:if-let
                #:when-let
                #:when-let*
                ;; Definitions
                #:define-constant
                ;; Hash tables
                #:copy-hash-table
                #:ensure-gethash
                #:hash-table-values
                #:plist-hash-table
                ;; Functions
                #:compose
                #:curry
                #:disjoin
                #:ensure-function
                #:named-lambda
                ;; Lists
                #:appendf
                #:doplist
                #:ensure-car
                #:ensure-list
                #:flatten
                #:lastcar
                #:mappend
                #:proper-list-p
                #:remove-from-plist
                #:setp
                ;; Numbers
                #:clamp
                ;; Arrays
                #:array-index
                #:array-length
                ;; Sequences
                #:copy-sequence
                #:emptyp
                #:last-elt
                #:ends-with-subseq
                #:extremum
                #:first-elt
                #:length=
                #:random-elt
                #:removef
                #:rotate
                #:sequence-of-length-p
                #:shuffle
                #:starts-with-subseq
                ;; Macros
                #:once-only
                #:parse-body
                #:parse-ordinary-lambda-list
                #:with-gensyms
                #:with-unique-names
                ;; Symbols
                #:make-gensym-list
                #:make-keyword
                ;; Strings
                #:string-designator
                ;; Types
                #:non-negative-integer
                #:of-type
                #:type=
                ;; Conditions
                #:required-argument
                #:ignore-some-conditions
                #:simple-style-warning
                #:simple-program-error
                ;; Functions
                #:copy-stream
                ;; io
                #:with-input-from-file
                #:with-output-to-file
                ;; new additions
                #:symbolicate)
  (:import-from #:tcr.parse-declarations-1.0
                #:map-declaration-env
                #:filter-declaration-env
                #:parse-declarations
                #:build-declarations)
  (:import-from #:split-sequence
                #:split-sequence
                #:split-sequence-if
                #:split-sequence-if-not)
  (:import-from #:parse-number
                #:parse-number
                #:parse-positive-real-number
                #:parse-real-number
                #:invalid-number
                #:invalid-number-value
                #:invalid-number-reason)
  (:import-from #:introspect-environment
                #:compiler-macroexpand #:compiler-macroexpand-1
                #:constant-form-value #:typexpand #:typexpand-1)
  (:import-from #:trivia
                #:match #:ematch :defpattern)
  (:import-from #:trivial-file-size
                #:file-size-in-octets)
  (:import-from
   #:serapeum/unlocked
   #:sum)
  (:import-from
   #:serapeum/macro-tools
   #:declaim-maybe-inline
   #:ensuring-functions
   #:extract-function-name
   #:gensym?
   #:lambda-list-vars
   #:let-over-lambda
   #:policy-quality
   #:rebinding-functions
   #:simple-lambda-list?
   #:speed-matters?
   #:variable-type)
  (:import-from
   #:serapeum/types
   #:canonicalize-key
   #:canonicalize-test
   #:declaim-freeze-type
   #:declaim-constant-function
   #:seq-dispatch
   #:truly-the
   #:vector-dispatch
   #:with-simple-vector-dispatch
   #:with-type-declarations-trusted)
  (:import-from
   #:serapeum/iter
   #:collecting*)
  (:import-from
   #:serapeum/control-flow
   #:check-exhaustiveness)
  (:import-from
   #:serapeum/definitions
   #:unbound)
  (:import-from
   #:serapeum/conditions
   #:ignoring)
  (:documentation "Utilities beyond Alexandria.")
  (:export
   ;; Internal definitions.
   #:local
   #:local*
   #:block-compile
   ;; Binding.
   ;; Statics.
   #:recklessly-continue
   #:static-binding-flush-error
   #:static-binding-flush-error-group
   #:static-binding-flush-error-all-groups-p
   #:static-binding-active-error
   #:flush-static-binding-group
   #:flush-all-static-binding-groups
   #:static-let
   #:static-let*
   ;; Dispatch case.
   :dispatch-case
   :dispatch-case-error
   :dispatch-case-let
   :dispatch-caseql
   :dispatch-caseql-let
   ;; Threads.
   #:count-cpus
   #:synchronized
   #:monitor
   ;; Iteration.
   #:nlet
   #:collecting
   #:with-collector
   #:with-collectors
   #:summing
   ;; Conditions.
   #:ignoring
   ;; Functions.
   #:eqs
   #:eqls
   #:equals
   #:capped-fork
   #:capped-fork2
   #:define-train
   #:distinct
   #:dynamic-closure
   #:flip
   #:fork
   #:fork2
   #:hook
   #:hook2
   #:juxt
   #:nth-arg
   #:partial
   #:unary->variadic
   #:variadic->unary
   #:throttle
   #:trampoline
   #:once
   #:fnil
   #:mvconstantly
   #:fuel
   #:do-nothing
   #:repeat-until-stable
   ;; Trees.
   #:reuse-cons
   #:car+cdr
   #:occurs
   #:occurs-if
   #:prune
   #:prune-if
   #:walk-tree
   #:map-tree
   #:leaf-walk
   #:leaf-map
   ;; Hash tables.
   #:do-hash-table
   #:pophash
   #:swaphash
   #:dict
   #:dict*
   #:dictq
   #:href #:@
   #:href-default
   #:merge-tables
   #:merge-tables*
   #:flip-hash-table
   #:hash-fold
   #:maphash-return
   #:maphash-new
   #:maphash-into
   #:set-hash-table
   #:hash-table-set
   #:hash-table-predicate
   #:hash-table-function
   #:make-hash-table-function
   #:delete-from-hash-table
   #:pairhash
   #:pretty-print-hash-table
   #:toggle-pretty-print-hash-table
   #:hash-table-test-p
   ;; Pathnames.
   #:wild-pathname
   #:non-wild-pathname
   #:absolute-pathname
   #:relative-pathname
   #:directory-pathname
   #:absolute-directory-pathname
   #:absolute-file-pathname
   #:file-pathname
   #:physical-pathname
   #:path-join
   #:path-basename
   #:base-path-join
   #:write-stream-into-file
   #:write-file-into-stream
   #:file=
   #:file-size
   #:exe
   #:resolve-executable
   #:format-file-size-human-readable
   #:file-size-human-readable
   #:with-open-files
   ;; Symbols.
   #:find-keyword
   #:bound-value
   ;; Arrays.
   #:array-index-row-major
   ;; Queues.
   #:queue
   #:queuep
   #:enq
   #:deq
   #:undeq
   #:front
   #:qback
   #:qlen
   #:qlist
   #:qconc
   #:qpreconc
   #:qappend
   #:qprepend
   #:queue-empty-p
   #:clear-queue
   #:copy-queue
   ;; Heaps.
   #:make-heap
   #:heap-insert
   #:heap-maximum
   #:heap-extract-maximum
   #:heap-extract
   #:heap-extract-all
   ;; Vectors.
   #:ensure-vector
   #:vect
   #:vector=
   #:values-vector
   #:vector-conc-extend
   ;; Numbers.
   #:null-if-zero
   #:fixnump
   #:parse-number
   #:parse-real-number
   #:parse-positive-real-number
   #:invalid-number
   #:invalid-number-reason
   #:invalid-number-value
   #:parse-float
   #:bits
   #:unbits
   #:round-to
   #:finc
   #:shift-incf
   #:fdec
   #:shift-decf
   #:shrink
   #:grow
   #:shrinkf
   #:growf
   #:random-in-range
   #:float-precision-contagion
   #:range
   ;; Octets.
   #:octet-vector-p
   #:make-octet-vector
   #:octets
   #:unoctets
   #:octet-vector=
   ;; Time.
   #:universal-to-unix
   #:unix-to-universal
   #:get-unix-time
   #:date-leap-year-p
   #:time-since
   #:time-until
   #:interval
   ;; Classes.
   #:subclass-union
   #:class-name-of
   #:class-name-safe
   #:find-class-safe
   #:make
   #:defmethods
   #:slot-value-safe
   ;; MOP.
   #:abstract-standard-class
   #:standard/context
   #:topmost-object-class
   ;; Hooks.
   #:*hook*
   #:with-hook-restart
   #:add-hook
   #:remove-hook
   #:run-hooks
   #:run-hook
   #:run-hook
   #:run-hook-until-failure
   #:run-hook-until-success
   ;; Fbind.
   #:fbind
   #:fbind*
   #:fbindrec
   #:fbindrec*
   #:letrec-restriction-violation
   ;; Lists.
   #:in
   #:filter-map
   #:memq
   #:delq
   #:append1
   #:nconc1
   #:prepend
   #:prependf
   #:push-end
   #:push-end-new
   #:assocar
   #:assocdr
   #:assocadr
   #:rassocar
   #:rassocdr
   #:firstn
   #:powerset
   #:efface
   #:pop-assoc
   #:mapply
   #:car-safe
   #:cdr-safe
   #:mapcar-into
   #:nthrest
   #:plist-keys
   #:plist-values
   #:stable-set-difference
   #:intersectionp
   #:append-longest
   #:mappend-longest
   ;; Strings.
   #:ascii-char-p
   #:whitespace
   #:collapse-whitespace
   #:whitespacep
   #:blankp
   #:trim-whitespace
   #:concat
   #:mapconcat
   #:string-join
   #:string-upcase-initials
   #:nstring-upcase-initials
   #:same-case-p
   #:string-invert-case
   #:nstring-invert-case
   #:words
   #:tokens
   #:word-wrap
   #:lines
   #:fmt
   #:with-string
   #:escape
   #:ellipsize
   #:string^=
   #:string$=
   #:string~=
   #:string*=
   #:string-prefix-p
   #:string-suffix-p
   #:string-contains-p
   #:string-token-p
   #:string-replace
   #:string-replace-all
   #:chomp
   #:string-count
   #:string+
   #:pad-start
   #:pad-end
   ;; Sequences.
   #:sequencep
   #:do-each
   #:keep
   #:filter
   #:filterf
   #:partition
   #:partitions
   #:runs
   #:batches
   #:assort
   #:single
   #:only-elt
   #:frequencies
   #:scan
   #:nub
   #:gcp
   #:gcs
   #:of-length
   #:length<
   #:length<=
   #:length>
   #:length>=
   #:longer
   #:shorter
   #:longest
   #:shortest
   #:ordering
   #:bisect-left
   #:bisect-right
   #:bestn
   #:nth-best
   #:nth-best!
   #:reshuffle
   #:sort-new
   #:stable-sort-new
   #:extrema
   #:nsubseq
   #:slice
   #:take
   #:drop
   #:halves
   #:dsu-sort
   #:dsu-sort-new
   #:deltas
   #:intersperse
   #:toposort
   #:inconsistent-graph
   #:inconsistent-graph-constraints
   #:mvfold
   #:mvfoldr
   #:repeat-sequence
   #:split-sequence
   #:split-sequence-if
   #:split-sequence-if-not
   #:take-while
   #:take-until
   #:drop-while
   #:drop-until
   #:drop-prefix
   #:ensure-prefix
   #:drop-suffix
   #:ensure-suffix
   #:seq=
   #:tree-case
   #:tree-ecase
   #:char-case
   #:char-ecase
   #:do-splits
   #:collapse-duplicates
   #:same
   #:copy-firstn
   #:splice-seq
   #:nsplice-seq
   #:splice-seqf
   #:nsplice-seqf
   #:null-if-empty
   ;; Generalized arrays.
   #:shape
   #:reshape
   #:ravel
   #:sum
   #:prod
   ;; Reader.
   #:with-standard-input-syntax
   ;; Packages.
   #:package-exports
   #:package-names
   #:package-name-keyword
   #:find-external-symbol
   #:export-only
   #:export-only-always
   #:export-always
   ;; Units.
   #:si-prefix
   #:human-size-formatter
   #:format-human-size))

#+sb-package-locks
(sb-ext:lock-package :serapeum)

#+sb-package-locks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:add-implementation-package
   :serapeum
   :serapeum/types))

(defpackage #:serapeum-user
  (:use #:cl #:alexandria #:serapeum))

(uiop:define-package :serapeum/bundle
  (:use-reexport
   :serapeum
   :alexandria
   :split-sequence
   :parse-number))
