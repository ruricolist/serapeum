;;;; package.lisp
(defpackage #:serapeum.sum
  (:use)
  ;; Create this in another package than SERAPEUM
  ;; to prevent SBCL package locking from keeping
  ;; SUM being defined in a FLET
  (:export #:sum))

(defpackage #:serapeum
  (:use :cl :alexandria :split-sequence :parse-number
        :tcr.parse-declarations-1.0)
  (:import-from :introspect-environment
   :compiler-macroexpand :compiler-macroexpand-1
   :constant-form-value :typexpand :typexpand-1)
  (:import-from :trivia :match :ematch :defpattern)
  (:import-from :trivial-file-size :file-size-in-octets)
  (:import-from :serapeum.sum :sum)
  (:documentation "Utilities beyond Alexandria.")
  #+sb-package-locks (:lock t)
  (:export
   ;; Portability.
   #:static-load-time-value
   #:static-load-time-value-error
   #:no-applicable-method-error
   ;; Macro tools.
   #:unsplice
   #:string-gensym
   #:unique-name
   #:unique-name-list
   #:with-thunk
   #:expand-macro
   #:expand-macro-recursively
   #:partition-declarations
   #:callf #:callf2
   #:define-do-macro
   #:define-post-modify-macro
   #:case-failure
   #:define-case-macro
   #:eval-if-constant
   #:parse-leading-keywords
   #:with-read-only-vars
   #:+merge-tail-calls+
   #:expect-form-list
   #:expect-single-form
   #:unparse-ordinary-lambda-list
   #:parse-method-args
   ;; Types.
   #:supertypep
   #:proper-subtype-p
   #:proper-supertype-p
   #:->
   #:assure
   #:assuref
   #:wholenum
   #:tuple
   #:input-stream
   #:output-stream
   #:with-type-dispatch
   #:vref
   #:*boolean-bypass*
   #:with-boolean #:boolean-if #:boolean-when #:boolean-unless
   #:with-subtype-dispatch
   #:with-string-dispatch
   #:with-vector-dispatch
   #:with-simple-vector-dispatch
   #:true
   #:soft-list-of
   #:soft-alist-of
   #:with-item-key-function
   #:with-two-arg-test
   #:with-member-test
   ;; Definitions.
   #:defconst
   #:defsubst
   #:defalias
   #:def
   #:define-values
   #:defplace
   #:defvar-unbound
   #:defloop
   ;; Defining types.
   #:read-eval-prefix
   #:defcondition
   #:defstruct-read-only
   #:defconstructor
   #:deconstruct
   #:defunit
   #:defunion
   #:match-of
   ;; Internal definitions.
   #:local
   #:local*
   #:block-compile
   ;; Binding.
   #:receive
   #:letrec
   #:letrec*
   #:mvlet
   #:mvlet*
   #:lret
   #:lret*
   #:and-let*
   #:recklessly-continue
   #:static-binding-flush-error
   #:static-binding-flush-error-group
   #:static-binding-flush-error-all-groups-p
   #:static-binding-active-error
   #:flush-static-binding-group
   #:flush-all-static-binding-groups
   #:static-let
   #:static-let*
   ;; Control flow.
   #:null-if
   #:eval-always
   #:eval-and-compile
   #:no
   #:nor
   #:nand
   #:typecase-of
   #:case-of
   #:etypecase-of
   #:ecase-of
   #:ctypecase-of
   #:ccase-of
   #:dispatch-case
   #:dispatch-caseql
   #:dispatch-case-let
   #:dispatch-caseql-let
   #:dispatch-case-error
   #:destructuring-case-of
   #:destructuring-ccase-of
   #:destructuring-ecase-of
   #:string-case
   #:string-ecase
   #:case-using
   #:ecase-using
   #:select
   #:selector
   #:eif
   #:eif-let
   #:econd
   #:econd-failure
   #:econd-let
   #:ecase-let
   #:cond-let
   #:case-let
   #:bcond
   #:comment
   #:example
   #:nix
   #:ensure
   #:ensure2
   #:~>
   #:~>>
   #:cond-every
   #:nest
   #:sort-values
   #:eq* #:eql* #:equal* #:equalp*
   #:recursion-forbidden
   #:without-recursion
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
   #:maybe-invoke-restart
   ;; Op.
   #:op
   #:opf
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
   #:file-pathname
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
   #:undisplace-array
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
   ;; Boxes.
   #:box
   #:unbox
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
   #:fdec
   #:shrink
   #:grow
   #:shrinkf
   #:growf
   #:random-in-range
   #:float-precision-contagion
   #:range
   ;; Octets.
   #:octet
   #:octet-vector
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

(defpackage #:serapeum-user
  (:use #:cl #:alexandria #:serapeum))

(uiop:define-package :serapeum/bundle
  (:use-reexport
   :serapeum
   :alexandria
   :split-sequence
   :parse-number))
