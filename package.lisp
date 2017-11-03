;;;; package.lisp

(defpackage #:serapeum
  (:use :cl :alexandria :split-sequence :parse-number
    :named-readtables :tcr.parse-declarations-1.0
    :introspect-environment)
  (:import-from :trivia :match :ematch)
  (:documentation "Utilities beyond Alexandria.")
  #+sb-package-locks (:lock t)
  (:export
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
   ;; Types.
   #:supertypep
   #:proper-subtype-p
   #:proper-supertype-p
   #:->
   #:assure
   #:assuref
   #:wholenum
   #:tuple
   #:with-type-dispatch
   #:vref
   #:with-subtype-dispatch
   #:with-string-dispatch
   #:with-vector-dispatch
   #:true
   ;; Definitions.
   #:defconst
   #:defsubst
   #:defalias
   #:def
   #:define-values
   #:defplace
   #:defcondition
   #:defstruct-read-only
   #:defconstructor
   #:deconstruct
   #:defvar-unbound
   #:local
   #:local*
   #:block-compile
   #:defloop
   ;; Binding.
   #:receive
   #:letrec
   #:letrec*
   #:mvlet
   #:mvlet*
   #:lret
   #:lret*
   #:and-let*
   ;; Control flow.
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
   #:dispatch-case-let
   #:destructuring-case-of
   #:destructuring-ccase-of
   #:destructuring-ecase-of
   #:string-case
   #:string-ecase
   #:case-using
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
   ;; Threads.
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
   ;; Functions.
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
   #:throttle
   #:trampoline
   ;; Trees.
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
   #:set-hash-table
   #:hash-table-set
   #:hash-table-predicate
   #:hash-table-function
   #:make-hash-table-function
   #:delete-from-hash-table
   ;; Pathnames.
   #:path-join
   #:write-stream-into-file
   #:file=
   #:file-size
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
   #:front
   #:qlen
   #:qlist
   #:qconc
   #:qappend
   #:queue-empty-p
   #:clear-queue
   ;; Boxes.
   #:box
   #:unbox
   ;; Vectors.
   #:vect
   #:vector=
   ;; Numbers.
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
   ;; Time.
   #:universal-to-unix
   #:unix-to-universal
   #:get-unix-time
   #:date-leap-year-p
   #:time-since
   #:time-until
   #:interval
   ;; Classes.
   #:class-name-safe
   #:find-class-safe
   #:make
   #:defmethods
   ;; MOP.
   #:standard/context
   #:topmost-object-class
   ;; Hooks.
   #:*hook*
   #:add-hook
   #:remove-hook
   #:run-hooks
   #:run-hook-with-args
   #:run-hook-with-args-until-failure
   #:run-hook-with-args-until-success
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
   #:assocdr
   #:assocadr
   #:rassocar
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
   ;; Sequences.
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
   #:longest
   #:ordering
   #:bestn
   #:nth-best
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
   #:drop-while
   #:seq=
   #:tree-case
   #:tree-ecase
   #:char-case
   #:char-ecase))

(defpackage #:serapeum-user
  (:use #:cl #:alexandria #:serapeum))
