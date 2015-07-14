;;;; package.lisp

(defpackage #:serapeum
  (:use :cl :alexandria :optima :split-sequence :parse-number
   :named-readtables :tcr.parse-declarations-1.0
   :introspect-environment)
  (:documentation "Utilities beyond Alexandria.")
  #+sb-package-locks (:lock t)
  (:export
   ;; Macro tools.
   #:unsplice
   #:string-gensym
   #:with-thunk
   #:expand-macro
   #:expand-macro-recursively
   #:partition-declarations
   #:callf
   #:define-do-macro
   #:define-post-modify-macro
   ;; Types.
   #:->
   #:assure
   #:assuref
   #:wholenum
   ;; Definitions.
   #:defconst
   #:defsubst
   #:defalias
   #:def
   #:defplace
   #:defcondition
   #:local
   ;; Binding.
   #:letrec
   #:letrec*
   #:mvlet
   #:mvlet*
   #:lret
   #:lret*
   #:and-let*
   ;; Control flow.
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
   #:string-case
   #:string-ecase
   #:case-using
   #:select
   #:selector
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
   ;; Threads.
   #:synchronized
   #:monitor
   ;; Iteration.
   #:nlet
   #:collecting
   #:summing
   ;; Conditions.
   #:ignoring
   #:maybe-invoke-restart
   ;; Op.
   #:op
   ;; Functions.
   #:flip
   #:nth-arg
   #:distinct
   #:throttle
   #:juxt
   #:dynamic-closure
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
   #:queue-empty-p
   #:clear-queue
   ;; Boxes.
   #:box
   #:unbox
   ;; Vectors.
   #:vect
   #:vector=
   ;; Numbers.
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
   #:standard/context
   #:defmethods
   ;; Hooks.
   #:*hook*
   #:add-hook
   #:remove-hook
   #:run-hooks
   #:run-hook-with-args
   #:run-hook-with-args-until-failure
   #:run-hook-with-args-until-success
   ;; Environment.
   #:with-timing
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
   #:lines
   #:fmt
   #:with-string
   #:escape
   #:ellipsize
   #:string^=
   #:string$=
   #:string~=
   #:string*=
   #:string-prefixp
   #:string-suffixp
   #:string-containsp
   #:string-tokenp
   #:string-replace-all
   ;; Sequences.
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
   #:length<
   #:length<=
   #:length>
   #:length>=
   #:longer
   #:longest
   #:ordering
   #:bestn
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
   #:split-sequence
   #:split-sequence-if
   #:split-sequence-if-not))
