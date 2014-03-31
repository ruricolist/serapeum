# Macro Tools

## `(string-gensym x)`

Equivalent to (gensym (string x)).

Generally preferable to calling GENSYM with a string, because it
respects the current read table.

The alternative to writing `(mapcar (compose #'gensym #'string) ...)'
in every other macro.

## `(unsplice form)`

If FORM is non-nil, wrap it in a list.

This is useful with ,@ in macros, and with `mapcan`.

From Lparallel.

## `(with-thunk (var &rest args) &body body)`

A macro-writing macro for the `call-with-` style.

In the `call-with-` style of writing macros, the macro is simply a
syntactic convenience that wraps its body in a thunk and a call to the
function that does the actual work.

    (defmacro with-foo (&body body)
      `(call-with-foo (lambda () ,@body)))

The `call-with-` style has many advantages. Functions are easier to
write than macros; you can change the behavior of a function without
having to recompile all its callers; functions can be traced, appear
in backtraces, etc.

But meanwhile, all those thunks are being allocated on the heap. Can
we avoid this? Yes, but at a high cost in boilerplate: the closure has
to be given a name (using `flet`) so it can be declared
`dynamic-extent`.

    (defmacro with-foo (&body body)
      (with-gensyms (thunk)
        `(flet ((,thunk () ,@body))
           (declare (dynamic-extent #',thunk))
           (call-with-foo #',thunk))))

`with-thunk` avoids the boilerplate:

    (defmacro with-foo (&body body)
      (with-thunk (body)
        `(call-with-foo #',body)))

It is also possible to construct a "thunk" with arguments.

    (with-thunk (body foo)
      `(call-with-foo #',body))
    ≡ `(flet ((,thunk (,foo)
          ,@body))
        (declare (dynamic-extent #',thunk))
        (call-with-foo #',thunk))

Needs a better name.

## `(expand-macro form &optional env)`

Like `macroexpand-1`, but also expand compiler macros.
From Swank.

## `(expand-macro-recursively form &optional env)`

Like `macroexpand`, but also expand compiler macros.
From Swank.

## `(parse-declarations declarations)`

Pick apart a list of `declare` forms.

Parse DECLARATIONS into an alist of (identifier . declarations).
Declarations should be a list like ((declare ...) ...), as would be
returned by `alexandria:parse-body`.

Declarations that are specific to functions are normalized to
use `(function ,identifier).

Type declarations are normalized to the form `(type ,type).

Ftype declarations are also normalized.

     (parse-declarations
      '((declare
         (fixnum x)
         (type list xs)
         (ftype (-> list fixnum) frob)
         (inline frob)
         (dynamic-extent #'frob))))
     => '((#'frob dynamic-extent inline (ftype (-> list fixnum)))
          (xs (type list))
          (x (type fixnum)))

Return any optimizations declared as a second value.

## `(expand-declaration decl)`

Opposite of `parse-declarations`.

Take a (identifier . declarations) pair, as returned by
`parse-declarations`, and turn it into a declaration form that can be
used in Lisp code.

     (locally ,(expand-declaration decl) ...)

Might be used to transfer declarations made for a variable to another,
temporary variable.

## `(partition-declarations xs declarations)`

Split DECLARATIONS into those that do and do not apply to XS.
Return two values, one with each set.

Both sets of declarations are returned in a form that can be spliced
directly into Lisp code:

     (locally ,@(partition-declarations vars decls) ...)

# Types

## `(-> function args values)`

Declaim the ftype of a function from ARGS to VALUES.

     (-> mod-fixnum+ (fixnum fixnum) fixnum)
     (defun mod-fixnum+ (x y) ...)

# Definitions

## `(def var &body (val &optional (doc nil docp)))`

The famous "deflex".

Define a top level (global) lexical VAR with initial value VAL,
which is assigned unconditionally as with DEFPARAMETER. If a DOC
string is provided, it is attached to both the name |VAR| and the name
*STORAGE-FOR-DEFLEX-VAR-|VAR|* as a documentation string of kind
'VARIABLE. The new VAR will have lexical scope and thus may be
shadowed by LET bindings without affecting its dynamic (global) value.

In implementations that support it (SBCL and CCL, at the moment) this
version creates a backing variable that is "global" or "static",
so there is not just a change in semantics, but also a gain in
efficiency.

The original `deflex` is due to Rob Warnock.

## `(defconst symbol init &optional docstring)`

Define a constant, lexically.

`defconst` defines a constant using a strategy similar to `def`, so
you don’t have to +cage+ your constants.

The name is from Emacs Lisp.

## `(defsubst name params &body body)`

Define an inline function.

     (defsubst fn ...)
     ≡ (declaim (inline fn))
       (defun fn ...)

The advantage of a separate defining form for inline functions is that
you can't forget to declaim the function inline before defining it –
without which it may not actually end up being inlined.

From Emacs and other ancient Lisps.

## `(defalias alias &body (def &optional docstring))`

Define a value as a top-level function.

     (defalias string-gensym (compose #'gensym #'string))

Like (setf (fdefinition ALIAS) DEF), but with a place to put
documentation and some niceties to placate the compiler.

Name from Emacs Lisp.

## `(defplace name args &body (form &optional docstring))`

Define NAME and (SETF NAME) in one go.

Note that the body must be a single, setf-able expression.

## `(defcondition name supers &body (slots &rest options))`

Alias for `define-condition`.

Like (define-condition ...), but blissfully conforming to the same
nomenclatural convention as every other definition form in Common
Lisp.

# Binding

## `(lret (&rest bindings) &body body)`

Return the initial value of the last binding in BINDINGS. The idea
is to create something, initialize it, and then return it.

`lret` may seem trivial, but it fufills the highest purpose a macro
can: it eliminates a whole class of bugs (initializing an object, but
forgetting to return it).

Cf. `aprog1` in Anaphora.

## `(lret* (&rest bindings) &body body)`

Cf. `lret`.

## `(letrec (&rest bindings) &body body)`

Recursive LET.
The idea is that functions created in BINDINGS can close over one
another, and themselves.

Note that `letrec` only binds variables: it can define functions, but
can't bind them as functions. (But see `fbindrec`.)

## `(letrec* (&rest bindings) &body body)`

Like LETREC, but the bindings are evaluated in order.
See Waddell et al., *Fixing Letrec* for motivation.

Cf. `fbindrec*`.

## `(mvlet* (&rest bindings) &body body)`

Expand a series of nested `multiple-value-bind` forms.

`mvlet*` is similar in intent to Scheme’s `let-values`, but with a
different and less parenthesis-intensive syntax. Each binding is a
list of

    (var var*... expr)

A simple example should suffice to show both the implementation and
the motivation:

    (defun uptime (seconds)
      (mvlet* ((minutes seconds (truncate seconds 60))
               (hours minutes (truncate minutes 60))
               (days hours (truncate hours 24)))
        (declare ((integer 0 *) days hours minutes seconds))
        (fmt "~d day~:p, ~d hour~:p, ~d minute~:p, ~d second~:p"
             days hours minutes seconds)))

Note that declarations work just like `let*`.

## `(mvlet (&rest bindings) &body body)`

Parallel (`let`-like) version of `mvlet*`.

## `(and-let* (&rest clauses) &body body)`

Scheme's guarded LET* (SRFI-2).

Each clause should have one of the following forms:

`identifier`
    in which case identifier's value is tested.
`(expression)`
    in which case the value of expression is tested.
`(identifier expression)'
    in which case expression is evaluated, and, if its value is not
    false, identifier is bound to that value for the remainder of the
    clauses and the optional body.

Note of course that the semantics are different in Common Lisp,
because our AND short-circuits on null, not false.

# Control Flow

## `(eval-and-compile &body body)`

Emacs's `eval-and-compile`.

Shorthand for
        (eval-when (:compile-toplevel :load-toplevel :execute) ...)

## `(no x)`

Another alias for `not` and `null`.

From Arc.

## `(nor &rest forms)`

Equivalent to (not (or ...)).

From Arc.

## `(case-using pred keyform &body clauses)`

ISLISP's case-using.

     (case-using #'eql x ...)
     ≡ (case x ...).

Note that, no matter the predicate, the keys are not evaluated.

This version supports both single-item clauses (x ...) and
multiple-item clauses ((x y) ...), as well as (t ...) for the default
clause.

## `(string-case stringform &body cases)`

Efficient `case`-like macro with string keys.

This uses Paul Khuong's `string-case` macro internally.

## `(string-ecase stringform &body cases)`

Efficient `ecase`-like macro with string keys.

Cf. `string-case`.

## `(econd &rest clauses)`

Like `cond`, but signal an error of type `econd-failure` if no
clause succeeds.

## `(cond-let var &body clauses)`

Cross between COND and LET.

     (cond-let x ((test ...)))
     ≡ (let (x)
         (cond ((setf x test) ...)))

Cf. `acond` in Anaphora.

## `(econd-let symbol &rest clauses)`

Like `cond-let` for `econd`.

## `(cond-every &body clauses)`

Like `cond`, but instead of stopping after the first clause that
succeeds, run all the clauses that succeed.

Return the value of the last successful clause.

If a clause begins with `cl:otherwise`, it runs only if no preceding
form has succeeded.

From Zetalisp.

## `(case-let (var expr) &body cases)`

Like (let ((VAR EXPR)) (case VAR ...))

## `(ecase-let (var expr) &body cases)`

Like (let ((VAR EXPR)) (ecase VAR ...))

## `(comment &body body)`

A macro that ignores its body and does nothing. Useful for
comments-by-example.

Also, as noted in EXTENSIONS.LISP of 1992, "This may seem like a
silly macro, but used inside of other macros or code generation
facilities it is very useful - you can see comments in the (one-time)
macro expansion!"

## `(example &body body)`

Like `comment`.

## `(nix place)`

Set PLACE to nil and return the old value of PLACE.

This may be more efficient than (shiftf place nil), because it only
sets PLACE when it is not already null.

## `(ensure place &body newval)`

Essentially (or place (setf place newval)).

PLACE is treated as unbound if it returns `nil`, signals
`unbound-slot`, or signals `unbound-variable`.

Note that ENSURE is `setf`-able, so you can do things like
     (incf (ensure x 0))

Cf. `ensure2`.

## `(ensure2 place &body newval)`

Like `ensure`, but specifically for accessors that return a second
value like `gethash`.

## `(callf function place &rest args)`

Set PLACE to the value of calling FUNCTION on PLACE, with ARGS.

## `(callf2 function arg1 place &rest args)`

Like CALLF, but with the place as the second argument.

## `(~> needle &rest holes)`

Threading macro from Clojure (by way of Racket).

Thread NEEDLE through HOLES, where each hole is either a
symbol (equivalent to `(hole needle)`) or a list (equivalent to `(hole
needle args...)`).

As an extension, an underscore in the argument list is replaced with
the needle, so you can pass the needle as an argument other than the
first.

## `(~>> needle &rest holes)`

Like `~>` but, by default, thread NEEDLE as the last argument
instead of the first.

## `(select keyform &body clauses)`

Like `case`, but with evaluated keys.

Note that, like `case`, `select` interprets a list as the first
element of a clause as a list of keys. To use a form as a key, you
must add an extra set of parentheses.

     (select 2
       ((+ 2 2) t))
     => T

     (select 4
       (((+ 2 2)) t))
     => T

This version of `select` is smart enough to compile itself to a `case`
statement if all its keys are constants.

From Zetalisp.

## `(selector keyform fn &body clauses)`

Like `select`, but compare using FN.

Note that (unlike `case-using`), FN is not evaluated.

From Zetalisp.

## `(atomic &body body)`

Run BODY as an anonymous critical section.

Only one thread can run BODY at a time.

# Iter

## `(nlet name (&rest bindings) &body body)`

Within BODY, bind NAME as a function, somewhat like LABELS, but
with the guarantee that recursive calls to NAME will not grow the
stack.

`nlet` resembles Scheme’s named let, and is used for the same purpose:
writing loops using tail recursion. You could of course do this with
`labels` as well, at least under some Lisp implementations, but `nlet`
guarantees tail call elimination anywhere and everywhere.

    (nlet rec ((i 1000000))
      (if (= i 0)
          0
          (rec (1- i))))
    => 0

Beware: because of the way it is written (literally, a GOTO with
arguments), `nlet` is limited: self calls must be tail calls. That is,
you cannot use `nlet` for true recursion.

The name comes from `Let Over Lambda', but this is a different
implementation: the function is not bound while the initial arguments
are being evaluated, and it is safe to close over the arguments.

## `(collecting &body body)`

Within BODY, bind `collect` to a function of one arguments that
accumulate all the arguments it has been called with in order, like
the collect clause in `loop`, finally returning the collection.

To see the collection so far, call `collect` with no arguments.

Note that this version of `collecting` binds `collect` to a closure,
not a macro: you can pass the collector around or return it like any
other function.

## `(summing &body body)`

Within BODY, bind `sum` to a function that gathers numbers to sum.

Return the total.

## `(minimizing &body body)`

Within BODY, bind `minimize` to a function that tracks its least argument.

Return the minimum.

## `(maximizing &body body)`

Within BODY, bind `maximize` to a function that tracks its greatest argument.

Return the maximum.

# Conditions

## `(ignoring type &body body)`

An improved version of `ignore-errors`.

The behavior is the same: if an error occurs in the body, the form
returns two values, `nil` and the condition itself.

`ignoring` forces you to specify the kind of error you want to ignore:

    (ignoring parse-error
      ...)

I call it an improvement because I think `ignore-errors` is too broad:
by hiding all errors it becomes itself a source of bugs.

Of course you can still ignore all errors, at the cost of one extra
character:

    (ignoring error
      ...)

NB `(ignoring t)` is a bad idea.

# Op

## `(op &body body)`

GOO's simple macro for positional lambdas.

An OP is like a lambda without an argument list. Within the body of the OP
form, an underscore introduces a new argument.

     (reduce (op (set-intersection _ _ :test #'equal))
             sets)

You can refer back to each argument by number, starting with _1.

     (funcall (op (+ _ _1)) 2) => 4

You can also use positional arguments directly:

     (reduce (op (funcall _2 _1)) ...)

Argument lists can be sparse:

     (apply (op (+ _1 _3 _5)) '(1 2 3 4 5)) => 9

Note that OP with a single argument is equivalent to CONSTANTLY:

     (funcall (op 1)) => 1

and that OP with a single placeholder is equivalent to IDENTITY:

     (funcall (op _) 1) => 1

OP can also be used to define variadic functions by using _* as the
placeholder. It is not necessary to use APPLY.

     (apply (op (+ _*)) '(1 2 3 4)) => 10

# Fbind

## `(fbind bindings &body body)`

Binds values in the function namespace.

That is,
     (fbind ((fn (lambda () ...))))
     ≡ (flet ((fn () ...))),

except that a bare symbol in BINDINGS is rewritten as (symbol
symbol).

## `(fbind* bindings &body body)`

Like `fbind`, but creates bindings sequentially.

## `(fbindrec bindings &body body)`

Like `fbind`, but creates recursive bindings.

The consequences of referring to one binding in the expression that
generates another are undefined.

## `(fbindrec* bindings &body body)`

Like `fbindrec`, but the function defined in each binding can be
used in successive bindings.

# Functions

## `(partial fn &rest args)`

Alias for `alexandria:curry`.

## `(flip f)`

Flip around the arguments of a binary function.

That is, given a binary function, return another, equivalent function
that takes its two arguments in the opposite order.

From Haskell.

## `(nth-arg n)`

Return a function that returns only its NTH argument, ignoring all others.

If you've ever caught yourself trying to do something like

    (mapcar #'second xs ys)

then `nth-arg` is what you need.

If `hash-table-keys` were not already defined by Alexandria, you could
define it thus:

    (defun hash-table-keys (table)
      (maphash-return (nth-arg 0) table))

## `(distinct &key key test)`

Return a function that echoes only values it has not seen before.

    (defalias test (distinct))
    (test 'foo) => foo, t
    (test 'foo) => nil, nil

The second value is T when the value is distinct.

TEST must be a valid test for a hash table.

This has many uses, for example:

    (count-if (distinct) seq)
    ≡ (length (remove-duplicates seq))

## `(throttle fn wait &key synchronized)`

Wrap FN so it can be called no more than every WAIT seconds.
If FN was called less than WAIT seconds ago, return the values from the
last call. Otherwise, call FN normally and update the cached values.

WAIT, of course, may be a fractional number of seconds.

The throttled function is not thread-safe by default; use SYNCHRONIZED
to get a version with a lock.

## `(juxt &rest fns)`

Clojure's `juxt`.

Return a function of one argument, which, in turn, returns a list
where each element is the result of applying one of FNS to the
argument.

It’s actually quite simple, but easier to demonstrate than to explain.
The classic example is to use `juxt` to implement `partition`:

    (defalias partition* (juxt #'filter #'remove-if))
    (partition* #'evenp '(1 2 3 4 5 6 7 8 9 10))
    => '((2 4 6 8 10) (1 3 5 7 9))

The general idea is that `juxt` takes things apart.

## `(dynamic-closure symbols fn)`

Create a dynamic closure.

Some ancient Lisps had closures without lexical binding. Instead, you
could "close over" pieces of the current dynamic environment. When
the resulting closure was called, the symbols closed over would be
bound to their values at the time the closure was created. These
bindings would persist through subsequent invocations and could be
mutated. The result was something between a closure and a
continuation.

This particular piece of Lisp history is worth reviving, I think, if
only for use with threads. For example, to start a thread and
propagate the current value of `*standard-output*`:

     (bt:make-thread (dynamic-closure '(*standard-output*) (lambda ...)))
     = (let ((temp *standard-output*))
         (bt:make-thread
          (lambda ...
            (let ((*standard-output* temp))
              ...))))

# Lists

## `(filter-map fn list &rest lists)`

Map FN over LISTS like `mapcar`, but omit empty results.

     (filter-map fn ...)
     ≅ (remove nil (mapcar fn ...))

## `(car-safe x)`

The car of X, or nil if X is not a cons.

This is different from Alexandria’s `ensure-car`, which returns the atom.

    (ensure-car '(1 . 2)) => 1
    (car-safe '(1 . 2)) => 1
    (ensure-car 1) => 1
    (car-safe 1) => nil

From Emacs Lisp.

## `(cdr-safe x)`

The cdr of X, or nil if X is not a cons.
From Emacs Lisp.

## `(append1 list item)`

Append an atom to a list.

    (append1 list item)
    ≡ (append list (list item))

## `(in x &rest items)`

Is X equal to any of ITEMS?

`(in x xs...)` is always equivalent to `(member x xs :test equal)`,
but `in` can sometimes compile to more efficient code when the
candidate matches are constant.

From Arc.

## `(memq item list)`

Like (member ... :test #'eq).
Should only be used for symbols.

## `(delq item list)`

Like (delete ... :test #'eq), but only for lists.

Almost always used as (delq nil ...).

## `(zip &rest lists)`

Like `pairlis`, but for an arbitrary number of lists.

## `(unzip &rest lists)`

Inverse of `zip`.

## `(riffle &rest lists)`

`riffle` is like `zip`, but using `mappend` instead of `mapcar`.

    (riffle '(1 2 3) '(4 5 6))
    => '(1 4 2 5 3 6)

## `(mapply fn &rest lists)`

`mapply` is a cousin of `mapcar`.

If you think of `mapcar` as using `funcall`:

    (mapcar #'- '(1 2 3))
    ≡ (loop for item in '(1 2 3)
             collect (funcall #'- item))

Then `mapply` does the same thing, but using `apply`.

    (mapply #'+ '((1 2 3) (4 5 6)))
    => (6 15)

In variadic use, `mapply` acts as if `mapcar #'append` had first been
used:

    (mapply #'+ xs ys)
    ≡ (mapply #'+ (mapcar #'append xs ys))

But the actual implementation is more efficient.

## `(assocdr item alist &rest args)`

Like (cdr (assoc ...))

## `(assocadr item alist &rest args)`

Like `assocdr` for alists of proper lists.

     (assocdr 'x '((x 1))) => '(1)
     (assocadr 'x '((x 1))) => 1

## `(rassocar item alist &rest args)`

Like (car (rassoc ...))

## `(firstn n list)`

The first N elements of LIST, as a fresh list:

    (firstn 4 (iota 10))
    => (0 1 2 4)

(I do not why this extremely useful function did not make it into
Common Lisp, unless it was deliberately left out as an exercise for
Maclisp users.)

## `(inconsistent-graph-constraints x)`

The constraints of an `inconsistent-graph` error.
Cf. `toposort`.

## `(toposort elts constraints &key key tie-breaker)`

The topographical sort routine from AMOP.

Takes a list of elements to sort, and a list of constraints, where each
constraint is a two-element list.

    (def dem-bones '((toe foot)
                     (foot heel)
                     (heel ankle)
                     (ankle shin)
                     (shin knee)
                     (knee back)
                     (back shoulder)
                     (shoulder neck)
                     (neck head)))
    (toposort (shuffle (mapcar #'car dem-bones))
              dem-bones)
    => (TOE FOOT HEEL ANKLE SHIN KNEE BACK SHOULDER NECK)

If the graph is inconsistent, signals an error of type
`inconsistent-graph`:

    (toposort '(chicken egg) '((chicken egg) (egg chicken)))
    => Inconsistent graph: ((CHICKEN EGG) (EGG CHICKEN))

## `(powerset set)`

Return the powerset of SET.
Uses a non-recursive algorithm.

## `(efface item list)`

Destructively remove only the first occurence of ITEM in LIST.

From Lisp 1.5.

## `(pop-assoc key alist &rest args)`

Like `assoc` but, if there was a match, delete it from ALIST.

From Newlisp.

## `(mapcar-into fn list)`

Like (map-into list fn list).

From PAIP.

## `(nthrest n list)`

Alias for `nthcdr`.

## `(deltas list &optional op)`

Return the successive differences in LIST.

     (deltas '(4 9 -5 1 2))
     => '(4 5 -14 6 1)

From Q.

# Trees

## `(walk-tree fun tree &optional tag)`

Call FUN in turn over each atom and cons of TREE.

FUN can skip the current subtree with (throw TAG nil).

## `(map-tree fun tree &optional tag)`

Walk FUN over TREE and build a tree from the results.

The new tree may share structure with the old tree.

     (eq tree (map-tree #'identity tree)) => T

FUN can skip the current subtree with (throw TAG SUBTREE), in which
case SUBTREE will be used as the value of the subtree.

## `(leaf-walk fun tree)`

Call FUN on each leaf of TREE.

## `(leaf-map fn tree)`

Call FN on each leaf of TREE.
Return a new tree possibly sharing structure with TREE.

## `(occurs-if test tree &key key)`

Is there a node (leaf or cons) in TREE that satisfies TEST?

## `(prune-if test tree &key key)`

Remove any atoms satisfying TEST from TREE.

## `(occurs leaf tree &key key test)`

Is LEAF present in TREE?

## `(prune leaf tree &key key test)`

Remove LEAF from TREE wherever it occurs.

# Strings

## `(whitespacep char)`

Is CHAR whitespace?

Spaces, tabs, any kind of line break, page breaks, and no-break spaces
are considered whitespace.

## `(trim-whitespace string)`

STRING without whitespace at ends.

## `(collapse-whitespace string)`

Collapse runs of whitespace in STRING.
Each run of space, newline, and other whitespace characters is
replaced by a single space character.

## `(blankp seq)`

SEQ is either empty, or consists entirely of characters that
satisfy `whitespacep`.

## `(concat &rest strings)`

Abbreviation for (concatenate 'string ...).

From Emacs Lisp.

## `(mapconcat fun seq separator)`

Build a string by mapping FUN over SEQ.
Separate each value with SEPARATOR.

Equivalent to
        (reduce #'concat (intersperse SEP SEQ) :key FUN)
but more efficient.

From Emacs Lisp.

## `(join strings &optional sep)`

Join STRINGS into one string, perhaps interspersing with SEP.

## `(string-upcase-initials string)`

Return STRING with the first letter of each word capitalized.
This differs from CAPITALIZE in that the other characters in each word
are not changed.

     (capitalize "an ACRONYM") -> "An Acronym")
     (string-upcase-initials "an ACRONYM") -> "An ACRONYM")

From Emacs Lisp (where it is simply `upcase-initials`).

## `(nstring-upcase-initials string)`

Destructive version of `string-upcase-initials`.

## `(nstring-invert-case string)`

Destructive version of `string-invert-case`.

## `(string-invert-case string)`

Invert the case of STRING.
This does the same thing as a case-inverting readtable.

## `(words string &key start end)`

Split STRING into words.

The definition of a word is the same as that used by
`string-capitalize`: a run of alphanumeric characters.

    (words "Four score and seven years")
    => ("Four" "score" "and" "seven" "years")

    (words "2 words")
    => ("2" "words")

    (words "two_words")
    => ("two" "words")

    (words "\"I'm here,\" Tom said presently.")
    => ("I" "m" "here" "Tom" "said" "presently")

Cf. `tokens`.

## `(tokens string &key start end)`

Separate STRING into tokens.
Tokens are runs of non-whitespace characters.

    (tokens "\"I'm here,\" Tom said presently.")
    => ("\"I'm" "here,\"" "Tom" "said" "presently.")

Cf. `words`.

## `(lines string)`

A list of lines in STRING.

## `(fmt control-string &rest args)`

A cousin of `format` expressly for fast formatting of strings.

Like (format nil ...), binding `*pretty-pretty*` to `nil`, which in
some Lisps means a significant increase in speed.

Has a compiler macro with `formatter`.

## `(downcase x)`

Downcase a string or character.

## `(upcase x)`

Upcase a string or character.

## `(capitalize x)`

Capitalize a string or character.

## `(escape-to-stream string stream table &key start end)`

Write STRING to STREAM, escaping with TABLE.

TABLE should be either a hash table, with characters for keys and
strings for values, or a function that takes a character and returns a
string.

## `(escape string table &key start end)`

Given a STRING and a table of escapes, return another, escaped
string.

From Clojure.

Cf. `escape-to-stream`.

## `(ellipsize string n &key ellipsis)`

If STRING is longer than N, truncate it and append ELLIPSIS.

Note that the resulting string is longer than N by the length of
ELLIPSIS, so the string may come out longer than it started.

     (ellipsize "abc" 2)
     => "ab..."

From Arc.

## `(string-prefixp s1 s2 &key start1 end1 start2 end2)`

Like `string^=`, but case-insensitive.

## `(string^= s1 s2 &key start1 end1 start2 end2)`

Is S1 a prefix of S2?

## `(string-suffixp s1 s2 &key start1 end1 start2 end2)`

Like `string$=`, but case-insensitive.

## `(string$= s1 s2 &key start1 end1 start2 end2)`

Is S1 a suffix of S2?

## `(string-containsp s1 s2 &key start1 end1 start2 end2)`

Like `string*=`, but case-insensitive.

## `(string*= s1 s2 &key start1 end1 start2 end2)`

Is S1 a substring of S2?

This is similar, but not identical, to SEARCH.

     (search nil "foo") => T
     (search "nil" "nil") => NIL
     (string*= nil "foo") => NIL
     (string*= nil "nil") => T

## `(string~= s1 s2 &key start1 end1 start2 end2)`

Does S1 occur in S2 as a token?

This is equivalent to
     (find S1 (tokens S2) :test #'string=),
but without consing.

## `(string-tokenp s1 s2 &key start1 end1 start2 end2)`

Like `string~=`, but case-insensitive.

## `(string-replace-all old string new &key start end)`

Do regex-style search-and-replace for constant strings.

Note that START and END only affect where the replacements are made:
the part of the string before START, and the part after END, are
always included verbatim.

     (string-replace-all "old" "The old old way" "new"
                         :start 3 :end 6)
     => "The new old way"

# Hash Tables

## `(dict &rest keys-and-values)`

A concise constructor for hash tables.

By default, return an 'equal hash table containing each successive
pair of keys and values from KEYS-AND-VALUES.

If the number of KEYS-AND-VALUES is odd, then the first argument is
understood as the test.

     (gethash "string" (dict "string" t)) => t
     (gethash "string" (dict 'eq "string" t)) => nil

## `(href table &rest keys)`

A concise way of doings lookups in (potentially nested) hash tables.

    (href (dict :x 1) :x) => x
    (href (dict :x (dict :y 2)) :x :y)  => y

## `(@ table key &rest keys)`

A concise way of doings lookups in (potentially nested) hash tables.

    (@ (dict :x 1) :x) => x
    (@ (dict :x (dict :y 2)) :x :y)  => y 

## `(pophash key hash-table)`

Lookup KEY in HASH-TABLE, return its value, and remove it.
From Zetalisp.

## `(swaphash key value hash-table)`

Set KEY and VALUE in HASH-TABLE, returning the old values of KEY.
From Zetalisp.

## `(maphash-return fn hash-table)`

Like MAPHASH, but collect and return the values from FN.
From Zetalisp.

## `(merge-tables table &rest tables)`

Merge TABLE and TABLES, working from left to right.
The resulting hash table has the same test as TABLE.

Clojure's `merge`.

## `(flip-hash-table table &key test key)`

Return a table like TABLE, but with keys and values flipped.

TEST filters which values to set. KEY defaults to `identity`.

## `(set-hash-table set &rest hash-table-args &key test key strict &allow-other-keys)`

Return SET, a list considered as a set, as a hash table.
This is the equivalent of `alist-hash-table` and `plist-hash-table`
for a list that denotes a set.

STRICT determines whether to check that the list actually is a set.

The resulting table has the members of SET for its keys and `t` for
every value.

## `(hash-table-set table &key strict)`

Return the set denoted by TABLE.
Given STRICT, check that each value is `t`.

Without STRICT, equivalent to `hash-table-keys`.

# Files

## `(write-stream-into-file stream pathname &key if-exists if-does-not-exist)`

Read STREAM and write the contents into PATHNAME.

STREAM will be closed afterwards, so wrap it with
`make-concatenated-stream` if you want it left open.

## `(file= file1 file2 &key buffer-size)`

Compare FILE1 and FILE2 octet by octet, using buffers of
BUFFER-SIZE.

## `(file-size file &key element-type)`

The size of FILE.

## `(delete-file-if-exists file)`

Delete FILE if it exists.

# Symbols

## `(find-keyword string)`

If STRING has been interned as a keyword, return it.

Like `make-keyword`, but preferable in most cases, because it doesn't
intern a keyword -- which is usually both unnecessary and unwise.

## `(bound-value s &optional default)`

If S is bound, return (values s t). Otherwise, return DEFAULT.

## `(special-variable-p symbol)`

Is SYMBOL a special variable?

# Arrays

## `(array-index-row-major array row-major-index)`

The inverse of ARRAY-ROW-MAJOR-INDEX.

Given an array and a row-major index, return a list of subscripts.

     (apply #'aref (array-index-row-major i))
     ≡ (array-row-major-aref i)

# Queue

## `(queuep x)`

Is X a queue?

## `(queue &rest initial-contents)`

Build a new queue with INITIAL-CONTENTS.

## `(clear-queue queue)`

Return QUEUE's contents and reset it.

## `(qlen queue)`

The number of items in QUEUE.

## `(qlist queue)`

A list of the times in QUEUE.

## `(enq item queue)`

Insert ITEM at end of QUEUE.

## `(deq queue)`

Remove item from the front of the QUEUE.

## `(front queue)`

The first element in QUEUE.

## `(queue-empty-p queue)`

Is QUEUE empty?

## `(qconc queue list)`

Destructively concatenate LIST onto the end of QUEUE.

# Box

## `(box value)`

Box a value.

## `(unbox x)`

The value in the box X.

# Sequences

## `(nsubseq seq start &optional end)`

Return a subsequence that may share structure with SEQ.

Note that `nsubseq` gets its aposematic leading `n` not because it is
itself destructive, but because, unlike `subseq`, destructive
operations on the subsequence returned may mutate the original.

`nsubseq` also works with `setf`, with the same behavior as
`replace`.

## `(filter pred seq &rest args &key count from-end start end key &allow-other-keys)`

Almost the opposite of `remove-if-not`.
The difference is the handling of COUNT.

## `(keep item seq &rest args &key test from-end count &allow-other-keys)`

Almost the opposite of `remove`.
Keep only those items in SEQ that are equivalent, under TEST and KEY,
to ITEM.

The difference is the handling of COUNT.

## `(single seq)`

Is SEQ a sequence of one element?

## `(partition pred seq &key start end key)`

Partition elements of SEQ into those for which PRED returns true
and false.

Return two values, one with each sequence.

Exactly equivalent to:
     (values (remove-if predicate seq) (remove-if-not predicate seq))
except it visits each element only once.

Note that `partition` is not just `assort` with an up-or-down
predicate. `assort` returns its groupings in the order they occur in
the sequence; `partition` always returns the “true” elements first.

    (assort '(1 2 3) :key #'evenp) => ((1 3) (2))
    (partition #'evenp '(1 2 3)) => (2), (1 3)

## `(partitions preds seq &key start end key)`

Generalized version of PARTITION.

PREDS is a list of predicates. For each predicate, `partitions`
returns a filtered copy of SEQ. As a second value, it returns an extra
sequence of the items that do not match any predicate.

Items are assigned to the first predicate they match.

## `(assort seq &key key test start end)`

Return SEQ assorted by KEY.

     (assort (iota 10)
             :key (lambda (n) (mod n 3)))
     => '((0 3 6 9) (1 4 7) (2 5 8))

You can think of `assort` as being akin to `remove-duplicates`:

     (mapcar #'first (assort list))
     ≡ (remove-duplicates list :from-end t)

## `(runs seq &key start end key test)`

Return a list of runs of similar elements in SEQ.
The arguments START, END, and KEY are as for `reduce`.

    (runs '(head tail head head tail))
    => '((head) (tail) (head head) (tail))

## `(batches seq &optional n)`

Return SEQ in batches of N elements.

    (batches (iota 11) 2)
    => ((0 1) (2 3) (4 5) (6 7) (8 9) (10))

## `(safe-sort seq pred &rest args)`

Like `sort`, but not destructive.

## `(sortf g18296 pred &rest args)`

Sort a place with `sort`.

## `(frequencies seq &rest hash-table-args)`

Return a hash table with the count of each unique item in SEQ.

From Clojure.

## `(nub seq &rest args &key start end key test)`

Remove duplicates from SEQ, starting from the end.
TEST defaults to `equal`.

From Haskell.

## `(gcp seqs &key test)`

The greatest common prefix of SEQS.

## `(gcs seqs &key test)`

The greatest common suffix of SEQS.

## `(length< seq n)`

Is SEQ less than N elements long?

## `(length> seq n)`

Is SEQ more than N elements long?

## `(length>= seq n)`

Is SEQ at least N elements long?

## `(length<= seq n)`

Is SEQ no more than N elements long?

## `(longer x y)`

Return the longer of X and Y.

If X and Y are of equal length, return X.

## `(longest seqs)`

Return the longest seq in SEQS.

## `(cut seq indices)`

Divide up SEQ at INDICES.

     (cut (iota 8) '(2 4 6))
     => ((0 1) (2 3) (4 5) (6 7))

From Q.

## `(slice seq start &optional end)`

Like `subseq`, but allows negative bounds to specify offsets.
Both START and END accept negative bounds.

     (slice "string" -3 -1) => "in"

Setf of `slice` is like setf of `ldb`: afterwards, the place being set
holds a new sequence which is not EQ to the old.

## `(ordering seq &key unordered-to-end from-end test)`

Given a sequence, return a function that, when called with `sort`,
restores the original order of the sequence.

That is, for any SEQ (without duplicates), it is always true that

     (equal seq (sort (shuffle (copy-seq seq)) (ordering seq)))

FROM-END controls what to do in case of duplicates. If FROM-END is
true, the last occurrence of each item is preserved; otherwise, only
the first occurrence counts.

TEST controls identity; it should be a valid test for a hash table.

UNORDERED-TO-END controls where to sort items that are not present in
the original ordering. By default they are sorted first but, if
UNORDERED-TO-END is true, they are sorted last. In either case, they
are left in no particular order.

## `(bestn n seq pred &key key)`

Partial sorting.
Equivalent to (firstn N (sort SEQ PRED)), but much faster, at least
for small values of N.

The name is from Arc.

## `(extrema seq pred &key key start end)`

Like EXTREMUM, but returns both the minimum and the maximum (as two
values).

     (extremum (iota 10) #'>) => 9
     (extrema (iota 10) #'>) => 9, 0

## `(vector= v1 v2 &key test start1 end1 start2 end2)`

Like `string=` for any vector.

## `(take n seq)`

Return the first N elements of SEQ, as a *new* sequence of the same
type as SEQ.

## `(drop n seq)`

Return all but the first N elements of SEQ.
The sequence returned is a new sequence of the same type as SEQ.

## `(halves seq &optional split)`

Return, as two values, the first and second halves of SEQ.
SPLIT designates where to split SEQ; it defaults to half the length,
but can be specified.

If SEQ is of an odd length, the split is made using `ceiling` rather
than `truncate`. This is on the theory that, if SEQ is a
single-element list, it should be returned unchanged.

# Numbers

## `(finc place &optional (delta 1))`

Like `incf`, but returns the old value instead of the new.

An alternative to using -1 as the starting value of a counter, which
can prevent optimization.

## `(fdec place &optional (delta 1))`

Like `decf`, but returns the old value instead of the new.

## `(parse-float string &key start end junk-allowed)`

Based on the venerable `parse-float` from the CMU Lisp repository.
Of course you could just use `parse-number`, but sometimes only a
float will do.

## `(round-to number &optional divisor)`

Like `round`, but return the resulting number.

     (round 15 10) => 2
     (round-to 15 10) => 20

## `(bits int)`

Return a bit vector of the bits in INT.

## `(unbits bits)`

Turn a sequence of BITS into an integer.

## `(shrink n by)`

Decrease N by a factor.

## `(grow n by)`

Increase N by a factor.

## `(shrinkf g18925 n)`

Shrink the value in a place by a factor.

## `(growf g18947 n)`

Grow the value in a place by a factor.

## `(ln n)`

Natural logarithm.

## `(lb n)`

Binary logarithm.

## `(lg n)`

Decimal logarithm.

## `(random-in-range low high)`

Random number in the range [low,high).

From Zetalisp.

# Octets

## `(octet-vector-p x)`

Is X an octet vector?

## `(make-octet-vector size)`

Make an octet vector of SIZE elements.

## `(octets n)`

Return N, an integer, as a little-endian octet vector.

## `(unoctets bytes)`

Concatenate BYTES into an integer in little-endian order.

# Time

## `(universal-to-unix time)`

Convert a universal time to a Unix time.

## `(unix-to-universal time)`

Convert a Unix time to a universal time.

## `(get-unix-time)`

The current time as a count of seconds from the Unix epoch.

## `(date-leap-year-p year)`

Is YEAR a leap year in the Gregorian calendar?

## `(time-since time)`

Return seconds since TIME.

## `(time-until time)`

Return seconds until TIME.

## `(interval &key seconds minutes hours days weeks months years month-days year-days)`

A verbose but readable way of specifying intervals in seconds.

Intended as a more readable alternative to idioms
like (let ((day-in-seconds #.(* 24 60 60))) ...)

Has a compiler macro.

## `(with-timing (&key quiet gc repeat) &body body)`

A convenience wrapper around TIME.

QUIET suppresses both the return value and any output to
`*standard-output*`.

REPEAT specifies a number of repetitions.

If GC is non-nil, perform a garbage collection before running BODY.
This can be useful with repeated trials, when you want to make sure
the running time of the *nth* run is not distorted by cleaning up
after the runs before it.

# Clos

## `(make class &rest initargs)`

Shorthand for `make-instance`.

After Eulisp.

## `(class-name-safe x)`

The class name of X.

If X is a class, the name of the class itself.

## `(find-class-safe x)`

The class designated by X.

If X is a class, it designates itself.

# Hooks

## `(add-hook name fn)`

Add FN to the value of NAME, a hook.

## `(remove-hook name fn)`

Remove fn from the symbol value of NAME.

## `(run-hooks &rest hookvars)`

Run all the hooks in all the HOOKVARS.
The variable `*hook*` is bound to each hook as it is being run.

## `(run-hook-with-args hook &rest args)`

Apply each function in the symbol value of HOOK to ARGS.

## `(run-hook-with-args-until-failure hook &rest args)`

Like `run-hook-with-args`, but quit once a function returns nil.

## `(run-hook-with-args-until-success hook &rest args)`

Like `run-hook-with-args`, but quit once a function returns
non-nil.

