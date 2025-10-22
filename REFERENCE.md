# Function Listing For serapeum (44 files, 470 functions)

- [Portability](#portability)
- [Macro Tools](#macro-tools)
- [Types](#types)
- [Control Flow](#control-flow)
- [Octets](#octets)
- [Iter](#iter)
- [Binding](#binding)
- [Control Flow](#control-flow)
- [Definitions](#definitions)
- [Defining Types](#defining-types)
- [Conditions](#conditions)
- [Op](#op)
- [Box](#box)
- [Threads](#threads)
- [Functions](#functions)
- [Trees](#trees)
- [Hash Tables](#hash-tables)
- [Files](#files)
- [Symbols](#symbols)
- [Arrays](#arrays)
- [Queue](#queue)
- [Numbers](#numbers)
- [Octets](#octets)
- [Time](#time)
- [Clos](#clos)
- [Hooks](#hooks)
- [Fbind](#fbind)
- [Static Let](#static-let)
- [Reader](#reader)
- [Packages](#packages)
- [Heap](#heap)
- [Lists](#lists)
- [Sequences](#sequences)
- [Strings](#strings)
- [Vectors](#vectors)
- [Vector=](#vector=)
- [Internal Definitions](#internal-definitions)
- [Tree Case](#tree-case)
- [Dispatch Case](#dispatch-case)
- [Range](#range)
- [Generalized Arrays](#generalized-arrays)
- [Units](#units)
- [Exporting](#exporting)
- [Docs](#docs)

## Portability

### `(undisplace-array array)`

Recursively get the fundamental array that ARRAY is displaced to.

Return the fundamental array, and the start and end positions into it.

Borrowed from Erik Naggum.

[View source](portability.lisp#L72)

### `(static-load-time-value form &optional (read-only-p nil read-only-p-supplied?))`

Like `load-time-value`, but signals an error if it cannot preserve identity.

On close reading of the standard, in a function that is evaluated but
not compiled, it is permissible for implementations to repeatedly
execute a `load-time-value` form, and in fact some implementations do
this (including, at the time of writing, ABCL, CLISP, Allegro and
LispWorks).

When `static-load-time-value` is compiled, it behaves exactly like
`load-time-value`. Otherwise it conducts a run-time check to ensure
that `load-time-value` preserves identity.

[View source](portability.lisp#L107)

## Macro Tools

### `(string-gensym x)`

Equivalent to (gensym (string x)).

Generally preferable to calling GENSYM with a string, because it
respects the current read table.

The alternative to writing `(mapcar (compose #'gensym #'string) ...)'
in every other macro.

[View source](macro-tools.lisp#L94)

### `(unique-name x)`

Alias for `string-gensym`.

[View source](macro-tools.lisp#L106)

### `(make-unique-name-list length &optional x)`

Alias for `alexandria:make-gensym-list`.

[View source](macro-tools.lisp#L110)

### `(unsplice form)`

If FORM is non-nil, wrap it in a list.

This is useful with ,@ in macros, and with `mapcan`.

E.g., instead of writing:

    `(.... ,@(when flag '((code))))

You can write:

    `(.... ,@(unsplice (when flag '(code))))

It may be especially helpful when splicing in variables. Instead of
writing:

    `(.... ,@(and docstring `(,docstring)))

You can simply write:

   `(.... ,@(unsplice docstring))

From Lparallel.

[View source](macro-tools.lisp#L118)

### `(with-thunk (spec &rest args) &body body)`

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
        `(call-with-foo ,body)))

You can give the thunk a name for easier debugging.

    (with-thunk ((body :name foo)) ...)

It is also possible to construct a "thunk" with arguments.

    (with-thunk (body foo)
      `(call-with-foo ,body))
    ≡ `(flet ((,thunk (,foo)
          ,@body))
        (declare (dynamic-extent #',thunk))
        (call-with-foo #',thunk))

Someday this may have a better name.

[View source](macro-tools.lisp#L148)

### `(expand-macro form &optional env)`

Like `macroexpand-1`, but also expand compiler macros.
From Swank.

[View source](macro-tools.lisp#L219)

### `(expand-macro-recursively form &optional env)`

Like `macroexpand`, but also expand compiler macros.
From Swank.

[View source](macro-tools.lisp#L228)

### `(partition-declarations xs declarations &optional env)`

Split DECLARATIONS into those that do and do not apply to XS.
Return two values, one with each set.

Both sets of declarations are returned in a form that can be spliced
directly into Lisp code:

     (locally ,@(partition-declarations vars decls) ...)

[View source](macro-tools.lisp#L241)

### `(callf function place &rest args)`

Set PLACE to the value of calling FUNCTION on PLACE, with ARGS.

[View source](macro-tools.lisp#L261)

### `(callf2 function arg1 place &rest args)`

Like CALLF, but with the place as the second argument.

[View source](macro-tools.lisp#L270)

### `(define-do-macro name binds &body body)`

Define an iteration macro like `dolist`.

Writing a macro like `dolist` is more complicated than it looks. For
consistency with the rest of CL, you have to do all of the following:

- The entire loop must be surrounded with an implicit `nil` block.
- The body of the loop must be an implicit `tagbody`.
- There must be an optional `return` form which, if given, supplies
  the values to return from the loop.
- While this return form is being evaluated, the iteration variables
  must be bound to `nil`.

Say you wanted to define a `do-hash` macro that iterates over hash
tables. A full implementation would look like this:

     (defmacro do-hash ((key value hash-table &optional return) &body body)
       (multiple-value-bind (body decls) (parse-body body)
         `(block nil
            (maphash (lambda (,key ,value)
                       ,@decls
                       (tagbody
                          ,@body))
                     ,hash-table)
            ,(when return
               `(let (,key ,value)
                  ,return)))))

Using `define-do-macro` takes care of all of this for you.

     (define-do-macro do-hash ((key value hash-table &optional return) &body body)
       `(maphash (lambda (,key ,value)
                   ,@body)
                 ,hash-table))

[View source](macro-tools.lisp#L279)

### `(define-post-modify-macro name lambda-list function &optional documentation)`

Like `define-modify-macro`, but arranges to return the original value.

[View source](macro-tools.lisp#L343)

### `(parse-leading-keywords body)`

Given BODY, return two values: a list of the leading inline keyword
arguments, and the rest of the body.

Inline keywords are like the keyword arguments to individual cases in
`restart-case`.

[View source](macro-tools.lisp#L371)

### `(with-read-only-vars (&rest vars) &body body)`

Make VARS read-only within BODY.

That is, within BODY, each var in VARS is bound as a symbol macro,
which expands into a macro whose setf expander, in turn, is defined to
signal a warning at compile time, and an error at run time.

Depending on your Lisp implementation this may or may not do anything,
and may or may not have an effect when used on special variables.

[View source](macro-tools.lisp#L433)

### `(define-case-macro name macro-args params &body macro-body)`

Define a macro like `case`.

A case-like macro is one that supports the following syntax:

- A list of keys is treated as matching any key in the list.
- An empty list matches nothing.
- The atoms T or `otherwise` introduce a default clause.
- There can only be one default clause.
- The default clause must come last.
- Any atom besides the empty list, T, or `otherwise` matches itself.

As a consequence of the above, to match against the empty list, T, or
`otherwise`, they must be wrapped in a list.

    (case x
      ((nil) "Matched nil.")
      ((t) "Matched t.")
      ((otherwise) "Matched `otherwise`.")
      (otherwise "Didn't match anything."))

A macro defined using `define-case-macro` can ignore all of the above.
It receives three arguments: the expression, already protected against
multiple evaluation; a normalized list of clauses; and, optionally, a
default clause.

The clauses are normalized as a list of `(key . body)', where each key
is an atom. (That includes nil, T, and `otherwise`.) Nonetheless, each
body passed to the macro will only appear once in the expansion; there
will be no duplicated code.

The body of the default clause is passed separately,
bound to the value of the `:default` keyword in PARAMS.

    (define-case-macro my-case (expr &body clauses)
        (:default default)
      ....)

Note that in this case, `default` will be bound to the clause's body
-- a list of forms -- and not to the whole clause. The key of the
default clause is discarded.

If no binding is specified for the default clause, then no default
clause is allowed.

One thing you do still have to consider is the handling of duplicated
keys. The macro defined by `define-case-macro` will reject case sets
that contains duplicate keys under `eql`, but depending on the
semantics of your macro, you may need to check for duplicates under a
looser definition of equality.

As a final example, if the `case` macro did not already exist, you
could define it almost trivially using `define-case-macro`:

    (define-case-macro my-case (expr &body clause)
        (:default default)
      `(cond
         ,@(loop for (key . body) in clauses
                 collect `((eql ,expr ,key) ,@body))
         (t ,@body)))

[View source](macro-tools.lisp#L479)

### `(case-failure expr keys)`

Signal an error of type `case-failure`.

[View source](macro-tools.lisp#L718)

### `(eval-if-constant form &optional env)`

Try to reduce FORM to a constant, using ENV.
If FORM cannot be reduced, return it unaltered.

Also return a second value, T if the form could be reduced to a
constant, or nil otherwise. (Note that the second value may be T if
FORM was already a constant; think of it as a "green light" to treat
the value as a constant.)

This is equivalent to testing if FORM is constant, then evaluating it,
except that FORM is macro-expanded in ENV (taking compiler macros into
account) before doing the test.

Note that this function may treat a form as constant which would not
be recognized as such by `constantp`, because we also expand compiler
macros.

[View source](macro-tools.lisp#L739)

### `(expect-form-list exp)`

Sanity-check EXP, a macro expansion, assuming it is supposed to be
  a series of forms suitable for splicing into a progn (implicit or
  explicit.)

[View source](macro-tools.lisp#L822)

### `(expect-single-form exp)`

Sanity-check EXP, a macro expansion, assuming it is supposed to be
  a single form suitable for inserting intact.

[View source](macro-tools.lisp#L832)

### `(unparse-ordinary-lambda-list &optional required optional rest keywords aok? aux key?)`

Put together an ordinary lambda list from its constituent parts.

This is the inverse of `alexandria:parse-ordinary-lambda-list`.

    lambda-list
    ≡ (multiple-value-call #'unparse-ordinary-lambda-list
        (parse-ordinary-lambda-list lambda-list)

[View source](macro-tools.lisp#L844)

### `(parse-defmethod-args args)`

Parse the args to defmethod (everything except the name).
Returns three values: a list of qualifiers, the specialized
lambda-list, and the forms that make up the body.

[View source](macro-tools.lisp#L874)

## Types

### `(-> functions (&rest args) &optional values)`

Declaim the ftype of one or multiple FUNCTIONS from ARGS to VALUES.

     (-> mod-fixnum+ (fixnum fixnum) fixnum)
     (defun mod-fixnum+ (x y) ...)

     (-> (mod-float+ mod-single-float+) (float float) float)
     (defun mod-float+ (x y) ...)
     (defun mode-single-float+ (x y) ...)

[View source](types.lisp#L108)

### `(assure type-spec &body (form))`

Macro for inline type checking.

`assure` is to `the` as `check-type` is to `declare`.

     (the string 1)    => undefined
     (assure string 1) => error

The value returned from the `assure` form is guaranteed to satisfy
TYPE-SPEC. If FORM does not return a value of that type, then a
correctable error is signaled. You can supply a value of the correct
type with the `use-value` restart.

Note that the supplied value is *not* saved into the place designated
by FORM. (But see `assuref`.)

Using `values` types is supported, with caveats:
- The types of `&rest` arguments are enforced using `soft-list-of`.
- Types defined with `deftype` that expand into values types may not be checked in some Lisps.

From ISLISP.

[View source](types.lisp#L222)

### `(assuref place type-spec)`

Like `(progn (check-type PLACE TYPE-SPEC) PLACE)`, but evaluates
PLACE only once.

[View source](types.lisp#L309)

### `(supertypep supertype type &optional env)`

Is SUPERTYPE a supertype of TYPE?
That is, is TYPE a subtype of SUPERTYPE?

[View source](types.lisp#L341)

### `(proper-subtype-p subtype type &optional env)`

Is SUBTYPE a proper subtype of TYPE?

This is, is it true that SUBTYPE is a subtype of TYPE, but not the
same type?

[View source](types.lisp#L347)

### `(proper-supertype-p supertype type &optional env)`

Is SUPERTYPE a proper supertype of TYPE?

That is, is it true that every value of TYPE is also of type
SUPERTYPE, but not every value of SUPERTYPE is of type TYPE?

[View source](types.lisp#L372)

### `(vref vec index)`

When used globally, same as `aref`.

Inside of a with-type-dispatch form, calls to `vref` may be bound to
different accessors, such as `char` or `schar`, or `bit` or `sbit`,
depending on the type being specialized on.

[View source](types.lisp#L430)

### `(with-type-dispatch (&rest types) var &body body)`

A macro for writing fast sequence functions (among other things).

In the simplest case, this macro produces one copy of BODY for each
type in TYPES, with the appropriate declarations to induce your Lisp
to optimize that version of BODY for the appropriate type.

Say VAR is a string. With this macro, you can trivially emit optimized
code for the different kinds of string that VAR might be. And
then (ideally) instead of getting code that dispatches on the type of
VAR every time you call `aref`, you get code that dispatches on the
type of VAR once, and then uses the appropriately specialized
accessors. (But see `with-string-dispatch`.)

But that's the simplest case. Using `with-type-dispatch` also provides
*transparent portability*. It examines TYPES to deduplicate types that
are not distinct on the current Lisp, or that are shadowed by other
provided types. And the expansion strategy may differ from Lisp to
Lisp: ideally, you should not have to pay for good performance on
Lisps with type inference with pointless code bloat on other Lisps.

There is an additional benefit for vector types. Around each version
of BODY, the definition of `vref` is shadowed to expand into an
appropriate accessor. E.g., within a version of BODY where VAR is
known to be a `simple-string`, `vref` expands into `schar`.

Using `vref` instead of `aref` is obviously useful on Lisps that do
not do type inference, but even on Lisps with type inference it can
speed compilation times (compiling `aref` is relatively slow on SBCL).

Within `with-type-dispatch`, VAR should be regarded as read-only.

Note that `with-type-dispatch` is intended to be used around
relatively expensive code, particularly loops. For simpler code, the
gains from specialized compilation may not justify the overhead of the
initial dispatch and the increased code size.

Note also that `with-type-dispatch` is relatively low level. You may
want to use one of the other macros in the same family, such as
`with-subtype-dispatch`, `with-string-dispatch`, or so forth.

The design and implementation of `with-type-dispatch` is based on a
few sources. It replaces a similar macro formerly included in
Serapeum, `with-templated-body`. One possible expansion is based on
the `string-dispatch` macro used internally in SBCL. But most of the
credit should go to the paper "Fast, Maintable, and Portable Sequence
Functions", by Irène Durand and Robert Strandh.

[View source](types.lisp#L493)

### `(with-subtype-dispatch type (&rest subtypes) var &body body)`

Like `with-type-dispatch`, but SUBTYPES must be subtypes of TYPE.

Furthermore, if SUBTYPES are not exhaustive, an extra clause will be
added to ensure that TYPE itself is handled.

[View source](types.lisp#L585)

### `(with-string-dispatch (&rest types) var &body body)`

Like `with-subtype-dispatch` with an overall type of `string`.

[View source](types.lisp#L598)

### `(with-vector-dispatch (&rest types) var &body body)`

Like `with-subtype-dispatch` with an overall type of `vector`.

[View source](types.lisp#L608)

### `(with-simple-vector-dispatch (&rest types) (var start end) &body body)`

Like `with-vector-dispatch` but on implementations that support it, the underlying simple vector of a displaced array is first dereferenced, so the type is guaranteed to be a subtype of simple-array (but not actually `simple-vector`).

START and END are the offset of the original vector's data in the array it is displaced to.

[View source](types.lisp#L614)

### `(with-boolean (&rest branches) &body body)`

Establishes a lexical environment in which it is possible to use
macroexpand-time branching. Within the lexical scope of
`with-boolean`, it is possible to use `boolean-if`, `boolean-when`,
and `boolean-unless` to conditionalize whether some forms are included
at compilation time. (You may also use `:if`, `:when`, or `:unless`
for brevity.)

The first argument must be a list of symbols which name variables. This macro
will expand into a series of conditionals

[View source](types.lisp#L651)

### `(boolean-if branch then &optional else)`

Chooses between the forms to include based on whether a macroexpand-time
branch is true. The first argument must be a symbol naming a branch in the
lexically enclosing WITH-BOOLEAN form.

It is an error to use this macro outside the lexical environment established by
WITH-BOOLEAN.

[View source](types.lisp#L714)

### `(boolean-when branch &body body)`

Includes some forms based on whether a macroexpand-time branch is true. The
first argument must be a symbol naming a branch in the lexically enclosing
WITH-BOOLEAN form.

It is an error to use this macro outside the lexical environment established by
WITH-BOOLEAN.

[View source](types.lisp#L731)

### `(boolean-unless branch &body body)`

Includes some forms based on whether a macroexpand-time branch is false. The
first argument must be a symbol naming a branch in the lexically enclosing
WITH-BOOLEAN form.

It is an error to use this macro outside the lexical environment established by
WITH-BOOLEAN.

[View source](types.lisp#L752)

### `(with-two-arg-test (test) &body body)`

Specialize BODY on the most common two-arg test functions.

[View source](types.lisp#L777)

### `(with-member-test (test-fn &key key test test-not) &body body)`

Emit BODY multiple times with specialized, inline versions of
`member` bound to TEST-FN.

[View source](types.lisp#L800)

### `(with-item-key-function (key &optional (key-form key)) &body body)`

For each of the most common key functions used in sequences, emit a
copy of BODY with KEY bound to a local macro that calls KEY-FORM.

If current optimization declarations favor space over speed, or
compilation speed over runtime speed, then BODY is only emitted once.

[View source](types.lisp#L850)

### `(true x)`

Coerce X to a boolean.
That is, if X is null, return `nil`; otherwise return `t`.

Based on an idea by Eric Naggum.

[View source](types.lisp#L876)

## Control Flow

### `(nor &rest forms)`

Equivalent to (not (or ...)).

From Arc.

[View source](control-flow.lisp#L105)

## Octets

### `(octet-vector &rest args)`

Constructor an octet vector from ARGS.

[View source](octets.lisp#L14)

## Iter

### `(nlet name (&rest bindings) &body body)`

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

The name comes from `Let Over Lambda', but this is a more careful
implementation: the function is not bound while the initial arguments
are being evaluated, and it is safe to close over the arguments.

[View source](iter.lisp#L40)

### `(with-collector (collector) &body body)`

Within BODY, bind COLLECTOR to a function of one argument that
accumulates all the arguments it has been called with in order, like
the collect clause in `loop`, finally returning the collection.

To see the collection so far, call COLLECTOR with no arguments.

Note that this version binds COLLECTOR to a closure, not a macro: you
can pass the collector around or return it like any other function.

[View source](iter.lisp#L119)

### `(collecting &body body)`

Like `with-collector`, with the collector bound to the result of
interning `collect` in the current package.

[View source](iter.lisp#L142)

### `(with-collectors (&rest collectors) &body body)`

Like `with-collector`, with multiple collectors.
Returns the final value of each collector as multiple values.

     (with-collectors (x y z)
       (x 1)
       (y 2)
       (z 3))
     => '(1) '(2) '(3)

[View source](iter.lisp#L149)

### `(summing &body body)`

Within BODY, bind `sum` to a function that gathers numbers to sum.

If the first form in BODY is a literal number, it is used instead of 0
as the initial sum.

To see the running sum, call `sum` with no arguments.

Return the total.

[View source](iter.lisp#L174)

## Binding

### `(lret (&rest bindings) &body body)`

Return the initial value of the last binding in BINDINGS. The idea
is to create something, initialize it, and then return it.

    (lret ((x 1)
           (y (make-array 1)))
      (setf (aref y 0) x))
    => #(1)

Note that the value returned is the value initially bound. Subsequent
assignments are ignored.

    (lret ((x 1))
      (setf x 2))
    => 1

Furthermore, on Lisps that support it, the variable may be made
read-only, making assignment a compiler-time error.

`lret` may seem trivial, but it fufills the highest purpose a macro
can: it eliminates a whole class of bugs (initializing an object, but
forgetting to return it).

Cf. `aprog1` in Anaphora.

[View source](binding.lisp#L65)

### `(lret* (&rest bindings) &body body)`

Cf. `lret`.

[View source](binding.lisp#L92)

### `(letrec (&rest bindings) &body body)`

Recursive LET.
The idea is that functions created in BINDINGS can close over one
another, and themselves.

Note that `letrec` only binds variables: it can define recursive
functions, but can't bind them as functions. (But see `fbindrec`.)

[View source](binding.lisp#L117)

### `(letrec* (&rest bindings) &body body)`

Like LETREC, but the bindings are evaluated in order.
See Waddell et al., *Fixing Letrec* for motivation.

Cf. `fbindrec*`.

[View source](binding.lisp#L127)

### `(receive formals expr &body body)`

Stricter version of `multiple-value-bind`.

Use `receive` when you want to enforce that EXPR should return a
certain number of values, or a minimum number of values.

If FORMALS is a proper list, then EXPR must return exactly as many
values -- no more and no less -- as there are variables in FORMALS.

If FORMALS is an improper list (VARS . REST), then EXPR must return at
least as many values as there are VARS, and any further values are
bound, as a list, to REST.

Lastly, if FORMALS is a symbol, bind that symbol to all the values
returned by EXPR, as if by `multiple-value-list`.

From Scheme (SRFI-8).

[View source](binding.lisp#L135)

### `(mvlet* (&rest bindings) &body body)`

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

[View source](binding.lisp#L192)

### `(mvlet (&rest bindings) &body body)`

Parallel (`let`-like) version of `mvlet*`.

[View source](binding.lisp#L251)

### `(and-let* (&rest clauses) &body body)`

Scheme's guarded LET* (SRFI-2).

Each clause should have one of the following forms:

- `identifier`, in which case IDENTIFIER's value is tested.

- `(expression)`, in which case the value of EXPRESSION is tested.

- `(identifier expression)' in which case EXPRESSION is evaluated,
    and, if its value is not false, IDENTIFIER is bound to that value
    for the remainder of the clauses and the optional body.

Note that, of course, the semantics are slightly different in Common
Lisp than in Scheme, because our AND short-circuits on null, not
false.

Also, this version makes the bindings immutable.

[View source](binding.lisp#L287)

### `(if-not test then &optional else)`

If TEST evaluates to NIL, evaluate THEN and return its values,
otherwise evaluate ELSE and return its values. ELSE defaults to NIL.

[View source](binding.lisp#L349)

### `(if-not-let bindings &body (then-form &optional else-form))`

Creates new variable bindings, and conditionally executes either
THEN-FORM or ELSE-FORM. ELSE-FORM defaults to NIL.
BINDINGS must be either single binding of the form:
 (variable initial-form)
or a list of bindings of the form:
 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))
All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.
If one of the variables was bound to NIL, the THEN-FORM is executed with the
bindings in effect, otherwise the ELSE-FORM is executed with the bindings in
effect.
Adapted from Alexandria if-let.

[View source](binding.lisp#L356)

## Control Flow

### `(eval-always &body body)`

Shorthand for
        (eval-when (:compile-toplevel :load-toplevel :execute) ...)

[View source](control-flow.lisp#L73)

### `(eval-and-compile &body body)`

Emacs's `eval-and-compile`.
Alias for `eval-always`.

[View source](control-flow.lisp#L79)

### `(no x)`

Another alias for `not` and `null`.

From Arc.

[View source](control-flow.lisp#L85)

### `(null-if arg1 arg2 &key test)`

Return nil if arguments are equal under TEST, ARG1 otherwise.
Return a second value of nil if the arguments were equal, T
otherwise.

From SQL.

[View source](control-flow.lisp#L95)

### `(nand &rest forms)`

Equivalent to (not (and ...)).

[View source](control-flow.lisp#L116)

### `(typecase-of type x &body clauses)`

Like `etypecase-of`, but may, and must, have an `otherwise` clause
in case X is not of TYPE.

[View source](control-flow.lisp#L231)

### `(etypecase-of type x &body body)`

Like `etypecase` but, at compile time, warn unless each clause in
BODY is a subtype of TYPE, and the clauses in BODY form an exhaustive
partition of TYPE.

[View source](control-flow.lisp#L244)

### `(case-of type x &body clauses)`

Like `case` but may, and must, have an `otherwise` clause.

[View source](control-flow.lisp#L256)

### `(ecase-of type x &body body)`

Like `ecase` but, given a TYPE (which should be defined as `(member
...)`), warn, at compile time, unless the keys in BODY are all of TYPE
and, taken together, they form an exhaustive partition of TYPE.

[View source](control-flow.lisp#L268)

### `(ctypecase-of type keyplace &body body)`

Like `etypecase-of`, but providing a `store-value` restart to correct KEYPLACE and try again.

[View source](control-flow.lisp#L280)

### `(ccase-of type keyplace &body body)`

Like `ecase-of`, but providing a `store-value` restart to correct KEYPLACE and try again.

[View source](control-flow.lisp#L285)

### `(destructuring-ecase-of type expr &body body)`

Like `destructuring-ecase`, from Alexandria, but with exhaustivness
checking.

TYPE is a designator for a type, which should be defined as `(member
...)`. At compile time, the macro checks that, taken together, the
symbol at the head of each of the destructuring lists in BODY form an
exhaustive partition of TYPE, and warns if it is not so.

[View source](control-flow.lisp#L303)

### `(destructuring-case-of type expr &body body)`

Like `destructuring-ecase-of`, but an `otherwise` clause must also be supplied.

Note that the otherwise clauses must also be a list:

    ((otherwise &rest args) ...)

[View source](control-flow.lisp#L313)

### `(destructuring-ccase-of type keyplace &body body)`

Like `destructuring-case-of`, but providing a `store-value` restart
to collect KEYPLACE and try again.

[View source](control-flow.lisp#L321)

### `(case-using pred keyform &body clauses)`

ISLISP's case-using.

     (case-using #'eql x ...)
     ≡ (case x ...).

Note that, no matter the predicate, the keys are not evaluated. (But
see `selector`.)

The PRED form is evaluated.

When PRED is invoked, KEYFORM is its first argument. You can use
`flip` if you want the arguments passed the other way around. For
example, to dispatch on potential elements of a list:

    (case-using list (flip #'member)
      (:item1 ...))

This version supports both single-item clauses (x ...) and
multiple-item clauses ((x y) ...), as well as (t ...) or (otherwise
...) for the default clause.

[View source](control-flow.lisp#L326)

### `(ecase-using pred keyform &body clauses)`

Exhaustive variant of `case-using`.

[View source](control-flow.lisp#L368)

### `(string-case stringform &body clauses)`

Efficient `case`-like macro with string keys.

Note that string matching is always case-sensitive.

This uses Paul Khuong's `string-case` macro internally.

[View source](control-flow.lisp#L379)

### `(string-ecase stringform &body clauses)`

Efficient `ecase`-like macro with string keys.

Note that string matching is always case-sensitive.

Cf. `string-case`.

[View source](control-flow.lisp#L410)

### `(eif test then &optional (else nil else?))`

Like `cl:if`, but expects two branches.

If there is only one branch a warning is signaled.

This macro is useful when writing explicit decision trees; it will
warn you if you forget a branch.

Short for “exhaustive if”.

[View source](control-flow.lisp#L426)

### `(eif-let binds &body (then &optional (else nil else?)))`

Like `alexandria:if-let`, but expects two branches.
Compare `eif`.

[View source](control-flow.lisp#L440)

### `(econd &rest clauses)`

Like `cond`, but signal an error of type `econd-failure` if no
clause succeeds.

[View source](control-flow.lisp#L457)

### `(cond-let var &body clauses)`

Cross between COND and LET.

     (cond-let x ((test ...)))
     ≡ (let (x)
         (cond ((setf x test) ...)))

Cf. `acond` in Anaphora.

[View source](control-flow.lisp#L466)

### `(econd-let symbol &body clauses)`

Like `cond-let` for `econd`.

[View source](control-flow.lisp#L490)

### `(cond-every &body clauses)`

Like `cond`, but instead of stopping after the first clause that
succeeds, run all the clauses that succeed.

Return the value of the last successful clause.

If a clause begins with `cl:otherwise`, it runs only if no preceding
form has succeeded.

Note that this does *not* do the same thing as a series of `when`
forms: `cond-every` evaluates *all* the tests *before* it evaluates
any of the forms.

From Zetalisp.

[View source](control-flow.lisp#L503)

### `(bcond &body clauses)`

Scheme's extended COND.

This is exactly like COND, except for clauses having the form

     (test :=> recipient)

In that case, if TEST evaluates to a non-nil result, then RECIPIENT, a
function, is called with that result, and the result of RECIPIENT is
return as the value of the `cond`.

As an extension, a clause like this:

     (test :=> var ...)

Can be used as a shorthand for

     (test :=> (lambda (var) ...))

The name `bcond` for a “binding cond” goes back at least to the days
of the Lisp Machines. I do not know who was first to use it, but the
oldest examples I have found are by Michael Parker and Scott L.
Burson.

[View source](control-flow.lisp#L538)

### `(case-let (var expr) &body cases)`

Like (let ((VAR EXPR)) (case VAR ...)), with VAR read-only.

[View source](control-flow.lisp#L591)

### `(ccase-let (var expr) &body cases)`

Like (let ((VAR EXPR)) (ccase VAR ...)), with VAR correctable.

[View source](control-flow.lisp#L597)

### `(ecase-let (var expr) &body cases)`

Like (let ((VAR EXPR)) (ecase VAR ...)), with VAR read-only.

[View source](control-flow.lisp#L603)

### `(typecase-let (var expr) &body cases)`

Like (let ((VAR EXPR)) (typecase VAR ...)), with VAR read-only.

[View source](control-flow.lisp#L609)

### `(ctypecase-let (var expr) &body cases)`

Like (let ((VAR EXPR)) (ctypecase VAR ...)), with VAR correctable.

[View source](control-flow.lisp#L615)

### `(etypecase-let (var expr) &body cases)`

Like (let ((VAR EXPR)) (etypecase VAR ...)), with VAR read-only.

[View source](control-flow.lisp#L621)

### `(comment &body body)`

A macro that ignores its body and does nothing. Useful for
comments-by-example.

Also, as noted in EXTENSIONS.LISP of 1992, "This may seem like a
silly macro, but used inside of other macros or code generation
facilities it is very useful - you can see comments in the (one-time)
macro expansion!"

[View source](control-flow.lisp#L627)

### `(example &body body)`

Like `comment`.

[View source](control-flow.lisp#L637)

### `(nix &rest places)`

Set PLACES to nil and return the old value(s) of PLACES.

If there is more than one PLACE, return their old values as multiple values.

This may be more efficient than (shiftf place nil), because it only
sets PLACE when it is not already null.

[View source](control-flow.lisp#L651)

### `(ensure place &body newval)`

Essentially (or place (setf place newval)).

PLACE is treated as unbound if it returns `nil`, signals
`unbound-slot`, or signals `unbound-variable`.

Note that ENSURE is `setf`-able, so you can do things like
     (incf (ensure x 0))

Cf. `ensure2`.

[View source](control-flow.lisp#L663)

### `(ensure2 place &body newval)`

Like `ensure`, but specifically for accessors that return a second
value like `gethash`.

[View source](control-flow.lisp#L695)

### `(~> needle &rest holes)`

Threading macro from Clojure (by way of Racket).

Thread NEEDLE through HOLES, where each hole is either a
symbol (equivalent to `(hole needle)`) or a list (equivalent to `(hole
needle args...)`).

As an extension, an underscore in the argument list is replaced with
the needle, so you can pass the needle as an argument other than the
first.

[View source](control-flow.lisp#L767)

### `(~>> needle &rest holes)`

Like `~>` but, by default, thread NEEDLE as the last argument
instead of the first.

[View source](control-flow.lisp#L785)

### `(nest &rest things)`

Like ~>>, but backward.

This is useful when layering `with-x` macros where the order is not
important, and extra indentation would be misleading.

For example:

    (nest
     (with-open-file (in file1 :direction input))
     (with-open-file (in file2 :direction output))
     ...)

Is equivalent to:

    (with-open-file (in file1 :direction input)
      (with-open-file (in file2 :direction output)
        ...))

(But see `with-open-files`).

If the outer macro has no arguments, you may omit the parentheses.

    (nest
      with-standard-io-syntax
      ...)
    ≡ (with-standard-io-syntax
        ...)

From UIOP, based on a suggestion by Marco Baringer.

[View source](control-flow.lisp#L804)

### `(select keyform &body clauses)`

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

From Zetalisp.

[View source](control-flow.lisp#L839)

### `(selector keyform fn &body clauses)`

Like `select`, but compare using FN.

Note that (unlike `case-using`), FN is not evaluated.

Prefer `selector` to `case-using` when FN is a macro, or has a
compiler macro.

From Zetalisp.

[View source](control-flow.lisp#L858)

### `(sort-values pred &rest values)`

Sort VALUES with PRED and return as multiple values.

Equivalent to

    (values-list (sort (list VALUES...) pred))

But with less consing, and potentially faster.

[View source](control-flow.lisp#L980)

### `(eq* &rest xs)`

Variadic version of `eq`.

With no arguments, return T.

With one argument, return T.

With two arguments, same as `eq`.

With three or more arguments, return T only if all of XS are
equivalent under `eq`.

Has a compiler macro, so there is no loss of efficiency relative to
writing out the tests by hand.

[View source](control-flow.lisp#L1051)

### `(eql* &rest xs)`

Variadic version of `eql`.

With no arguments, return T.

With one argument, return T.

With two arguments, same as `eql`.

With three or more arguments, return T only if all of XS are
equivalent under `eql`.

Has a compiler macro, so there is no loss of efficiency relative to
writing out the tests by hand.

[View source](control-flow.lisp#L1053)

### `(equal* &rest xs)`

Variadic version of `equal`.

With no arguments, return T.

With one argument, return T.

With two arguments, same as `equal`.

With three or more arguments, return T only if all of XS are
equivalent under `equal`.

Has a compiler macro, so there is no loss of efficiency relative to
writing out the tests by hand.

[View source](control-flow.lisp#L1055)

### `(equalp* &rest xs)`

Variadic version of `equalp`.

With no arguments, return T.

With one argument, return T.

With two arguments, same as `equalp`.

With three or more arguments, return T only if all of XS are
equivalent under `equalp`.

Has a compiler macro, so there is no loss of efficiency relative to
writing out the tests by hand.

[View source](control-flow.lisp#L1057)

### `(without-recursion (&key) &body body)`

If BODY calls itself, at any depth, signal a (continuable) error of
type `recursion-forbidden`.

[View source](control-flow.lisp#L1067)

## Definitions

### `(def var &body (&optional val documentation))`

The famous "deflex".

Define a top level (global) lexical VAR with initial value VAL,
which is assigned unconditionally as with DEFPARAMETER. If a DOC
string is provided, it is attached to both the name |VAR| and the name
*STORAGE-FOR-DEFLEX-VAR-|VAR|* as a documentation string of kind
'VARIABLE. The new VAR will have lexical scope and thus may be
shadowed by LET bindings without affecting its dynamic (global) value.

The original `deflex` is due to Rob Warnock.

This version of `deflex` differs from the original in the following ways:

- It is possible for VAL to close over VAR.
- On implementations that support it (SBCL, CCL, and LispWorks, at the
moment) this version creates a backing variable that is "global" or
"static", so there is not just a change in semantics, but also a
gain in efficiency.
- If VAR is a list that starts with `values`, each element is treated as
a separate variable and initialized as if by `(setf (values VAR...)
VAL)`.

[View source](definitions.lisp#L42)

### `(define-values values &body (expr))`

Like `def`, but for multiple values.
Each variable in VALUES is given a global, lexical binding, as with
`def`, then set all at once, as with `multiple-value-setq`.

[View source](definitions.lisp#L99)

### `(defconst symbol init &optional docstring)`

Define a constant, lexically.

`defconst` defines a constant using a strategy similar to `def`, so
you don’t have to +cage+ your constants.

The constant is only redefined on re-evaluation if INIT has a
different literal representation than the old value.

A constant defined with `defconst` is guaranteed to be available as
soon as it has been defined (for example, for use with reader macros
later in the same file). This is not guaranteed to be portably true
for `defconstant`.

The name is from Emacs Lisp.

[View source](definitions.lisp#L123)

### `(defsubst name params &body body)`

Define an inline function.

     (defsubst fn ...)
     ≡ (declaim (inline fn))
       (defun fn ...)

The advantage of a separate defining form for inline functions is that
you can't forget to declaim the function inline before defining it –
without which it may not actually end up being inlined.

From Emacs and other ancient Lisps.

[View source](definitions.lisp#L152)

### `(defalias alias &body (def &optional docstring))`

Define a value as a top-level function.

     (defalias string-gensym (compose #'gensym #'string))

Like (setf (fdefinition ALIAS) DEF), but with a place to put
documentation and some niceties to placate the compiler.

Note that a function defined with `defalias` is declared `notinline`.
This is a matter of semantics: before we can assign to the function,
we must make it assignable (which is what `notinline` means).

Name from Emacs Lisp.

[View source](definitions.lisp#L174)

### `(defplace name args &body body)`

Define NAME and (SETF NAME) in one go.

BODY is a list of forms, starting with an optional docstring. The last
form in BODY, however, must be a single, setf-able expression.

[View source](definitions.lisp#L219)

### `(defvar-unbound var &body (docstring))`

Define VAR as if by `defvar` with no init form, and set DOCSTRING
as its documentation.

I believe the name comes from Edi Weitz.

[View source](definitions.lisp#L238)

### `(defparameter-unbound var &body (docstring))`

Like `defvar-unbound`, but ensures VAR is unbound when evaluated.

[View source](definitions.lisp#L248)

### `(defloop name args &body body)`

Define a function, ensuring proper tail recursion.
This is entirely equivalent to `defun` over `nlet`.

[View source](definitions.lisp#L256)

## Defining Types

### `(defcondition name supers &body (slots &rest options))`

Alias for `define-condition`.

Like (define-condition ...), but blissfully conforming to the same
nomenclatural convention as every other definition form in Common
Lisp.

[View source](defining-types.lisp#L27)

### `(defstruct-read-only name-and-opts &body slots)`

Easily define a defstruct with no mutable slots.

The syntax of `defstruct-read-only` is as close as possible to that of
`defstruct`. Given an existing structure definition, you can usually
make it immutable simply by switching out `defstruct` for
`defstruct-read-only`.

There are only a few syntactic differences:

1. To prevent accidentally inheriting mutable slots, and preserve its
   own usefulness as a marker of the programmer's intent,
   `defstruct-read-only` only allows inheritance from other classes
   defined using `defstruct-read-only`.

2. The `:type` option may not be used.

3. The `:copier` option is disabled, because it would be useless.

4. Slot definitions can use slot options without having to provide an
   initform. In this case, any attempt to make an instance of the
   struct without providing a value for that slot will signal an
   error.

    (my-slot :type string)
    ≡ (my-slot (required-argument 'my-slot) :read-only t :type string)

The idea here is simply that an unbound slot in an immutable data
structure does not make sense.

A read-only struct is always externalizable; it has an implicit
definition for `make-load-form`.

On Lisps that support it, the structure is also marked as "pure":
that is, instances may be moved into read-only memory.

`defstruct-read-only` is designed to stay as close to the syntax of
`defstruct` as possible. The idea is to make it easy to flag data as
immutable, whether in your own code or in code you are refactoring. In
new code, however, you may sometimes prefer `defconstructor`, which is
designed to facilitate working with immutable data.

[View source](defining-types.lisp#L107)

### `(read-eval-prefix object stream)`

A helper for making objects readable.

The obvious way to give an object a readable representation is to use
the sharp-dot reader macro. However, methods are supposed to consult
the values of `*print-readably*` and `*read-eval*` before doing so.
This function takes care of that for you.

If `*print-readably*` is false, return an empty string.

If `*print-readably*` is true, and `*read-eval*` is also true, return
the string "#.".

If `*print-readably*` is true, but `*read-eval*` is not true, signal
an error.

[View source](defining-types.lisp#L193)

### `(deconstruct x)`

If X is a type defined with `defconstructor`, return its slots as
multiple values.

[View source](defining-types.lisp#L237)

### `(defconstructor type-name &body slots)`

A variant of `defstruct` for modeling immutable data.

The structure defined by `defconstructor` has only one constructor,
which takes its arguments as required arguments (a BOA constructor).
Thus, `defconstructor` is only appropriate for data structures that
require no initialization.

The printed representation of an instance resembles its constructor:

    (person "Common Lisp" 33)
    => (PERSON "Common Lisp" 33)

While the constructor is BOA, the copier takes keyword arguments,
allowing you to override the values of a selection of the slots of the
structure being copied, while retaining the values of the others.

    (defconstructor person
      (name string)
      (age (integer 0 1000)))

    (defun birthday (person)
      (copy-person person :age (1+ (person-age person))))

    (birthday (person "Common Lisp" 33))
    => (PERSON "Common Lisp" 34)

Obviously the copier becomes more useful the more slots the type has.

When `*print-readably*` is true, the printed representation is
readable:

    (person "Common Lisp" 33)
    => #.(PERSON "Common Lisp" 33)

(Why override how a structure is normally printed? Structure types
are not necessarily readable unless they have a default (`make-X`)
constructor. Since the type defined by `defconstructor` has only one
constructor, we have to take over to make sure it re-readable.)

Besides being re-readable, the type is also externalizable, with a
method for `make-load-form`:

    (make-load-form (person "Common Lisp" 33))
    => (PERSON "Common Lisp" 33)

Users of Trivia get an extra benefit: defining a type with
`defconstructor` also defines a symmetrical pattern for destructuring
that type.

    (trivia:match (person "Common Lisp" 33)
      ((person name age)
       (list name age)))
    => ("Common Lisp" 33)

Note that the arguments to the pattern are optional:

    (trivia:match (person "Common Lisp" 33)
      ((person name) name))
    => "Common Lisp"

If you don't use Trivia, you can still do destructuring with
`deconstruct`, which returns the slots of a constructor as multiple
values:

    (deconstruct (person "Common Lisp" 33))
    => "Common Lisp", 33

Note also that no predicate is defined for the type, so to test for
the type you must either use `typep` or pattern matching as above.

While it is possible to inherit from a type defined with
`defconstructor` (this is Lisp, I can't stop you), it's a bad idea. In
particular, on Lisps which support it, a type defined with
`defconstructor` is declared to be frozen (sealed), so your new
subtype may not be recognized in type tests that have already been
compiled.

Because `defconstructor` is implemented on top of
`defstruct-read-only`, it shares the limitations of
`defstruct-read-only`. In particular it cannot use inheritance.

The design of `defconstructor` is mostly inspired by Scala's [case
classes](https://docs.scala-lang.org/tour/case-classes.html), with
some implementation tricks from `cl-algebraic-data-type`.

[View source](defining-types.lisp#L247)

### `(defunit name &optional docstring)`

Define a unit type.

A unit type is a type with only one instance.

You can think of a unit type as a singleton without state.

Unit types are useful for many of the same purposes as quoted symbols
(or keywords) but, unlike a symbol, a unit type is tagged with its
own individual type.

[View source](defining-types.lisp#L463)

### `(defunion union &body variants)`

Define an algebraic data type.

Each expression in VARIANTS is either a symbol (in which case it
defines a unit type, as with `defunit`) or a list (in which case it
defines a read-only structure, as with `defconstructor`).

UNION is defined as a type equivalent to the disjunction of all the
member types. A class is also defined, with the same name, but with
angle brackets around it.

[View source](defining-types.lisp#L503)

### `(match-of union expr &body clauses)`

Do pattern matching on an algebraic data type.

UNION should be an algebraic data type.

Each clause in CLAUSES has a pattern as its first element.

If the pattern is a symbol, it matches a unit type.

If the pattern is a list, it matches a constructor.

If the pattern is an underscore, it introduces a default or
fallthrough clause.

If the pattern is a list that starts with `or`, it is a disjunction of
other patterns.

[View source](defining-types.lisp#L588)

## Conditions

### `(maybe-invoke-restart restart &rest values)`

When RESTART is active, invoke it with VALUES.

[View source](conditions.lisp#L18)

## Op

This differs from [the original][GOO] in expecting an extra layer of
parentheses. I find it easier to put the extra parentheses in than to
remember to leave them out. Doing it this way also lets completion
work.

Of course, the extra parentheses make it longer, but the point of
positional lambdas isn't to save typing: it's to save the mental
effort of giving things *names* when all we are interested in is the
*shape* of the code.

[GOO]: http://people.csail.mit.edu/jrb/goo/manual.46/goomanual_15.html#17


### `(op &body body)`

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

OP is intended for simple functions -- one-liners. Parameters are
extracted according to a depth-first walk of BODY. Macro expansion
may, or may not, be done depending on the implementation; it should
not be relied on. Lexical bindings may, or may not, shadow
placeholders -- again, it depends on the implementation. (This means,
among other things, that nested use of `op` is not a good idea.)
Because of the impossibility of a truly portable code walker, `op`
will never be a true replacement for `lambda`. But even if it were
possible to do better, `op` would still only be suited for one-liners.
If you need more than a one-liner, then you should be giving your
parameters names.

(One thing you *can* count on is the ability to use `op` with
quasiquotes. If using placeholders inside quasiquotes does not work on
your Lisp implementation, that's a bug, not a limitation.)

[View source](op.lisp#L180)

### `(opf place expr)`

Like `(callf PLACE (op EXPR))'.
From GOO.

[View source](op.lisp#L232)

## Box

### `(unbox box)`

The value in the box X.

[View source](box.lisp#L27)

### `(box unbox)`

Box a value.

[View source](box.lisp#L27)

### `(boxp g)`

Predicate for a boxed value.

[View source](box.lisp#L27)

### `(ensure-box x)`

Return X if boxed, otherwise a box containing X.

[View source](box.lisp#L80)

## Threads

### `(count-cpus &key default online memoize)`

Try very hard to return a meaningful count of CPUs.
If ONLINE is non-nil, try to return only the active CPUs.

The second value is T if the number of processors could be queried,
`nil` otherwise.

If MEMOIZE is non-nil (default), then memoize the result. Calling with
MEMOIZE nil clears any memoized results.

[View source](threads.lisp#L81)

### `(synchronized (&optional (object nil objectp)) &body body)`

Run BODY holding a unique lock associated with OBJECT.
If no OBJECT is provided, run BODY as an anonymous critical section.

If BODY begins with a literal string, attach the string to the lock
object created (as the argument to `bt:make-recursive-lock`).

[View source](threads.lisp#L139)

### `(monitor object)`

Return a unique lock associated with OBJECT.

[View source](threads.lisp#L156)

## Functions

### `(eqs x)`

Return a one-argument function that tests if its argument is `eq` to X.

[View source](functions.lisp#L5)

### `(eqls x)`

Return a one-argument function that tests if its argument is `eql` to X.

[View source](functions.lisp#L13)

### `(equals x)`

Return a one-argument function that tests if its argument is `equal` to X.

[View source](functions.lisp#L21)

### `(partial fn &rest args)`

Partial application.

Unlike `alexandria:curry`, which is only inlined when you ask it to
be, `partial` is always inlined if possible.

From Clojure.

[View source](functions.lisp#L31)

### `(trampoline fn &rest args)`

Use the trampoline technique to simulate mutually recursive functions.

Call FN with supplied ARGS, if any.

If FN returns a functions, call that function with no arguments.
Repeat until the return value is not a function, and finally return
that non-function value.

Note that, to return a function as a final value, you must wrap it in
some data structure and unpack it.

Most likely to be useful for Lisp implementations that do not provide
tail call elimination.

From Clojure.

[View source](functions.lisp#L63)

### `(define-train name args &body body)`

Define a higher-order function and its compiler macro at once.

When defining a higher-order function it is often a good idea to
write a compiler macro so compilers can inline the resulting lambda
form.

For the special case of a fixed-arity function that only takes other
functions as arguments, you can use `define-train` to define the
function and the compiler macro in one go. The catch is that you have
to write the single definition as a macro.

E.g., if `complement` did not exist, you could define it like so:

    (define-train complement (fn)
      `(lambda (&rest args)
         (not (apply ,fn args))))

Besides providing an implicit compiler macro, `define-train` also
inserts the proper declarations to ensure the compiler recognizes the
function arguments as functions, avoiding runtime type checks.

The term "train" is from J.

[View source](functions.lisp#L86)

### `(flip f)`

Flip around the arguments of a binary function.

That is, given a binary function, return another, equivalent function
that takes its two arguments in the opposite order.

From Haskell.

[View source](functions.lisp#L128)

### `(nth-arg n)`

Return a function that returns only its NTH argument, ignoring all others.

If you've ever caught yourself trying to do something like

    (mapcar #'second xs ys)

then `nth-arg` is what you need.

If `hash-table-keys` were not already defined by Alexandria, you could
define it as:

    (defun hash-table-keys (table)
      (maphash-return (nth-arg 0) table))

[View source](functions.lisp#L138)

### `(distinct &key key test synchronized)`

Return a closure returning only values it has not seen before.

    (defalias test (distinct))
    (test 'foo) => foo, t
    (test 'foo) => nil, nil

The second value is T when the value is distinct.

TEST must be a valid test for a hash table.

This has many uses, for example:

    (count-if (distinct) seq)
    ≡ (length (remove-duplicates seq))

If SYNCHRONIZED is non-nil, then `distinct` can safely be used from
multiple threads. Otherwise it is not thread-safe.

Note the closure returned by `distinct` changes how it tracks unique
items based on the number of items it is tracking, so it is suitable
for all sizes of set.

[View source](functions.lisp#L165)

### `(throttle fn wait &key synchronized memoized)`

Wrap FN so it can be called no more than every WAIT seconds.
If FN was called less than WAIT seconds ago, return the values from the
last call. Otherwise, call FN normally and update the cached values.

WAIT, of course, may be a fractional number of seconds.

The throttled function is not thread-safe by default; use SYNCHRONIZED
to get a version with a lock.

You can pass MEMOIZED if you want the function to remember values
between calls.

[View source](functions.lisp#L226)

### `(once fn)`

Return a function that runs FN only once, caching the results
forever.

[View source](functions.lisp#L279)

### `(fuel level)`

Return a function to count 'fuel' consumption down from the initial level.

The function takes one argument and subtracts its value from the
current fuel level.

The two return values are a boolean indicating whether the available
fuel has been exceeded followed by the current fuel level (which may
be negative.)

[View source](functions.lisp#L295)

### `(juxt &rest fns)`

Clojure's `juxt`.

Return a function which returns a list where each element is the
result of applying one of FNS to the arguments.

It’s actually quite simple, but easier to demonstrate than to explain.
The classic example is to use `juxt` to implement `partition`:

    (defalias partition* (juxt #'filter #'remove-if))
    (partition* #'evenp '(1 2 3 4 5 6 7 8 9 10))
    => '((2 4 6 8 10) (1 3 5 7 9))

The general idea is that `juxt` takes things apart.

[View source](functions.lisp#L317)

### `(dynamic-closure symbols fn)`

Create a dynamic closure.

Some ancient Lisps had closures without lexical binding. Instead, you
could "close over" pieces of the current dynamic environment. When
the resulting closure was called, the symbols closed over would be
bound to their storage at the time the closure was created. These
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

[View source](functions.lisp#L346)

### `(hook f g)`

Monadic hook.
From J.

The hook of f is defined as f(y,g(y)).

For example, you can use a hook to test whether a number is an
integer, by asking whether it is equal to its own floor.

    (hook #'= #'floor)
    (funcall * 2.0)
    => T

AKA Schoenfinkel's S combinator.

[View source](functions.lisp#L379)

### `(fork g f h)`

Monadic fork.

The monadic fork of f, g, and h is defined as

   (f g h) y <-> (f y) g (h y)

The usual example of a monadic fork is defining the mean. Assuming a
`sum` function defined as

   (defun sum (xs)
    (reduce #'+ xs))

you can write a (numerically unstable) `mean` using `fork`.

    (fork #'/ #'sum #'length)
    (funcall * '(1.0 2.0 3.0 4.0))
    => 2.5

From J.

[View source](functions.lisp#L396)

### `(hook2 f g)`

Dyadic hook.

The usual (only?) example of a dyadic hook is an `hour` function that
takes an hour and a count of minutes and returns a fractional count of
hours.

    (hook2 #'+ (partial (flip #'/) 60))
    (funcall * 3.0 15.0)
    => 3.25

From J.

[View source](functions.lisp#L421)

### `(fork2 g f h)`

Dyadic fork.

The dyadic fork of f, g, and h is defined as:

    x (f g h) y <-> (x f y) g (x h y)

For example, say you wanted a "plus or minus" operator. Given
numbers x and y, it returns a list of x+y and x-y. This can easily be
written as a dyadic fork.

    (fork2 #'list #'+ #'-)
    (funcall * 10 2)
    => '(12 8)

From J.

[View source](functions.lisp#L437)

### `(capped-fork g h)`

J's capped fork (monadic).

Like a monadic fork, but F is omitted.

Effectively the composition of G and H.

[View source](functions.lisp#L458)

### `(capped-fork2 g h)`

J's capped fork (dyadic).

Like a dyadic fork, but F is omitted.

[View source](functions.lisp#L467)

### `(fnil fn &rest defaults)`

Return a function that ORs its arguments with DEFAULTS.

If the first argument is nil, then the first default in DEFAULTS is
used instead; if the second argument is nil, then the second default
in DEFAULTS is used instead; and so on until we run out of DEFAULTS.

The minimum arity is equal to the length of DEFAULTS.

This has a compiler macro for reasonable efficiency.

From Clojure.

[View source](functions.lisp#L474)

### `(variadic->unary fn)`

Return a function that takes a single argument, a list, and
applies FN to it.

Practically equivalent to `(curry #'apply FN arguments...)'.

[View source](functions.lisp#L513)

### `(unary->variadic fn)`

Return a function that takes any number of arguments and calls FN
on them as a list.

Wraps a function that expects a single argument, a list, so it can be
used variadically.

[View source](functions.lisp#L523)

### `(mvconstantly &rest values)`

Like `constantly`, but returns all of VALUES as multiple values.
If there are not VALUES, returns nothing.

[View source](functions.lisp#L533)

### `(do-nothing &rest args)`

Do nothing and return nothing.
This function is meant as a placeholder for a function argument.

From LispWorks.

[View source](functions.lisp#L569)

### `(repeat-until-stable fn x &key test max-depth)`

Takes a single-argument FN and calls (fn x), then (fn (fn x)), and so on
until the result doesn't change according to TEST. If MAX-DEPTH is specified
then FN will be called at most MAX-DEPTH times even if the result is still changing.

Returns two values, the stable result of FN and the remainder of
MAX-DEPTH. (If this value is 0, the result is unstable.)

[View source](functions.lisp#L581)

## Trees

### `(reuse-cons x y x-y)`

If X and Y are the car and cdr of X-Y, return X-Y.

Otherwise, return a fresh cons of X and Y.

[View source](trees.lisp#L4)

### `(car+cdr list)`

Given LIST, return its car and cdr as two values.

[View source](trees.lisp#L16)

### `(walk-tree fun tree &key tag traversal)`

Call FUN in turn over each atom and cons of TREE.

FUN can skip the current subtree with (throw TAG nil).

[View source](trees.lisp#L21)

### `(map-tree fun tree &key tag traversal)`

Walk FUN over TREE and build a tree from the results.

The new tree may share structure with the old tree.

     (eq tree (map-tree #'identity tree)) => T

FUN can skip the current subtree with (throw TAG SUBTREE), in which
case SUBTREE will be used as the value of the subtree.

TRAVERSE can be one of `:preorder`, `:postorder`, or `:inorder`. The
default is `:preorder`.

[View source](trees.lisp#L36)

### `(leaf-walk fun tree)`

Call FUN on each leaf of TREE.

[View source](trees.lisp#L122)

### `(leaf-map fn tree)`

Call FN on each leaf of TREE.
Return a new tree possibly sharing structure with TREE.

[View source](trees.lisp#L136)

### `(occurs-if test tree &key key traversal)`

Is there a node (leaf or cons) in TREE that satisfies TEST?

[View source](trees.lisp#L148)

### `(prune-if test tree &key key)`

Remove any atoms satisfying TEST from TREE.

Pruning is defined "modulo flatten": you should get the same result
from pruning, and then flattening, that you would get from flattening,
and then filtering.

Also note that pruning is not defined for trees containing improper
lists.

[View source](trees.lisp#L158)

### `(occurs node tree &key key test traversal)`

Is NODE present in TREE?

[View source](trees.lisp#L183)

### `(prune leaf tree &key key test)`

Remove LEAF from TREE wherever it occurs.
See `prune-if` for more information.

[View source](trees.lisp#L191)

## Hash Tables

### `(do-hash-table (key value table &optional return) &body body)`

Iterate over hash table TABLE, in no particular order.

At each iteration, a key from TABLE is bound to KEY, and the value of
that key in TABLE is bound to VALUE.

[View source](hash-tables.lisp#L3)

### `(hash-table-test-p test)`

Is TEST a valid hash table test?

[View source](hash-tables.lisp#L21)

### `(dict &rest keys-and-values)`

A concise constructor for hash tables.

    (gethash :c (dict :a 1 :b 2 :c 3)) => 3, T

By default, return an 'equal hash table containing each successive
pair of keys and values from KEYS-AND-VALUES.

If the number of KEYS-AND-VALUES is odd, then the first argument is
understood as the test.

     (gethash "string" (dict "string" t)) => t
     (gethash "string" (dict 'eq "string" t)) => nil

Note that `dict` can also be used for destructuring (with Trivia).

    (match (dict :x 1)
      ((dict :x x) x))
    => 1

[View source](hash-tables.lisp#L72)

### `(dict* dict &rest args)`

Merge new bindings into DICT.
Roughly equivalent to `(merge-tables* DICT (dict args...))'

DICT argument is modified and returned as the result.

[View source](hash-tables.lisp#L132)

### `(dictq &rest keys-and-values)`

A literal hash table.
Like `dict`, but the keys and values are implicitly quoted, and the
hash table is inlined as a literal object.

[View source](hash-tables.lisp#L141)

### `(href table &rest keys)`

A concise way of doing lookups in (potentially nested) hash tables.

    (href (dict :x 1) :x) => 1
    (href (dict :x (dict :y 2)) :x :y)  => 2

[View source](hash-tables.lisp#L148)

### `(href-default default table &rest keys)`

Like `href`, with a default.
As soon as one of KEYS fails to match, DEFAULT is returned.

[View source](hash-tables.lisp#L161)

### `(@ table &rest keys)`

A concise way of doing lookups in (potentially nested) hash tables.

    (@ (dict :x 1) :x) => 1
    (@ (dict :x (dict :y 2)) :x :y)  => 2

[View source](hash-tables.lisp#L210)

### `(pophash key hash-table)`

Lookup KEY in HASH-TABLE, return its value, and remove it.

This is only a shorthand. It is not in itself thread-safe.

From Zetalisp.

[View source](hash-tables.lisp#L231)

### `(swaphash key value hash-table)`

Set KEY and VALUE in HASH-TABLE, returning the old values of KEY.

This is only a shorthand. It is not in itself thread-safe.

From Zetalisp.

[View source](hash-tables.lisp#L243)

### `(hash-fold fn init hash-table)`

Reduce TABLE by calling FN with three values: a key from the hash
table, its value, and the return value of the last call to FN. On the
first call, INIT is supplied in place of the previous value.

From Guile.

[View source](hash-tables.lisp#L253)

### `(maphash-return fn hash-table)`

Like MAPHASH, but collect and return the values from FN.
From Zetalisp.

[View source](hash-tables.lisp#L268)

### `(maphash-new fn hash-table &rest hash-table-args &key &allow-other-keys)`

Like MAPHASH, but builds and returns a new hash table.

FN is a function of two arguments, like the function argument to
`maphash`. It is required, however, to return two values, a new key
and a new value.

If `copy-hash-table` did not exist, you could define it as:

    (maphash-new #'values hash-table)

Note it is not necessarily the case that the new hash table will have
the same number of entries as the old hash table, since FN might
evaluate to the same key more than once.

By default, the new hash table has the same hash table
properties (test, size) as HASH-TABLE, but these can be overridden
with HASH-TABLE-ARGS.

[View source](hash-tables.lisp#L281)

### `(maphash-into hash-table fn &rest seqs)`

Map FN over SEQS, updating HASH-TABLE with the results. Return HASH-TABLE.

FN is required to return two values, and key and a value.

[View source](hash-tables.lisp#L310)

### `(merge-tables* table &rest tables)`

Modifies the first TABLE by merging all keys from other TABLES into it.

Returns the first TABLE.

[View source](hash-tables.lisp#L325)

### `(merge-tables &rest tables)`

Merge TABLES, working from left to right.
The resulting hash table has the same parameters as the first table.

If no tables are given, an new, empty hash table is returned.

If a single table is given, a copy of it is returned.

If the same key is present in two tables, the value from the rightmost
table is used.

All of the tables being merged must have the same value for
`hash-table-test`.

Clojure's `merge`.


[View source](hash-tables.lisp#L337)

### `(flip-hash-table table &rest hash-table-args &key filter key test size rehash-size rehash-threshold)`

Return a table like TABLE, but with keys and values flipped.

     (gethash :y (flip-hash-table (dict :x :y)))
     => :x, t

TEST allows you to filter which keys to set.

     (def number-names (dictq 1 one 2 two 3 three))

     (def name-numbers (flip-hash-table number-names))
     (def name-odd-numbers (flip-hash-table number-names :filter #'oddp))

     (gethash 'two name-numbers) => 2, t
     (gethash 'two name-odd-numbers) => nil, nil

KEY allows you to transform the keys in the old hash table.

     (def negative-number-names (flip-hash-table number-names :key #'-))
     (gethash 'one negative-number-names) => -1, t

KEY defaults to `identity`.

[View source](hash-tables.lisp#L366)

### `(set-hash-table set &rest hash-table-args &key test key strict &allow-other-keys)`

Return SET, a list considered as a set, as a hash table.
This is the equivalent of Alexandria's `alist-hash-table` and
`plist-hash-table` for a list that denotes a set.

STRICT determines whether to check that the list actually is a set.

The resulting hash table has the elements of SET for both its keys and
values. That is, each element of SET is stored as if by
     (setf (gethash (key element) table) element)

[View source](hash-tables.lisp#L401)

### `(hash-table-set table &key strict test key)`

Return the set denoted by TABLE.
Given STRICT, check that the table actually denotes a set.

Without STRICT, equivalent to `hash-table-values`.

[View source](hash-tables.lisp#L433)

### `(hash-table-predicate hash-table)`

Return a predicate for membership in HASH-TABLE.
The predicate returns the same two values as `gethash`, but in the
opposite order.

[View source](hash-tables.lisp#L444)

### `(hash-table-function hash-table &key read-only strict key-type value-type strict-types default)`

Return a function for accessing HASH-TABLE.

Calling the function with a single argument is equivalent to `gethash`
against a copy of HASH-TABLE at the time HASH-TABLE-FUNCTION was
called.

    (def x (make-hash-table))

    (funcall (hash-table-function x) y)
    ≡ (gethash y x)

If READ-ONLY is nil, then calling the function with two arguments is
equivalent to `(setf (gethash ...))' against HASH-TABLE.

If STRICT is non-nil, then the function signals an error if it is
called with a key that is not present in HASH-TABLE. This applies to
setting keys, as well as looking them up. Pass `:strict :read` if you
only want strict checking for lookups.

DEFAULT is the default value to return from `gethash`.

The function is able to restrict what types are permitted as keys and
values. If KEY-TYPE is specified, an error will be signaled if an
attempt is made to get or set a key that does not satisfy KEY-TYPE. If
VALUE-TYPE is specified, an error will be signaled if an attempt is
made to set a value that does not satisfy VALUE-TYPE. However, the
hash table provided is *not* checked to ensure that the existing
pairings satisfy KEY-TYPE and VALUE-TYPE -- not unless STRICT-TYPES is
also specified.

[View source](hash-tables.lisp#L454)

### `(make-hash-table-function &rest args &key &allow-other-keys)`

Call `hash-table-function` on a fresh hash table.
ARGS can be args to `hash-table-function` or args to
`make-hash-table`, as they are disjoint.

[View source](hash-tables.lisp#L560)

### `(delete-from-hash-table table &rest keys)`

Return TABLE with KEYS removed (as with `remhash`).
Cf. `delete-from-plist` in Alexandria.

[View source](hash-tables.lisp#L568)

### `(pairhash keys data &optional hash-table)`

Like `pairlis`, but for a hash table.

Unlike `pairlis`, KEYS and DATA are only required to be sequences (of
the same length), not lists.

By default, the hash table returned uses `eql` as its tests. If you
want a different test, make the table yourself and pass it as the
HASH-TABLE argument.

[View source](hash-tables.lisp#L577)

### `(pretty-print-hash-table ht &optional stream)`

Pretty print the hash-table HT to STREAM.

```
(pretty-print-hash-table (dict :a 1 :b 2 :c 3))
;; =>
(dict
  :A 1
  :B 2
  :C 3
 )
```

If you want to always pretty print hash tables, you can set this in your init file:

``` lisp
(toggle-pretty-print-hash-table)
```

  Ported from RUTILS.

[View source](hash-tables.lisp#L599)

### `(toggle-pretty-print-hash-table &optional on)`

Toggles printing hash-tables with PRETTY-PRINT-HASH-TABLE or with the default method.
    If ON is set explicitly, turn on literal printing (T), otherwise use the default (NIL).

    Ported from RUTILS.

[View source](hash-tables.lisp#L649)

## Files

### `(with-open-files (&rest args) &body body)`

A simple macro to open one or more files providing the streams for the BODY. The ARGS is a list of `(stream filespec options*)` as supplied to WITH-OPEN-FILE.

[View source](files.lisp#L62)

### `(path-basename pathname)`

Return the basename, that is:
- if it's a directory, the name of the directory,
- if it's a file, the name of the file including its type (extension).

[View source](files.lisp#L73)

### `(path-join &rest pathnames)`

Build a pathname by merging from right to left.
With `path-join` you can pass the elements of the pathname being built
in the order they appear in it:

    (path-join (user-homedir-pathname) config-dir config-file)
    ≡ (uiop:merge-pathnames* config-file
       (uiop:merge-pathnames* config-dir
        (user-homedir-pathname)))

Note that `path-join` does not coerce the parts of the pathname into
directories; you have to do that yourself.

    (path-join "dir1" "dir2" "file") -> #p"file"
    (path-join "dir1/" "dir2/" "file") -> #p"dir1/dir2/file"

Cf. `base-path-join` for a similar function with more intuitive
behavior.

[View source](files.lisp#L81)

### `(base-path-join base &rest suffixes)`

Build a pathname by appending SUFFIXES to BASE.
For `path-join-base`, the path on the left is always the *base* and
the path on the right is always the *suffix*. This means that even if
the right hand path is absolute, it will be treated as if it were
relative.

    (base-path-join #p"foo/bar" #p"/baz")
    => #p"foo/bar/baz")

Also, a bare file name as a suffix does not override but is appended
to the accumulated file name. This includes the extension.

    (base-path-join #p"foo/bar" "baz")
    => #p"foo/barbaz")

    (base-path-join #p"foo/bar.x" "baz.y")
    => #p"foo/bar.xbaz.y")

See `path-join` for a similar function with more consistent behavior.

[View source](files.lisp#L107)

### `(write-stream-into-file stream pathname &key if-exists if-does-not-exist)`

Read STREAM and write the contents into PATHNAME.

STREAM will be closed afterwards, so wrap it with
`make-concatenated-stream` if you want it left open.

[View source](files.lisp#L147)

### `(write-file-into-stream pathname output &key if-does-not-exist external-format)`

Write the contents of FILE into STREAM.

[View source](files.lisp#L161)

### `(file= file1 file2 &key buffer-size)`

Compare FILE1 and FILE2 octet by octet, (possibly) using buffers
of BUFFER-SIZE.

[View source](files.lisp#L171)

### `(file-size file &key element-type)`

The size of FILE, in units of ELEMENT-TYPE (defaults to bytes).

The size is computed by opening the file and getting the length of the
resulting stream.

If all you want is to read the file's size in octets from its
metadata, consider `trivial-file-size:file-size-in-octets` instead.

[View source](files.lisp#L232)

### `(exe p)`

If P, a pathname designator, has no extension, then, on Windows
only, add an extension of `.exe`.

[View source](files.lisp#L248)

### `(resolve-executable p)`

Look for an executable using the PATH environment variable.
P is a pathname designator.

On Windows only, if P does not have an extension, it assumed to end in
`.exe`.

Note that this function does not check the current directory (even on
Windows) and it does not care if P is already an absolute pathname: it
only cares about its name and type.

[View source](files.lisp#L268)

### `(format-file-size-human-readable stream file-size &key flavor space suffix)`

Write FILE-SIZE, a file size in bytes, to STREAM, in human-readable form.

STREAM is interpreted as by `format`.

If FLAVOR is nil, kilobytes are 1024 bytes and SI prefixes are used.

If FLAVOR is `:si`, kilobytes are 1000 bytes and SI prefixes are used.

If FLAVOR is `:iec`, kilobytes are 1024 bytes and IEC prefixes (Ki,
Mi, etc.) are used.

If SPACE is non-nil, include a space between the number and the
prefix. (Defaults to T if FLAVOR is `:si`.)

SUFFIX is the suffix to use; defaults to B if FLAVOR is `:iec`,
otherwise empty.

[View source](files.lisp#L288)

### `(file-size-human-readable file &key flavor space suffix stream)`

Format the size of FILE (in octets) using `format-file-size-human-readable`.
The size of file is found by `trivial-file-size:file-size-in-octets`.

Inspired by the function of the same name in Emacs.

[View source](files.lisp#L316)

## Symbols

### `(find-keyword string)`

If STRING has been interned as a keyword, return it.

Like `make-keyword`, but preferable in most cases, because it doesn't
intern a keyword -- which is usually both unnecessary and unwise.

[View source](symbols.lisp#L9)

### `(bound-value s &optional default)`

If S is bound, return (values s t). Otherwise, return DEFAULT and nil.

[View source](symbols.lisp#L26)

## Arrays

### `(array-index-row-major array row-major-index)`

The inverse of ARRAY-ROW-MAJOR-INDEX.

Given an array and a row-major index, return a list of subscripts.

     (apply #'aref (array-index-row-major i))
     ≡ (array-row-major-aref i)

[View source](arrays.lisp#L4)

## Queue

Norvig-style queues, but wrapped in objects so they don't overflow the
printer, and with a more concise, Arc-inspired API.


### `(queuep g)`

Test for a queue.

[View source](queue.lisp#L9)

### `(queue &rest initial-contents)`

Build a new queue with INITIAL-CONTENTS.

[View source](queue.lisp#L85)

### `(clear-queue queue)`

Return QUEUE's contents and reset it.

[View source](queue.lisp#L104)

### `(qlen queue)`

The number of items in QUEUE.

[View source](queue.lisp#L112)

### `(qlist queue)`

A list of the items in QUEUE.
Does not cons.

[View source](queue.lisp#L117)

### `(enq item queue)`

Insert ITEM at the end of QUEUE.

[View source](queue.lisp#L123)

### `(deq queue)`

Remove item from the front of the QUEUE.

[View source](queue.lisp#L132)

### `(undeq item queue)`

Add an item to the front of QUEUE.
For an empty queue, this does the same thing as ENQ.

For a queue with elements, this adds a new element onto the front of
queue (like pushing to an ordinary list).

This is called `undeq` because it can be used to undo a `deq`.

[View source](queue.lisp#L144)

### `(queue-empty-p queue)`

Is QUEUE empty?

[View source](queue.lisp#L159)

### `(front queue)`

The first element in QUEUE.

[View source](queue.lisp#L164)

### `(qback queue)`

Get the last element of a queue.

[View source](queue.lisp#L176)

### `(qconc queue list)`

Destructively concatenate LIST onto the end of QUEUE.
Return the queue.

[View source](queue.lisp#L189)

### `(qappend queue list)`

Append the elements of LIST onto the end of QUEUE.
Return the queue.

[View source](queue.lisp#L200)

### `(qprepend list queue)`

Insert ITEMS at the beginning of QUEUE.

[View source](queue.lisp#L209)

### `(qpreconc list queue)`

Destructively splice LIST at the beginning of QUEUE.

[View source](queue.lisp#L213)

### `(copy-queue queue)`

Copy QUEUE as another queue.

[View source](queue.lisp#L223)

## Numbers

### `(fixnump n)`

Same as `(typep N 'fixnum)'.

[View source](numbers.lisp#L3)

### `(null-if-zero x)`

If X is a nonzero number, return it, otherwise return nil.
The second value is T if X was nonzero.

[View source](numbers.lisp#L10)

### `(finc place &optional (delta 1))`

DEPRECATED: use `shift-incf` instead.

[View source](numbers.lisp#L16)

### `(shift-incf ref &optional (delta 1))`

Like `incf`, but returns the old value instead of the new.

    (shift-incf x n)
    ≡ (shiftf x (+ x n))

In C terms, this is a postincrement while `incf` is a preincrement.

An alternative to using -1 as the starting value of a counter, which
can prevent optimization.

[View source](numbers.lisp#L22)

### `(fdec place &optional (delta 1))`

DEPRECATED: use `shift-decf` instead.

[View source](numbers.lisp#L33)

### `(shift-decf ref &optional (delta 1))`

Like `decf`, but returns the old value instead of the new.

    (shift-decf x n)
    ≡ (shiftf x (- x n))

In C terms, this is a postdecrement while `decf` is a predecrement.

[View source](numbers.lisp#L39)

### `(parse-float string &key start end junk-allowed type)`

Parse STRING as a float of TYPE.

The type of the float is determined by, in order:
- TYPE, if it is supplied;
- The type specified in the exponent of the string;
- or `*read-default-float-format*`.

The second return value is upper bounding index of the substring that is parsed,
as in `parse-integer`.

     (parse-float "1.0") => 1.0s0, 3
     (parse-float "1.0d0") => 1.0d0, 5
     (parse-float "1.0s0" :type 'double-float) => 1.0d0, 5

Of course you could just use `parse-number`, but sometimes only a
float will do.

[View source](numbers.lisp#L139)

### `(round-to number &optional divisor)`

Like `round`, but return the resulting number.

     (round 15 10) => 2
     (round-to 15 10) => 20

[View source](numbers.lisp#L202)

### `(bits int &key big-endian)`

Return a bit vector of the bits in INT.
Defaults to little-endian.

[View source](numbers.lisp#L211)

### `(unbits bits &key big-endian)`

Turn a sequence of BITS into an integer.
Defaults to little-endian.

[View source](numbers.lisp#L233)

### `(shrink n by)`

Decrease N by a factor.

[View source](numbers.lisp#L250)

### `(grow n by)`

Increase N by a factor.

[View source](numbers.lisp#L254)

### `(shrinkf g n)`

Shrink the value in a place by a factor.

[View source](numbers.lisp#L258)

### `(growf g n)`

Grow the value in a place by a factor.

[View source](numbers.lisp#L261)

### `(random-in-range low high)`

Random number in the range [low,high).

LOW and HIGH are automatically swapped if HIGH is less than LOW.

Note that the value of LOW+HIGH may be greater than the range that can
be represented as a number in CL. E.g., you can generate a random double float with

    (random-in-range most-negative-double-float most-positive-double-float)

even though (+ most-negative-double-float most-positive-double-float)
would cause a floating-point overflow.

From Zetalisp.

[View source](numbers.lisp#L264)

### `(float-precision-contagion &rest ns)`

Perform numeric contagion on the elements of NS.

That is, if any element of NS is a float, then every number in NS will
be returned as "a float of the largest format among all the
floating-point arguments to the function".

This does nothing but numeric contagion: the number of arguments
returned is the same as the number of arguments given.

[View source](numbers.lisp#L344)

## Octets

### `(octet-vector-p x)`

Is X an octet vector?

[View source](octets.lisp#L4)

### `(make-octet-vector size)`

Make an octet vector of SIZE elements.

[View source](octets.lisp#L9)

### `(octets n &key big-endian)`

Return N, an integer, as an octet vector.
Defaults to little-endian order.

[View source](octets.lisp#L27)

### `(unoctets bytes &key big-endian)`

Concatenate BYTES, an octet vector, into an integer.
Defaults to little-endian order.

[View source](octets.lisp#L52)

### `(octet-vector= v1 v2 &key start1 end1 start2 end2)`

Like `string=` for octet vectors.

[View source](octets.lisp#L89)

## Time

### `(universal-to-unix time)`

Convert a universal time to a Unix time.

[View source](time.lisp#L18)

### `(unix-to-universal time)`

Convert a Unix time to a universal time.

[View source](time.lisp#L22)

### `(get-unix-time)`

The current time as a count of seconds from the Unix epoch.

[View source](time.lisp#L26)

### `(date-leap-year-p year)`

Is YEAR a leap year in the Gregorian calendar?

[View source](time.lisp#L30)

### `(time-since time)`

Return seconds since TIME.

[View source](time.lisp#L37)

### `(time-until time)`

Return seconds until TIME.

[View source](time.lisp#L41)

### `(interval &key seconds minutes hours days weeks months years month-days year-days)`

A verbose but readable way of specifying intervals in seconds.

Intended as a more readable alternative to idioms
like (let ((day-in-seconds #.(* 24 60 60))) ...)

Has a compiler macro.

[View source](time.lisp#L45)

## Clos

### `(make class &rest initargs &key &allow-other-keys)`

Shorthand for `make-instance`.
Unlike `make-instance`, this is not a generic function, so it can do
more compile-time argument checking.

Also unlike `make-instance`, `make` is defined to always return a
single value. It also declares its return type (as `standard-object`,
or also `structure-object` if the implementation allows
`make-instance` on structures). This may allow the compiler to warn
you if you (e.g.) try to treat the return value as a list or number.

After Eulisp.

[View source](clos.lisp#L29)

### `(class-name-of x)`

The class name of the class of X.

[View source](clos.lisp#L60)

### `(class-name-safe x)`

The class name of the class of X.
If X is a class, the name of the class itself.

[View source](clos.lisp#L65)

### `(find-class-safe x &optional env)`

The class designated by X.
If X is a class, it designates itself.

[View source](clos.lisp#L74)

### `(slot-value-safe instance slot-name &optional default)`

Like `slot-value`, but doesn't signal errors.
Returns three values:
1. The slot's value (or nil),
2. A boolean that is T if the slot exists and *was* bound,
3. A boolean that is T if the slot exists.

Note that this function does call `slot-value` (if the slot exists),
so if there is a method on `slot-unbound` for the class it will be
invoked. In this case the second value will still be `nil`, however.

[View source](clos.lisp#L85)

### `(defmethods class (self . slots) &body body)`

Concisely define methods that specialize on the same class.

You can already use `defgeneric` to define an arbitrary number of
methods on a single generic function without having to repeat the name
of the function:

    (defgeneric fn (x)
      (:method ((x string)) ...)
      (:method ((x number)) ...))

Which is equivalent to:

    (defgeneric fn (x))

    (defmethod fn ((x string))
      ...)

    (defmethod fn ((x number))
      ...)

Similarly, you can use `defmethods` to define methods that specialize
on the same class, and access the same slots, without having to
repeat the names of the class or the slots:

    (defmethods my-class (self x y)
      (:method initialize-instance :after (self &key)
        ...)
      (:method print-object (self stream)
        ...)
      (:method some-method ((x string) self)
        ...))

Which is equivalent to:

    (defmethod initialize-instance :after ((self my-class) &key)
      (with-slots (x y) self
        ...))

    (defmethod print-object ((self my-class) stream)
      (with-slots (x y) self
        ...))

    (defmethod some-method ((x string) (self my-class))
      (with-slots (y) self              ;!
        ...))

Note in particular that `self` can appear in any position, and that
you can freely specialize the other arguments.

Just as in `with-slots`, slots can be renamed:

    (defmethods my-class (self (abscissa x) (ordinate y))
      ...)

You can also use `defmethods` in place of `with-accessors`, by using a
function-quote:

    (defmethods my-class (self (x #'my-class-x)
                               (y #'my-class-y))
      ...)

(The difference from using `with-slots` is the scope of the slot
bindings: they are established *outside* of the method definition,
which means argument bindings shadow slot bindings:

    (some-method "foo" (make 'my-class :x "bar"))
    => "foo"

Since slot bindings are lexically outside the argument bindings, this
is surely correct, even if it makes `defmethods` slightly harder to
explain in terms of simpler constructs.)

Is `defmethods` trivial? Yes, in terms of its implementation. This
docstring is far longer than the code it documents. But you may find
it does a lot to keep heavily object-oriented code readable and
organized, without any loss of power.

Note that `defmethods` may also be useful when converting state
machines written using `labels` into an object-oriented style.

This construct is very loosely inspired by impl blocks in Rust.

[View source](clos.lisp#L112)

## Hooks

### `(add-hook hook fn &key append)`

Add FN to the value of HOOK.

[View source](hooks.lisp#L6)

### `(remove-hook hook fn)`

Remove FN from the symbol value of HOOK.

[View source](hooks.lisp#L16)

### `(with-hook-restart &body body)`

NO DOCS!

[View source](hooks.lisp#L22)

### `(run-hooks &rest hooks)`

Run all the hooks in HOOKS, without arguments.
The variable `*hook*` is bound to the name of each hook as it is being
run.

[View source](hooks.lisp#L26)

### `(run-hook hook &rest args)`

Apply each function in HOOK to ARGS.

[View source](hooks.lisp#L33)

### `(run-hook-until-failure hook &rest args)`

Like `run-hook-with-args`, but quit once a function returns nil.

[View source](hooks.lisp#L40)

### `(run-hook-until-success hook &rest args)`

Like `run-hook-with-args`, but quit once a function returns
non-nil.

[View source](hooks.lisp#L46)

## Fbind

### `(fbind bindings &body body)`

Binds values in the function namespace.

That is,
     (fbind ((fn (lambda () ...))))
     ≡ (flet ((fn () ...))),

except that a bare symbol in BINDINGS is rewritten as (symbol
symbol).

[View source](fbind.lisp#L155)

### `(fbind* bindings &body body)`

Like `fbind`, but creates bindings sequentially.

[View source](fbind.lisp#L211)

### `(fbindrec bindings &body body)`

Like `fbind`, but creates recursive bindings.

The consequences of referring to one binding in the expression that
generates another are undefined.

[View source](fbind.lisp#L300)

### `(fbindrec* bindings &body body)`

Like `fbindrec`, but the function defined in each binding can be
used in successive bindings.

[View source](fbind.lisp#L343)

## Static Let

### `(recklessly-continue &optional condition)`

Invokes the last bound RECKLESSLY-CONTINUE restart. Returns NIL if
no such restart was bound or if the restart failed to transfer control.

[View source](static-let.lisp#L71)

### `(static-binding-flush-error-group static-binding-flush-error)`

NO DOCS!

[View source](static-let.lisp#L92)

### `(static-binding-flush-error-all-groups-p static-binding-flush-error)`

NO DOCS!

[View source](static-let.lisp#L92)

### `(static-binding-flush-error &optional group)`

NO DOCS!

[View source](static-let.lisp#L103)

### `(static-binding-active-error group &optional all-groups-p)`

NO DOCS!

[View source](static-let.lisp#L136)

### `(flush-static-binding-group group &key are-you-sure-p)`

Flushes all static binding values in binding group `group` and
restores them to their uninitialized state, forcing any initforms
for these static bindings to be reevaluated whenever control
next reaches the respective `static-let`/`static-let*`. Returns the
number of live bindings flushed that way.

This operation is unsafe to perform while any other threads are
trying to access these bindings; proper synchronization is left
to the user. Therefore, a continuable error is signaled unless
Lisp is running single-threaded or `are-you-sure-p` is true.

Note that a static binding that was created as `:flushablep nil'
will not be affected by this operation.

[View source](static-let.lisp#L174)

### `(flush-all-static-binding-groups)`

Flush all static binding values in ALL binding groups and
restore them to their uninitialized state, forcing any initforms
for these static bindings to be reevaluated whenever control
next reaches the respective `static-let`/`static-let*`.  Returns the
number of live bindings flushed that way.

This operation is unsafe to perform while any other threads are
trying to access these bindings; proper synchronization is left
to the user. In addition, this operation will clear ALL values,
including these which were not bound by the programmer. This can
lead to unintended behavior, hence, a continuable error is signaled
unless Lisp is running single-threaded.

This function is useful e.g. when deploying Lisp binaries in order
to not include static binding values in the resulting Lisp image.

Note that a static binding that was created as `:flushablep nil'
will not be affected by this operation.

[View source](static-let.lisp#L196)

### `(static-let (&rest bindings) &body body)`

Like `let`, except the variables are only initialized once and
retain their values between different invocations of `body`.

Every static binding is similar to a `let` binding, except it can have
additional keyword arguments:

- `type` Denotes the type of the variable.
- `once` If true, then binding initialization and mutation will be
         thread-safe.
- `flush` If true, this binding will be flushable. Defaults to true.
- `in` Denotes the static binding group in which the binding will be
       placed for flushing. Defaults to the value of `*package`.
- `read-only` If true, then the binding cannot be mutated with `setf`.

Static bindings can be flushed via `flush-static-binding-group` and
`flush-all-static-binding-groups`; the latter is automatically pushed
into `uiop:*dump-image-hooks*` by Serapeum.

An unflushable static binding will carry its value over into dumped
Lisp binaries.

[View source](static-let.lisp#L397)

### `(static-let* (&rest bindings) &body body)`

Like `let*`, except the variables are only initialized once and
retain their values between different invocations of `body`.

Every static binding is similar to a `let` binding, except it can have
additional keyword arguments:

- `type` Denotes the type of the variable.
- `once` If true, then binding initialization and mutation will be
         thread-safe.
- `flush` If true, this binding will be flushable. Defaults to true.
- `in` Denotes the static binding group in which the binding will be
       placed for flushing. Defaults to the value of `*package`.
- `read-only` If true, then the binding cannot be mutated with `setf`.

Static bindings can be flushed via `flush-static-binding-group` and
`flush-all-static-binding-groups`; the latter is automatically pushed
into `uiop:*dump-image-hooks*` by Serapeum.

An unflushable static binding will carry its value over into dumped
Lisp binaries.

[View source](static-let.lisp#L420)

## Reader

### `(with-standard-input-syntax &body body)`

Like `with-standard-io-syntax`, but only bind the variables that
control the reader, not the printer.

This may be preferable to using `with-standard-io-syntax` when loading
data, as it will not effect how errors are printed, thus preserving
debugging information.

[View source](reader.lisp#L22)

## Packages

### `(package-exports &optional package)`

Return a list of the symbols exported by PACKAGE.

[View source](packages.lisp#L3)

### `(package-names package)`

Return a list of all the names of PACKAGE: its name and its nicknames.

[View source](packages.lisp#L8)

### `(package-name-keyword package)`

Return the name of PACKAGE as a keyword.

[View source](packages.lisp#L13)

### `(find-external-symbol string package &key error)`

If PACKAGE exports a symbol named STRING, return it.
If PACKAGE does not contain such a symbol, or if the symbol is not
exported, then `nil` is returned, unless ERROR is non-nil, in which
case an error is signaled.

[View source](packages.lisp#L19)

### `(export-always symbols &optional (package nil package-supplied?))`

Like `export`, but also evaluated at compile time.

[View source](packages.lisp#L35)

### `(export-only export/s &optional package)`

Like EXPORT, but unexport any other, existing exports.

[View source](packages.lisp#L40)

### `(export-only-always symbols &optional (package nil package-supplied?))`

Like `export-only`, but also evaluated at compile time.

[View source](packages.lisp#L48)

## Heap

### `(make-heap &key size element-type key test)`

NO DOCS!

[View source](heap.lisp#L6)

### `(heap-insert heap new-item)`

Insert NEW-ITEM into HEAP.

[View source](heap.lisp#L73)

### `(heap-maximum heap)`

Return (without extracting) the greatest element in HEAP.

[View source](heap.lisp#L88)

### `(heap-extract heap i)`

Destructively extract the element in heap at index I, counting from
the greatest element.

[View source](heap.lisp#L94)

### `(heap-extract-maximum heap)`

Destructively extract the greatest element of HEAP.

[View source](heap.lisp#L107)

### `(heap-extract-all heap)`

Destructively extract all the elements of HEAP from greatest to least.

[View source](heap.lisp#L111)

## Lists

### `(filter-map fn list &rest lists)`

Map FN over (LIST . LISTS) like `mapcar`, but omit empty results.

     (filter-map fn ...)
     ≅ (remove nil (mapcar fn ...))

[View source](lists.lisp#L9)

### `(car-safe x)`

The car of X, or nil if X is not a cons.

This is different from Alexandria’s `ensure-car`, which returns the atom.

    (ensure-car '(1 . 2)) => 1
    (car-safe '(1 . 2)) => 1
    (ensure-car 1) => 1
    (car-safe 1) => nil

From Emacs Lisp.

[View source](lists.lisp#L35)

### `(cdr-safe x)`

The cdr of X, or nil if X is not a cons.
From Emacs Lisp.

[View source](lists.lisp#L48)

### `(append1 list item)`

Append an atom to a list.

    (append1 list item)
    ≡ (append list (list item))

[View source](lists.lisp#L53)

### `(nconc1 list item)`

Like `append1`, but destructive.

[View source](lists.lisp#L60)

### `(prepend &rest lists)`

Construct and return a list by concatenating LISTS in reverse order.

    (prepend list-1 list-2)
    ≡ (append list-2 list-1)

[View source](lists.lisp#L64)

### `(prependf g &rest lists)`

Modify-macro for prepend. Prepends LISTS to the PLACE designated by the first argument.

[View source](lists.lisp#L71)

### `(push-end item place)`

Destructively push ITEM to the end of PLACE.
Like `push`, but affects the last item rather than the first.

You may want to use `enq` on a `queue` instead.

From LispWorks.

[View source](lists.lisp#L74)

### `(push-end-new item place &rest kwargs &key key test test-not)`

Pushes ITEM to the end of place (like `push-end`) but only if it not already a member of PLACE (like `pushnew`).

For the use of KEY, TEST, and TEST-NOT, see `pushnew`.

[View source](lists.lisp#L90)

### `(in x &rest items)`

Is X equal to any of ITEMS?

`(in x xs...)` is always equivalent to `(and (member x xs :test equal) t)`,
but `in` can sometimes compile to more efficient code when the
candidate matches are constant.

From Arc.

[View source](lists.lisp#L109)

### `(memq item list)`

Like (member ITEM LIST :test #'eq).
Should only be used for symbols.

[View source](lists.lisp#L137)

### `(delq item list)`

Like (delete ... :test #'eq), but only for lists.

Almost always used as (delq nil ...).

[View source](lists.lisp#L164)

### `(mapply fn list &rest lists)`

`mapply` is a cousin of `mapcar`.

If you think of `mapcar` as using `funcall`:

    (mapcar #'- '(1 2 3))
    ≅ (loop for item in '(1 2 3)
            collect (funcall #'- item))

Then `mapply` does the same thing, but with `apply` instead.

    (loop for item in '((1 2 3) (4 5 6))
            collect (apply #'+ item))
    => (6 15)

    (mapply #'+ '((1 2 3) (4 5 6)))
    => (6 15)

In variadic use, `mapply` acts as if `append` had first been used:

    (mapply #'+ xs ys)
    ≡ (mapply #'+ (mapcar #'append xs ys))

But the actual implementation is more efficient.

`mapply` can convert a list of two-element lists into an alist:

    (mapply #'cons '((x 1) (y 2))
    => '((x . 1) (y . 2))

[View source](lists.lisp#L181)

### `(assocdr item alist &rest args &key &allow-other-keys)`

Like (cdr (assoc ...))

[View source](lists.lisp#L235)

### `(assocar item alist &rest args &key &allow-other-keys)`

Like (car (assoc ...))

[View source](lists.lisp#L240)

### `(assocadr item alist &rest args &key &allow-other-keys)`

Like `assocdr` for alists of proper lists.

     (assocdr 'x '((x 1))) => '(1)
     (assocadr 'x '((x 1))) => 1

[View source](lists.lisp#L245)

### `(rassocar item alist &rest args &key &allow-other-keys)`

Like (car (rassoc ...))

[View source](lists.lisp#L253)

### `(rassocdr item alist &rest args &key &allow-other-keys)`

Like (cdr (rassoc ...))

[View source](lists.lisp#L258)

### `(firstn n list)`

The first N elements of LIST, as a fresh list:

    (firstn 4 (iota 10))
    => (0 1 2 4)

(I do not know why this extremely useful function did not make it
into Common Lisp, unless it was deliberately left out as an exercise
for Maclisp users.)

[View source](lists.lisp#L263)

### `(powerset set)`

Return the powerset of SET.
Uses a non-recursive algorithm.

[View source](lists.lisp#L275)

### `(efface item list)`

Destructively remove only the first occurence of ITEM in LIST.

From Lisp 1.5.

[View source](lists.lisp#L286)

### `(pop-assoc key alist &rest args)`

Like `assoc` but, if there was a match, delete it from ALIST.

From Newlisp.

[View source](lists.lisp#L305)

### `(mapcar-into fn list)`

Like (map-into list fn list).

From PAIP.

[View source](lists.lisp#L321)

### `(nthrest n list)`

Alias for `nthcdr`.

[View source](lists.lisp#L330)

### `(plist-keys plist)`

Return the keys of a plist.

[View source](lists.lisp#L334)

### `(plist-values plist)`

Return the values of a plist.

[View source](lists.lisp#L340)

### `(intersectionp list1 list2 &key key test test-not)`

Return T if LIST1 and LIST2 intersect.
Equivalent to `(and (intersection list1 list2) t)`, without
intermediate consing.

Two empty lists are not considered to intersect.

[View source](lists.lisp#L396)

### `(stable-set-difference list1 list2 &key key test test-not)`

Like `set-difference`, but preserve the order of LIST1's elements.

[View source](lists.lisp#L410)

### `(append-longest &rest lists)`

Like `append`, but without guarantees as to order.
The longest list in LISTS is put last, to maximize structure sharing.

This also ignores nil, so `(append-longest list nil)' will return its
first argument unchanged.

[View source](lists.lisp#L421)

### `(mappend-longest fn &rest lists)`

Like `mappend`, but using `append-longest`.

[View source](lists.lisp#L435)

## Sequences

### `(sequencep x)`

Is X a sequence?

[View source](sequences.lisp#L25)

### `(null-if-empty xs)`

Return nil if XS is empty, XS otherwise.
If XS was empty the second value is nil; otherwise t.

This function also accepts multidimensional arrays. Arrays are
considered empty if their total size (from `array-total-size`) is
zero.

Hash tables are considered empty if their count is 0.

[View source](sequences.lisp#L31)

### `(do-each (var seq &optional return) &body body)`

Iterate over the elements of SEQ, a sequence.
If SEQ is a list, this is equivalent to `dolist`.

[View source](sequences.lisp#L122)

### `(nsubseq seq start &optional end)`

Return a subsequence that may share structure with SEQ.

Note that `nsubseq` gets its aposematic leading `n` not because it is
itself destructive, but because, unlike `subseq`, destructive
operations on the subsequence returned may mutate the original.

`nsubseq` also works with `setf`, with the same behavior as
`replace`.

[View source](sequences.lisp#L349)

### `(filter pred seq &rest args &key count &allow-other-keys)`

Almost, but not quite, an alias for `remove-if-not`.

The difference is the handling of COUNT: for `filter`, COUNT is the
number of items to *keep*, not remove.

     (remove-if-not #'oddp '(1 2 3 4 5) :count 2)
     => '(1 3 5)

     (filter #'oddp '(1 2 3 4 5) :count 2)
     => '(1 3)

[View source](sequences.lisp#L404)

### `(filterf g pred &rest args)`

Modify-macro for FILTER.
The place designed by the first argument is set to the result of
calling FILTER with PRED, the place, and ARGS.

[View source](sequences.lisp#L434)

### `(keep item seq &rest args &key test from-end key count &allow-other-keys)`

Almost, but not quite, an alias for `remove` with `:test-not` instead of `:test`.

The difference is the handling of COUNT. For keep, COUNT is the number of items to keep, not remove.

     (remove 'x '(x y x y x y) :count 2)
     => '(y y x y)

     (keep 'x '(x y x y x y) :count 2)
     => '(x x)

`keep` becomes useful with the KEY argument:

     (keep 'x ((x 1) (y 2) (x 3)) :key #'car)
     => '((x 1) (x 3))

[View source](sequences.lisp#L440)

### `(single seq)`

Is SEQ a sequence of one element?

[View source](sequences.lisp#L476)

### `(only-elt seq)`

Return the only element of SEQ.
If SEQ is empty, or contains more than one element, signal an error.

[View source](sequences.lisp#L485)

### `(partition pred seq &key start end key)`

Partition elements of SEQ into those for which PRED returns true
and false.

Return two values, one with each sequence.

Exactly equivalent to:
     (values (remove-if-not predicate seq) (remove-if predicate seq))
except it visits each element only once.

Note that `partition` is not just `assort` with an up-or-down
predicate. `assort` returns its groupings in the order they occur in
the sequence; `partition` always returns the “true” elements first.

    (assort '(1 2 3) :key #'evenp) => ((1 3) (2))
    (partition #'evenp '(1 2 3)) => (2), (1 3)

[View source](sequences.lisp#L507)

### `(partitions preds seq &key start end key)`

Generalized version of PARTITION.

PREDS is a list of predicates. For each predicate, `partitions`
returns a filtered copy of SEQ. As a second value, it returns an extra
sequence of the items that do not match any predicate.

Items are assigned to the first predicate they match.

[View source](sequences.lisp#L534)

### `(assort seq &key key test start end hash)`

Return SEQ assorted by KEY.

     (assort (iota 10)
             :key (lambda (n) (mod n 3)))
     => '((0 3 6 9) (1 4 7) (2 5 8))

Groups are ordered as encountered. This property means you could, in
principle, use `assort` to implement `remove-duplicates` by taking the
first element of each group:

     (mapcar #'first (assort list))
     ≡ (remove-duplicates list :from-end t)

However, if TEST is ambiguous (a partial order), and an element could
qualify as a member of more than one group, then it is not guaranteed
that it will end up in the leftmost group that it could be a member
of.

    (assort '(1 2 1 2 1 2) :test #'<=)
    => '((1 1) (2 2 1 2))

The default algorithm used by `assort` is, in the worst case, O(n) in
the number of groups. If HASH is specified, then a hash table is used
instead. However TEST must be acceptable as the `:test` argument to
`make-hash-table`.

[View source](sequences.lisp#L566)

### `(runs seq &key start end key test compare-last count)`

Return a list of runs of similar elements in SEQ.
The arguments START, END, and KEY are as for `reduce`.

    (runs '(head tail head head tail))
    => '((head) (tail) (head head) (tail))

By defualt, the function TEST is called with the first element of the
run as its first argument.

    (runs #(10 1 5 10 1) :test #'>)
    => (#(10 1 5) #(10))

COMPARE-LAST changes this behavior to test against the previous
element of the run:

    (runs #(10 1 5 10 1) :test #'> :compare-last t)
    (#(10 1) #(5) #(10))

The COUNT argument limits how many runs are returned.

    (runs '(head tail tail head head tail) :count 2)
    => '((head) (tail tail))

If COUNT is zero, `runs` returns an empty list. Otherwise, since
`runs` always returns a list of subsequences of SEQ, if SEQ is empty,
the return value will be a single-element list having the original
sequence as its only value:

    (runs "") -> '("")
    (runs #())  -> '(#())
    (runs '())  -> '(())

[View source](sequences.lisp#L678)

### `(batches seq n &key start end even)`

Return SEQ in batches of N elements.

    (batches (iota 11) 2)
    => ((0 1) (2 3) (4 5) (6 7) (8 9) (10))

If EVEN is non-nil, then SEQ must be evenly divisible into batches of
size N, with no leftovers.

[View source](sequences.lisp#L741)

### `(frequencies seq &rest hash-table-args &key key &allow-other-keys)`

Return a hash table with the count of each unique item in SEQ.
As a second value, return the length of SEQ.

From Clojure.

[View source](sequences.lisp#L801)

### `(scan fn seq &rest args &key from-end start end initial-value &allow-other-keys)`

Return the partial reductions of SEQ.

Each element of the result sequence is the result of calling `reduce`
on the elements of the sequence up to that point (inclusively).

    (reduce #'+ '(1))       => 1
    (reduce #'+ '(1 2))     => 3
    (reduce #'+ '(1 2 3))   => 6
    (reduce #'+ '(1 2 3 4)) => 10
    (scan   #'+ '(1 2 3 4)) => '(1 3 6 10)

The result of calling `scan` on an empty sequence is always an empty
sequence, however.

    (reduce #'+ '()) => 0
    (scan   #'+ '()) => '()

This is sometimes called a "prefix sum", "cumulative sum", or
"inclusive scan".

From APL.

[View source](sequences.lisp#L826)

### `(nub seq &rest args &key start end key test)`

Remove duplicates from SEQ, starting from the end.
That means, for each duplicate, the first occurrence will be the kept, and subsequent occurrences will be discarded.

TEST defaults to `equal`.

From Haskell.

[View source](sequences.lisp#L875)

### `(gcp seqs &key test)`

The greatest common prefix of SEQS.

If there is no common prefix, return NIL.

[View source](sequences.lisp#L888)

### `(gcs seqs &key test)`

The greatest common suffix of SEQS.

If there is no common suffix, return NIL.

[View source](sequences.lisp#L906)

### `(of-length length)`

Return a predicate that returns T when called on a sequence of
length LENGTH.

    (funcall (of-length 3) '(1 2 3)) => t
    (funcall (of-length 1) '(1 2 3)) => nil

[View source](sequences.lisp#L926)

### `(length> &rest seqs)`

Is each length-designator in SEQS longer than the next?
A length designator may be a sequence or an integer.

[View source](sequences.lisp#L966)

### `(length>= &rest seqs)`

Is each length-designator in SEQS longer or as long as the next?
A length designator may be a sequence or an integer.

[View source](sequences.lisp#L971)

### `(length< &rest seqs)`

Is each length-designator in SEQS shorter than the next?
A length designator may be a sequence or an integer.

[View source](sequences.lisp#L976)

### `(length<= &rest seqs)`

Is each length-designator in SEQS as long or shorter than the next?
A length designator may be a sequence or an integer.

[View source](sequences.lisp#L982)

### `(longer x y)`

Return the longer of X and Y.

If X and Y are of equal length, return X.

If X and Y are lists, this will only traverse the shorter of X and Y.

[View source](sequences.lisp#L988)

### `(shorter x y)`

Return the shorter of X and Y.

[View source](sequences.lisp#L1005)

### `(longest seqs)`

Return the longest seq in SEQS.

If there are lists in SEQS, then the total number of conses traversed
will never exceed n*m, where n is the number of lists in SEQS and m
is the length of the next-to-longest list (unless the longest list is
not unique!).

[View source](sequences.lisp#L1074)

### `(shortest seqs)`

Return the shortest seq in SEQS.

If there are lists in SEQS, then the total number of conses traversed
will never exceed n*m, where n is the number of lists in SEQS and m
is the length of the shortest list.

[View source](sequences.lisp#L1085)

### `(slice seq start &optional end)`

Like `subseq`, but allows negative bounds to specify offsets.
Both START and END accept negative bounds.

     (slice "string" -3 -1) => "in"

A call to `slice` where the first argument is positive and the second argument is negative is equivalent to chaining two calls to `drop`:

    (drop 3 (drop -1 "string")) = "in"
    (slice "string" 3 -1)       = "in"

If the bounds cross in the middle, the result is an empty string:

    (slice "x" 1 -1) => ""

Note that `slice` implicitly clamps bounds, even when they are not negative:

    (slice "x" 0 100) => "x"

Setf of `slice` is like setf of `ldb`: afterwards, the place being set
holds a new sequence which is not EQ to the old.

[View source](sequences.lisp#L1115)

### `(ordering seq &key unordered-to-end from-end test key)`

Given a sequence, return a function that, when called with `sort`,
restores the original order of the sequence.

That is, for any SEQ (without duplicates), it is always true that

     (equal seq (sort (reshuffle seq) (ordering seq)))

FROM-END controls what to do in case of duplicates. If FROM-END is
true, the last occurrence of each item is preserved; otherwise, only
the first occurrence counts.

TEST controls identity; it should be a valid test for a hash table. If
the items cannot be compared that way, you can use KEY to transform
them.

UNORDERED-TO-END controls where to sort items that are not present in
the original ordering. By default they are sorted first but, if
UNORDERED-TO-END is true, they are sorted last. In either case, they
are left in no particular order.

[View source](sequences.lisp#L1162)

### `(take n seq)`

Return, at most, the first N elements of SEQ, as a *new* sequence
of the same type as SEQ.

If N is longer than SEQ, SEQ is simply copied.

If N is negative, then |N| elements are taken (in their original
order) from the end of SEQ.

[View source](sequences.lisp#L1205)

### `(drop n seq)`

Return all but the first N elements of SEQ.
The sequence returned is a new sequence of the same type as SEQ.

If N is greater than the length of SEQ, returns an empty sequence of
the same type.

If N is negative, then |N| elements are dropped from the end of SEQ.

[View source](sequences.lisp#L1224)

### `(take-while pred seq &key from-end)`

Return the prefix of SEQ for which PRED returns true.

    (take-while #'alpha-char-p "Really!?")
    => "Really"

If FROM-END is non-nil, return the suffix instead.

    (take-while (complement #'alpha-char-p) "Really!?" :from-end t)
    => "!?"

If PRED returns true for all elements of SEQ, the result is a sequence
with the same type and contents as SEQ.

[View source](sequences.lisp#L1243)

### `(take-until pred seq &key from-end)`

Like `take-while` with the complement of PRED.

[View source](sequences.lisp#L1279)

### `(drop-while pred seq &key from-end)`

Return the largest possible suffix of SEQ for which PRED returns
false when called on the first element.

    (drop-while #'alpha-char-p "Really!?")
    => "!?"

If FROM-END is non-nil, then drop the longest possible suffix of SEQ
for which PRED returns true when called on the first element.

    (drop-while (complement #'alpha-char-p) "Really!?" :from-end t)
    => "Really"

If PRED returns true for all elements of SEQ, then the result is
always an empty sequence of the same type as SEQ.

[View source](sequences.lisp#L1284)

### `(drop-until pred seq &key from-end)`

Like `drop-while` with the complement of PRED.

[View source](sequences.lisp#L1313)

### `(drop-prefix prefix seq &key test)`

If SEQ starts with PREFIX, remove it.

[View source](sequences.lisp#L1319)

### `(drop-suffix suffix seq &key test)`

If SEQ ends with SUFFIX, remove it.

[View source](sequences.lisp#L1328)

### `(ensure-prefix prefix seq &key test)`

Return a sequence like SEQ, but starting with PREFIX.
If SEQ already starts with PREFIX, return SEQ.

[View source](sequences.lisp#L1337)

### `(ensure-suffix seq suffix &key test)`

Return a sequence like SEQ, but ending with SUFFIX.
If SEQ already ends with SUFFIX, return SEQ.

[View source](sequences.lisp#L1351)

### `(bisect-left vec item pred &key key start end)`

Return the index in VEC to insert ITEM and keep VEC sorted.

If a value equivalent to ITEM already exists in VEC, then the index
returned is to the left of that existing item.

[View source](sequences.lisp#L1364)

### `(bisect-right vec item pred &key key start end)`

Return the index in VEC to insert ITEM and keep VEC sorted.

If a value equivalent to ITEM already exists in VEC, then the index
returned is to the right of that existing item.

[View source](sequences.lisp#L1381)

### `(bestn n seq pred &key key memo)`

Partial sorting.
Equivalent to (take N (sort SEQ PRED)), but much faster, at least
for small values of N.

With MEMO, use a decorate-sort-undecorate transform to ensure KEY is
only ever called once per element.

The name is from Arc.

[View source](sequences.lisp#L1398)

### `(nth-best n seq pred &key key)`

Return the Nth-best element of SEQ under PRED.

Equivalent to

    (elt (sort (copy-seq seq) pred) n)

Or even

    (elt (bestn (1+ n) seq pred) n)

But uses a selection algorithm for better performance than either.

[View source](sequences.lisp#L1446)

### `(nth-best! n seq pred &key key)`

Destructive version of `nth-best`.
Note that this function requires that SEQ be a vector.

[View source](sequences.lisp#L1463)

### `(reshuffle seq &key element-type)`

Like `alexandria:shuffle`, but non-destructive.

Regardless of the type of SEQ, the return value is always a vector.

If ELEMENT-TYPE is provided, this is the element type (modulo
upgrading) of the vector returned.

If ELEMENT-TYPE is not provided, then the element type of the vector
returned is T, if SEQ is not a vector. If SEQ is a vector, then the
element type of the vector returned is the same as the as the element
type of SEQ.

[View source](sequences.lisp#L1502)

### `(sort-new seq pred &key key element-type)`

Return a sorted vector of the elements of SEQ.

You can think of this as a non-destructive version of `sort`, except
that it always returns a vector. (If you're going to copy a sequence
for the express purpose of sorting it, you might as well copy it into
a form that can be sorted efficiently.)

ELEMENT-TYPE is interpreted as for `reshuffle`.

[View source](sequences.lisp#L1524)

### `(stable-sort-new seq pred &key key element-type)`

Like `sort-new`, but sort as if by `stable-sort` instead of `sort`.

[View source](sequences.lisp#L1544)

### `(extrema seq pred &key key start end)`

Like EXTREMUM, but returns both the minimum and the maximum (as two
values).

     (extremum (iota 10) #'>) => 9
     (extrema (iota 10) #'>) => 9, 0

[View source](sequences.lisp#L1551)

### `(halves seq &optional split)`

Return, as two values, the first and second halves of SEQ.
SPLIT designates where to split SEQ; it defaults to half the length,
but can be specified.

If SPLIT is not provided, the length is halved using `ceiling` rather
than `truncate`. This is on the theory that, if SEQ is a
single-element list, it should be returned unchanged.

If SPLIT is negative, then the split is determined by counting |split|
elements from the right (or, equivalently, length+split elements from
the left). Note that providing a negative argument to a list works
similarly to `butlast` (a single traversal).

[View source](sequences.lisp#L1612)

### `(dsu-sort seq fn &key key stable)`

Decorate-sort-undecorate using KEY.
Useful when KEY is an expensive function (e.g. database access).

[View source](sequences.lisp#L1645)

### `(dsu-sort-new seq fn &key key stable)`

Like `dsu-sort`, but returning a new vector.

[View source](sequences.lisp#L1652)

### `(deltas seq &optional fn)`

Return the successive differences in SEQ.

     (deltas '(4 9 -5 1 2))
     => '(4 5 -14 6 1)

Note that the first element of SEQ is also the first element of the
return value.

By default, the delta is the difference, but you can specify another
function as a second argument:

    (deltas '(2 4 2 6) #'/)
    => '(2 2 1/2 3)

From Q.

[View source](sequences.lisp#L1667)

### `(inconsistent-graph-constraints inconsistent-graph)`

The constraints of an `inconsistent-graph` error.
Cf. `toposort`.

[View source](sequences.lisp#L1691)

### `(toposort constraints &key test tie-breaker from-end unordered-to-end)`

Turn CONSTRAINTS into a predicate for use with SORT.

Each constraint should be two-element list, where the first element of
the list should come before the second element of the list.

    (def dem-bones '((toe foot)
                     (foot heel)
                     (heel ankle)
                     (ankle shin)
                     (shin knee)
                     (knee back)
                     (back shoulder)
                     (shoulder neck)
                     (neck head)))
    (sort (reshuffle (mapcar #'car dem-bones))
          (toposort dem-bones))
    => (TOE FOOT HEEL ANKLE SHIN KNEE BACK SHOULDER NECK)

If the graph is inconsistent, signals an error of type
`inconsistent-graph`:

    (toposort '((chicken egg) (egg chicken)))
    => Inconsistent graph: ((CHICKEN EGG) (EGG CHICKEN))

TEST, FROM-END, and UNORDERED-TO-END are passed through to
`ordering`.

[View source](sequences.lisp#L1781)

### `(intersperse new-elt seq)`

Return a sequence like SEQ, but with NEW-ELT inserted between each
element.

[View source](sequences.lisp#L1843)

### `(mvfold fn seq &rest seeds)`

Like `reduce` extended to multiple values.

Calling `mvfold` with one seed is equivalent to `reduce`:

    (mvfold fn xs seed) ≡ (reduce fn xs :initial-value seed)

However, you can also call `mvfold` with multiple seeds:

    (mvfold fn xs seed1 seed2 seed3 ...)

How is this useful? Consider extracting the minimum of a sequence:

    (reduce #'min xs)

Or the maximum:

    (reduce #'max xs)

But both?

    (reduce (lambda (cons item)
              (cons (min (car cons) item)
                    (max (cdr cons) item)))
            xs
            :initial-value (cons (elt xs 0) (elt xs 0)))

You can do this naturally with `mvfold`.

    (mvfold (lambda (min max item)
              (values (min item min)
                      (max item max)))
            xs (elt xs 0) (elt xs 0))

In general `mvfold` provides a functional idiom for “loops with
book-keeping” where we might otherwise have to use recursion or
explicit iteration.

Has a compiler macro that generates efficient code when the number of
SEEDS is fixed at compile time (as it usually is).

[View source](sequences.lisp#L1873)

### `(mvfoldr fn seq &rest seeds)`

Like `(reduce FN SEQ :from-end t)' extended to multiple
values. Cf. `mvfold`.

[View source](sequences.lisp#L1915)

### `(repeat-sequence seq n)`

Return a sequence like SEQ, with the same content, but repeated N times.

    (repeat-sequence "13" 3)
    => "131313"

The length of the sequence returned will always be the length of SEQ
times N.

This means that 0 repetitions results in an empty sequence:

    (repeat-sequence "13" 0)
    => ""

Conversely, N may be greater than the possible length of a sequence,
as long as SEQ is empty.

    (repeat-sequence "" (1+ array-dimension-limit))
    => ""


[View source](sequences.lisp#L1955)

### `(seq= &rest xs)`

Like `equal`, but recursively compare sequences element-by-element.

Two elements X and Y are `seq=` if they are `equal`, or if they are
both sequences of the same length and their elements are all `seq=`.

[View source](sequences.lisp#L2038)

### `(do-splits ((left right &optional not-at-end?) (seq split-fn &key (start 0) end from-end) &optional return) &body body)`

For each run of elements in SEQ that does not satisfy SPLIT-FN, call the body with LEFT bound to the start of the run and RIGHT bound to the end of the run.

If `split-sequence-if` did not exist, you could define a simple version trivially with `do-splits` and `collecting`:

    (defun split-sequence-if (fn seq &key (start 0) end from-end)
      (collecting
        (do-splits ((l r) (seq fn :start start :end end :from-end from-end))
          (collect (subseq seq l r)))))

Providing NOT-AT-END? will bind it as a variable that is T if RIGHT is
not equal to END, and null otherwise. This can be useful when, in
processing a sequence, you want to replace existing delimiters, but do
nothing at the end.

In general `do-splits` will be found useful in situations where you
want to iterate over subsequences in the manner of `split-sequence`,
but don't actually need to realize the sequences.

[View source](sequences.lisp#L2102)

### `(collapse-duplicates seq &key key test)`

Remove adjacent duplicates in SEQ.

Repetitions that are not adjacent are left alone.

    (remove-duplicates '(1 1 2 2 1 1)) => '(1 2)
    (collapse-duplicates  '(1 1 2 2 1 1)) => '(1 2 1)

[View source](sequences.lisp#L2154)

### `(same key-fn seq &key test start end)`

Return true if KEY-FN returns the same value for any/all members of LIST.

[View source](sequences.lisp#L2185)

### `(copy-firstn list n)`

Like COPY-LIST, but copies at most the first N conses of LIST. Handles cyclic
lists gracefully.

[View source](sequences.lisp#L2198)

### `(splice-seq sequence &key new start end)`

Removes a part of SEQUENCE between START and END and replaces it with
contents of NEW (if provided). Does not modify SEQUENCE or NEW, but the result
is allowed to share structure with the original if SEQUENCE is a list.

    (splice-seq '(1 2 3 4 5) :new '(:a :b :c) :start 1 :end 1)
    => (1 :A :B :C 2 3 4 5)

    (splice-seq '(1 2 3 4 5) :new '(:a :b :c) :start 1 :end 4)
    => (1 :A :B :C 5)

Omitting NEW removes elements from SEQUENCE:

    (splice-seq '(1 2 3 4 5) :start 1 :end 3)
    => '(1 4 5)

[View source](sequences.lisp#L2340)

### `(nsplice-seq sequence &key new start end)`

Removes a part of SEQUENCE between START and END and replaces it with
contents of NEW (if provided). SEQUENCE and NEW may be destroyed in the process
and the result is allowed to share structure with the original if SEQUENCE is a
list.

    (nsplice-seq (list 1 2 3 4 5) :new (list :a :b :c) :start 1 :end 1)
    => (1 :A :B :C 2 3 4 5)

    (nsplice-seq (list 1 2 3 4 5) :new (list :a :b :c) :start 1 :end 4)
    => (1 :A :B :C 5)

Omitting NEW removes elements from SEQUENCE:

    (nsplice-seq (list 1 2 3 4 5) :start 1 :end 3)
    => '(1 4 5)

[View source](sequences.lisp#L2367)

### `(splice-seqf g &rest keyword-args)`

Modify macro for SPLICE-SEQ.

[View source](sequences.lisp#L2390)

### `(nsplice-seqf g &rest keyword-args)`

Modify macro for NSPLICE-seq.

[View source](sequences.lisp#L2393)

## Strings

### `(whitespacep char)`

Is CHAR whitespace?

Spaces, tabs, any kind of line break, page breaks, and no-break spaces
are considered whitespace.

[View source](strings.lisp#L18)

### `(trim-whitespace string)`

STRING without whitespace at ends.

[View source](strings.lisp#L26)

### `(ascii-char-p char)`

Is CHAR an ASCII char?

[View source](strings.lisp#L30)

### `(with-string (var &optional stream &key (element-type nil element-type-supplied?)) &body body)`

Bind VAR to the character stream designated by STREAM.

STREAM is resolved like the DESTINATION argument to `format`: it can
be any of t (for `*standard-output*`), nil (for a string stream), a
string with a fill pointer, or a stream to be used directly.

When possible, it is a good idea for functions that build strings to
take a stream to write to, so callers can avoid consing a string just
to write it to a stream. This macro makes it easy to write such
functions.

    (defun format-x (x &key stream)
      (with-string (s stream)
        ...))

[View source](strings.lisp#L48)

### `(blankp seq)`

SEQ is either empty, or consists entirely of characters that
satisfy `whitespacep`.

[View source](strings.lisp#L82)

### `(collapse-whitespace string &key space stream)`

Collapse runs of whitespace in STRING.
Each run of space, newline, and other whitespace characters is
replaced by a single space character (or SPACE, if that is specified).

[View source](strings.lisp#L87)

### `(concat &rest strings)`

Abbreviation for (concatenate 'string ...).

From Emacs Lisp.

[View source](strings.lisp#L121)

### `(mapconcat fun seq separator &key stream end)`

Build a string by mapping FUN over SEQ.
Separate each value with SEPARATOR.

Equivalent to
        (reduce #'concat (intersperse SEP SEQ) :key FUN)
but more efficient.

STREAM can be used to specify a stream to write to. It is resolved
like the first argument to `format`.

From Emacs Lisp.

[View source](strings.lisp#L156)

### `(string-join strings separator &key stream end)`

Join strings in STRINGS, separated by SEPARATOR.

SEPARATOR can be any string designator.

If STREAM is provided, write to STREAM rather than returning a string.

If END is provided, then insert SEPARATOR after the last string, as
well as between strings.

Equivalent to `(mapconcat #'string STRINGS SEPARATOR)'.

[View source](strings.lisp#L180)

### `(string-upcase-initials string)`

Return STRING with the first letter of each word capitalized.
This differs from STRING-CAPITALIZE in that the other characters in
each word are not changed.

     (string-capitalize "an ACRONYM") -> "An Acronym")
     (string-upcase-initials "an ACRONYM") -> "An ACRONYM")

From Emacs Lisp (where it is simply `upcase-initials`).

[View source](strings.lisp#L252)

### `(nstring-upcase-initials string)`

Destructive version of `string-upcase-initials`.

[View source](strings.lisp#L264)

### `(same-case-p string)`

Every character with case in STRING has the same case.
Return `:upper` or `:lower` as appropriate.

[View source](strings.lisp#L284)

### `(nstring-invert-case string)`

Destructive version of `string-invert-case`.

[View source](strings.lisp#L307)

### `(string-invert-case string)`

Invert the case of STRING.
This does the same thing as a case-inverting readtable:
- If the string is uppercase, downcase the string.
- If the string is lowercase, upcase the string.
- If the string is mixed-case, leave it alone.

[View source](strings.lisp#L316)

### `(words string &key start end)`

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

[View source](strings.lisp#L326)

### `(tokens string &key start end)`

Separate STRING into tokens.
Tokens are runs of non-whitespace characters.

    (tokens "\"I'm here,\" Tom said presently.")
    => ("\"I'm" "here,\"" "Tom" "said" "presently.")

Cf. `words`.

[View source](strings.lisp#L355)

### `(word-wrap string &key column stream)`

Return a word-wrapped version of STRING that breaks at COLUMN.

Note that this is not a general-purpose word-wrapping routine like you
would find in a text editor: in particular, any existing whitespace is
removed.

[View source](strings.lisp#L370)

### `(lines string &key eol-style honor-crlf keep-eols count)`

Return a list of the lines in STRING, stripped of any EOL characters
and including the last nonempty line even if it has no EOL characters,
or NIL if STRING is empty or NIL.

If COUNT is provided, only the first COUNT lines are returned.

EOL-STYLE can be one of the following:

- NIL, the default, which means split on #\Newline.
- :CR, which means split on CR, i.e., #\Return.
- :LF, which means split on LF, i.e., #\Linefeed.
- :CRLF, which means split on CRLF, i.e., #\Return followed by
  #\Linefeed.
- :ASCII, which means split on any of CR, LF, and CRLF.
- :UNICODE, which means split on any of the newlines described in
  Section 5.8, "Newline Guidelines", of the Unicode Standard,
  available at http://www.unicode.org/versions/latest/.
  These newlines are CR, LF, CRLF, next line, vertical tab, form feed,
  line separator, and paragraph separator.
- A predicate that accepts one CHARACTER and returns non-NIL if the
  CHARACTER should be split on, NIL otherwise.

:CR, :LF, :CRLF, and :ASCII assume that the Common Lisp implementation
represents CHARACTERs internally as ASCII or one of its supersets
(e.g., extended ASCII), and :UNICODE assumes that it represents them
internally as Unicode (which is also a superset of ASCII).
Additionally, all of the EOL-STYLEs just mentioned assume that #\Newline
is either #\Return or #\Linefeed (which can be reasonably expected).

If HONOR-CRLF is supplied, it overrides EOL-STYLE's interpretation of
CRLF except if EOL-STYLE is NIL or :CRLF, in which case HONOR-CRLF has
no effect.
(The :CRLF, :ASCII and :UNICODE EOL-STYLEs honor CRLF by default; the
rest do not.)

If KEEP-EOLS is non-NIL, LINES does not strip the EOL characters from
the lines.

Note that Common Lisp implementations may convert some or all of CR, LF,
and CRLF to #\Newline when reading from file streams, which causes
LINES to split the contents of files differently across implementations.
:CR, :LF, and :CRLF are suitable only when STRING's lines certainly end
with the corresponding EOL character, but if STRING originates from a
file stream, LINES splits nothing unless the corresponding EOL character
is the same as #\Newline, in which case LINES behaves as if EOL-STYLE
were NIL (and indeed NIL is preferable to :CR, :LF, and :CRLF, though
not to :ASCII and :UNICODE).

:UNICODE and :ASCII are the preferred EOL-STYLEs, the former to be
maximally portable and correct, and the latter when Unicode is inapt.
With either EOL-STYLE, LINES splits the entire contents of files
correctly only when the Common Lisp implementation converts only CR,
only LF, or all of CR, LF, and CRLF, to #\Newline (and when it
converts only CR or only LF, #\Newline must the same as the EOL
character in question).
Again with either EOL-STYLE, LINES splits the lines of files, read with
READ-LINE, correctly only when the implementation converts only LF or
all of CR, LF, and CRLF to #\Newline (which must be #\Linefeed).
(Note the lack of the only-CR case when reading files line by line.)
However, any incorrect behavior with :ASCII and :UNICODE is limited to
LINES returning too many or too few empty lines.
The former -- which is uncorrectable -- can occur when CR and LF are
converted, but not CRLF, and the latter -- which can be corrected by
supplying HONOR-CRLF as NIL -- when CR and CRLF are converted (to
#\Return), but not LF, or when LF and CRLF are converted (to
#\Linefeed), but not CR.

For example, to split lines on LF and CRLF (eschewing the recommended
:ASCII and :UNICODE) when the Common Lisp implementation converts only
LF to #\Newline (which must be #\Linefeed), which is the same
behavior as Rust's std::io::BufRead.lines
(https://doc.rust-lang.org/std/io/trait.BufRead.html#method.lines) and
Go's bufio.ScanLines (https://golang.org/pkg/bufio/#ScanLines):

    #.(ecase #\Newline (#\Linefeed))
    (let ((string (coerce '(#\a #\Return
                            #\b #\Linefeed
                            #\c #\Return #\Linefeed
                            #\d)
                          'string)))
      (serapeum:lines string :eol-style :lf :honor-crlf t))
    => ("a^Mb" "c" "d")
    ;; where ^M is #\Return.

(EOL-STYLE cannot be NIL here because otherwise HONOR-CRLF would have
no effect.)

To split lines in the same way as Python's str.splitlines
(https://docs.python.org/3/library/stdtypes.html#str.splitlines) when
the Common Lisp implementation converts only CR, only LF, or all of CR,
LF, and CRLF, to #\Newline (as previously described), but also keeping
the EOL characters in order to know what they were:

    #.(ecase #\Newline ((#\Return #\Linefeed)))
    ;; Omit file separator from the example because its textual
    ;; representation (^\) can confuse documentation browsers.
    (let ((string (coerce '(#\a #.(code-char #x001D)
                            #\b #.(code-char #x001E)
                            #\c)
                          'string)))
      (serapeum:lines
       string
       :eol-style (lambda (c)
                    (serapeum:in
                     c #\Return #\Linefeed
                     #.(code-char #x000B)   ; #\Vt (vertical tab)
                     #\Page                 ; Form feed
                     #.(code-char #x001C)   ; #\Fs (file separator)
                     #.(code-char #x001D)   ; #\Gs (group separator)
                     #.(code-char #x001E)   ; #\Rs (record separator)
                     #.(code-char #x0085)   ; Next line
                     #.(code-char #x2028)   ; #\Line_Separator
                     #.(code-char #x2029))) ; #\Paragraph_Separator
       :honor-crlf t
       :keep-eols t))
    => ("a^]" "b^^" "c")
    ;; where ^] is group separator and ^^ is record separator.

To omit empty lines (thus uniformizing LINES's behavior across Common
Lisp implementations):

    #.(ecase #\Newline ((#\Return #\Linefeed)))
    (let ((string (coerce '(#\a #\b #\c
                            #\Return #\Return #\Linefeed #\Linefeed
                            #\z)
                          'string)))
      (delete-if #'uiop:emptyp (serapeum:lines string :eol-style :unicode)))
    => ("abc" "z")

To additionally omit lines consisting only of whitespace:

    #.(ecase #\Newline ((#\Return #\Linefeed)))
    (let ((string (coerce '(#\a #\b #\c
                            #\Return #\Return #\Linefeed #\Linefeed
                            #\Space #\Linefeed
                            #\Tab #\Linefeed
                            #\z)
                          'string)))
      (delete-if #'uiop:emptyp
                 (mapcar #'serapeum:trim-whitespace
                         (serapeum:lines string :eol-style :unicode))))
    => ("abc" "z")

[View source](strings.lisp#L405)

### `(fmt control-string &rest args)`

A cousin of `format` expressly for fast formatting of strings.

Like (format nil ...), binding `*print-pretty*` to `nil`, which in
some Lisps means a significant increase in speed.

Has a compiler macro with `formatter`.

[View source](strings.lisp#L617)

### `(escape string table &key start end stream)`

Write STRING to STREAM, escaping with TABLE.

TABLE should be either a hash table, with characters for keys and
strings for values, or a function that takes a character and
returns (only) either a string or null.

That is, the signature of TABLE should be:

    (function (character) (or string null))

where `nil` means to pass the character through unchanged.

STREAM can be used to specify a stream to write to, like the first
argument to `format`. The default behavior, with no stream specified,
is to return a string.

[View source](strings.lisp#L707)

### `(ellipsize string n &key ellipsis)`

If STRING is longer than N, truncate it and append ELLIPSIS.

Note that the resulting string is longer than N by the length of
ELLIPSIS, so if N is very small the string may come out longer than it
started.

     (ellipsize "abc" 2)
     => "ab..."

From Arc.

[View source](strings.lisp#L736)

### `(string^= prefix string &key start1 end1 start2 end2)`

Is PREFIX a prefix of STRING?

[View source](strings.lisp#L780)

### `(string-prefix-p prefix string &key start1 end1 start2 end2)`

Like `string^=`, but case-insensitive.

[View source](strings.lisp#L780)

### `(string$= suffix string &key start1 end1 start2 end2)`

Is SUFFIX a suffix of STRING?

[View source](strings.lisp#L800)

### `(string-suffix-p suffix string &key start1 end1 start2 end2)`

Like `string$=`, but case-insensitive.

[View source](strings.lisp#L800)

### `(string*= substring string &key start1 end1 start2 end2)`

Is SUBSTRING a substring of STRING?

This is similar, but not identical, to SEARCH.

     (search nil "foo") => 0
     (search "nil" "nil") => 0
     (string*= nil "foo") => NIL
     (string*= nil "nil") => T

[View source](strings.lisp#L820)

### `(string-contains-p substring string &key start1 end1 start2 end2)`

Like `string*=`, but case-insensitive.

[View source](strings.lisp#L820)

### `(string~= token string &key start1 end1 start2 end2)`

Does TOKEN occur in STRING as a token?

Equivalent to
     (find TOKEN (tokens STRING) :test #'string=),
but without consing.

[View source](strings.lisp#L842)

### `(string-token-p token string &key start1 end1 start2 end2)`

Like `string~=`, but case-insensitive.

[View source](strings.lisp#L842)

### `(string-replace-all old string new &key start end stream count)`

Do search-and-replace for constant strings.

Note that START and END only affect where the replacements are made:
the part of the string before START, and the part after END, are
always included verbatim.

     (string-replace-all "old" "The old old way" "new"
                         :start 3 :end 6)
     => "The new old way"

COUNT can be used to limit the maximum number of occurrences to
replace. If COUNT is not specified, every occurrence of OLD between
START and END is replaced with NEW.

    (string-replace-all "foo" "foo foo foo" "quux")
    => "quux quux quux"

    (string-replace-all "foo" "foo foo foo" "quux" :count 2)
    => "quux quux foo"

STREAM can be used to specify a stream to write to. It is resolved
like the first argument to `format`.

[View source](strings.lisp#L876)

### `(string-replace old string new &key start end stream)`

Like `string-replace-all`, but only replace the first match.

[View source](strings.lisp#L930)

### `(chomp string &optional suffixes)`

If STRING ends in one of SUFFIXES, remove that suffix.

SUFFIXES defaults to a Lisp newline, a literal line feed, a literal
carriage return, or a literal carriage return followed by a literal
line feed.

Takes care that the longest suffix is always removed first.

[View source](strings.lisp#L939)

### `(string-count substring string &key start end)`

Count how many times SUBSTRING appears in STRING.

[View source](strings.lisp#L968)

### `(string+ &rest args)`

Optimized function for building small strings.

Roughly equivalent to

    (let ((*print-pretty* nil))
     (format nil "~@{~a}" args...))

But may be more efficient when the arguments of certain simple
types (such as strings, characters, symbols, pathnames, and fixnums).

Note that unlike `princ`, `string+` treats `nil` as the same as the
empty string:

    (string+ nil)
    => ""

    (string+ "x" nil)
    => "x"

This utility is inspired by the utility of the same name in Allegro.

[View source](strings.lisp#L1005)

## Vectors

### `(ensure-vector x)`

If X is a vector, return it.
Otherwise, return a vector with X as its sole element.

[View source](vectors.lisp#L3)

### `(vect &rest initial-contents)`

Succinct constructor for adjustable vectors with fill pointers.

    (vect 1 2 3)
    ≡ (make-array 3
            :adjustable t
            :fill-pointer 3
            :initial-contents (list 1 2 3))

The fill pointer is placed after the last element in INITIAL-CONTENTS.

As a constructor this also has a matching definition as a Trivia
pattern for destructing.

[View source](vectors.lisp#L23)

### `(values-vector vec)`

Return the elements of VEC, a vector, as multiple values.
This is to vectors what `values-list` is to lists.

[View source](vectors.lisp#L54)

### `(pad-start vec length &optional pad)`

Pad VEC, a vector, to LENGTH, using PAD.
If VEC is already the same length, or longer, than LENGTH, return VEC
unchanged.

    (pad-start "abc" 3)
    => "abc"

If PAD is a sequence, then it is repeated before VEC to make up LENGTH.

    (pad-start "abc" 9 "def")
    => "defdefabc"

If PAD is not a sequence, it is used to fill the remainder of VEC.

    (pad-start "abc" 6 #x)
    => "xxxabc"

PAD defaults to the space character.

This function is most useful for strings, but it can be used with any
vector. Note that the vector returned has the same element type as
VEC, so PAD must satisfy that element type.

Loosely inspired by ECMA.

[View source](vectors.lisp#L110)

### `(pad-end vec length &optional pad)`

Pad VEC, a vector, to LENGTH, using PAD.
Like `pad-start`, but padding is addded to the end, rather than the
beginning.

[View source](vectors.lisp#L165)

### `(vector-conc-extend vector new-elements &optional extension)`

Add NEW-ELEMENTS to the end of VECTOR, an adjustable array with a fill-pointer.
This is the practical equivalent to calling `vector-push-extend` on
each element on NEW-ELEMENTS, but should be faster.

Returns VECTOR.

[View source](vectors.lisp#L190)

## Vector=

### `(vector= vec1 vec2 &key test start1 start2 end1 end2)`

Like `string=` for any vector.
If no TEST is supplied, elements are tested with `eql`.

[View source](vector=.lisp#L161)

## Internal Definitions

### `(local* &body body)`

Like `local`, but leave the last form in BODY intact.

     (local*
       (defun aux-fn ...)
       (defun entry-point ...))
     =>
     (labels ((aux-fn ...))
       (defun entry-point ...))

[View source](internal-definitions.lisp#L12)

### `(local &body orig-body)`

Make internal definitions using top-level definition forms.

Within `local` you can use top-level definition forms and have them
create purely local definitions, like `let`, `labels`, and `macrolet`:

     (fboundp 'plus) ; => nil

     (local
       (defun plus (x y)
         (+ x y))
       (plus 2 2))
     ;; => 4

     (fboundp 'plus) ; => nil

Each form in BODY is subjected to partial expansion (with
`macroexpand-1`) until either it expands into a recognized definition
form (like `defun`) or it can be expanded no further.

(This means that you can use macros that expand into top-level
definition forms to create local definitions.)

Just as at the real top level, a form that expands into `progn` (or an
equivalent `eval-when`) is descended into, and definitions that occur
within it are treated as top-level definitions.

(Support for `eval-when` is incomplete: `eval-when` is supported only
when it is equivalent to `progn`).

The recognized definition forms are:

- `def`, for lexical variables (as with `letrec`)
- `define-values`, for multiple lexical variables at once
- `defun`, for local functions (as with `labels`)
- `defalias`, to bind values in the function namespace (like `fbindrec*`)
- `declaim`, to make declarations (as with `declare`)
- `defconstant` and `defconst`, which behave exactly like symbol macros
- `define-symbol-macro`, to bind symbol macros (as with `symbol-macrolet`)

Also, with serious restrictions, you can use:

- `defmacro`, for local macros (as with `macrolet`)

(Note that the top-level definition forms defined by Common Lisp
are (necessarily) supplemented by three from Serapeum: `def`,
`define-values`, and `defalias`.)

The exact order in which the bindings are made depends on how `local`
is implemented at the time you read this. The only guarantees are that
variables are bound sequentially; functions can always close over the
bindings of variables, and over other functions; and macros can be
used once they are defined.

     (local
       (def x 1)
       (def y (1+ x))
       y)
     => 2

     (local
       (defun adder (y)
         (+ x y))
       (def x 2)
       (adder 1))
     => 3

Perhaps surprisingly, `let` forms (as well as `let*` and
`multiple-value-bind`) *are* descended into; the only difference is
that `defun` is implicitly translated into `defalias`. This means you
can use the top-level idiom of wrapping `let` around `defun`.

    (local
      (let ((x 2))
        (defun adder (y)
          (+ x y)))
      (adder 2))
    => 4

Support for macros is sharply limited. (Symbol macros, on the other
hand, are completely supported.)

1. Macros defined with `defmacro` must precede all other expressions.

2. Macros cannot be defined inside of binding forms like `let`.

3. `macrolet` is not allowed at the top level of a `local` form.

These restrictions are undesirable, but well justified: it is
impossible to handle the general case both correctly and portably, and
while some special cases could be provided for, the cost in complexity
of implementation and maintenance would be prohibitive.

The value returned by the `local` form is that of the last form in
BODY. Note that definitions have return values in `local` just like
they do at the top level. For example:

     (local
       (plus 2 2)
       (defun plus (x y)
         (+ x y)))

Returns `plus`, not 4.

The `local` macro is loosely based on Racket's support for internal
definitions.

[View source](internal-definitions.lisp#L687)

### `(block-compile (&key entry-points (block-compile t)) &body body)`

Shorthand for block compilation with `local*`.

Only the functions in ENTRY-POINTS will have global definitions. All
other functions in BODY will be compiled as purely local functions,
and all of their calls to one another will be compiled as local calls.
This includes calls to the entry points, and even self-calls from
within the entry points.

Note that `declaim` forms occuring inside of BODY will be translated
into local `declare` forms.

If you pass `:block-compile nil', this macro is equivalent to progn.
This may be useful during development.

[View source](internal-definitions.lisp#L801)

## Tree Case

### `(tree-case keyform &body cases)`

A variant of `case` optimized for when every key is an integer.

Comparison is done using `eql`.

[View source](tree-case.lisp#L8)

### `(tree-ecase keyform &body clauses)`

Like `tree-case`, but signals an error if KEYFORM does not match
any of the provided cases.

[View source](tree-case.lisp#L41)

### `(char-case keyform &body clauses)`

Like `case`, but specifically for characters.
Expands into `tree-case`.

As an extension to the generalized `case` syntax, the keys of a clause
can be specified as a literal string.

    (defun vowel? (c)
      (char-case c
        ("aeiouy" t)))

Signals an error if KEYFORM does not evaluate to a character.

[View source](tree-case.lisp#L54)

### `(char-ecase keyform &body clauses)`

Like `ecase`, but specifically for characters.
Expands into `tree-case`.

[View source](tree-case.lisp#L69)

## Dispatch Case

### `(dispatch-case-error &key type datum)`

NO DOCS!

[View source](dispatch-case.lisp#L30)

### `(dispatch-case (&rest exprs-and-types) &body clauses)`

Dispatch on the types of multiple expressions, exhaustively.

Say you are working on a project where you need to handle timestamps
represented both as universal times, and as instances of
`local-time:timestamp`. You start by defining the appropriate types:

    (defpackage :dispatch-case-example
      (:use :cl :alexandria :serapeum :local-time)
      (:shadow :time))
    (in-package :dispatch-case-example)

    (deftype universal-time ()
      '(integer 0 *))

    (deftype time ()
      '(or universal-time timestamp))

Now you want to write a `time=` function that works on universal
times, timestamps, and any combination thereof.

You can do this using `etypecase-of`:

    (defun time= (t1 t2)
      (etypecase-of time t1
        (universal-time
         (etypecase-of time t2
           (universal-time
            (= t1 t2))
           (timestamp
            (= t1 (timestamp-to-universal t2)))))
        (timestamp
         (etypecase-of time t2
           (universal-time
            (time= t2 t1))
           (timestamp
            (timestamp= t1 t2))))))

This has the advantage of efficiency and exhaustiveness checking, but
the serious disadvantage of being hard to read: to understand what
each branch matches, you have to backtrack to the enclosing branch.
This is bad enough when the nesting is only two layers deep.

Alternately, you could do it with `defgeneric`:

    (defgeneric time= (t1 t2)
      (:method ((t1 integer) (t2 integer))
        (= t1 t2))
      (:method ((t1 timestamp) (t2 timestamp))
        (timestamp= t1 t2))
      (:method ((t1 integer) (t2 timestamp))
        (= t1 (timestamp-to-universal t2)))
      (:method ((t1 timestamp) (t2 integer))
        (time= t2 t1)))

This is easy to read, but it has three potential disadvantages. (1)
There is no exhaustiveness checking. If, at some point in the future,
you want to add another representation of time to your project, the
compiler will not warn you if you forget to update `time=`. (This is
bad enough with only two objects to dispatch on, but with three or
more it gets rapidly easier to miss a case.) (2) You cannot use the
`universal-time` type you just defined; it is a type, not a class, so
you cannot specialize methods on it. (3) You are paying a run-time
price for extensibility -- the inherent overhead of a generic function
-- when extensibility is not what you want.

Using `dispatch-case` instead gives you the readability of
`defgeneric` with the efficiency and safety of `etypecase-of`.

    (defun time= (t1 t2)
      (dispatch-case ((t1 time)
                      (t2 time))
        ((universal-time universal-time)
         (= t1 t2))
        ((timestamp timestamp)
         (timestamp= t1 t2))
        ((universal-time timestamp)
         (= t1 (timestamp-to-universal t2)))
        ((timestamp universal-time)
         (time= t2 t1))))

The syntax of `dispatch-case` is much closer to `defgeneric` than it
is to `etypecase`. The order in which clauses are defined does not
matter, and you can define fallthrough clauses in the same way you
would define fallthrough methods in `defgeneric`.

Suppose you wanted to write a `time=` function like the one above, but
always convert times to timestamps before comparing them. You could
write that using `dispatch-case` like so:

    (defun time= (x y)
      (dispatch-case ((x time)
                      (y time))
        ((* universal-time)
         (time= x (universal-to-timestamp y)))
        ((universal-time *)
         (time= (universal-to-timestamp x) y))
        ((timestamp timestamp)
         (timestamp= x y))))

(In the list of types, you can use as asterisk as a shorthand for the
type of the corresponding argument to `dispatch-case`; in that above,
`time`.)

Note that this requires only three clauses, where writing it out using
nested `etypecase-of` forms would require four clauses. This is a
small gain; but with more subtypes to dispatch on, or more objects,
such fallthrough clauses become more useful.

[View source](dispatch-case.lisp#L128)

### `(dispatch-caseql (&rest exprs-and-types) &body clauses)`

Like `dispatch-case`, but types in clauses are implicitly wrapped in `eql`.
The syntax of `dispatch-caseql` is tohus closer to `case` than to
`typecase`.

[View source](dispatch-case.lisp#L251)

### `(dispatch-case-let (&rest bindings) &body clauses)`

Like `dispatch-case`, but establish new bindings for each expression.

For example,

    (dispatch-case-let (((x string) (expr1))
                        ((y string) (expr2)))
      ...)

is equivalent to

    (let ((x (expr1))
          (y (expr2)))
      (dispatch-case ((x string)
                      (y string))
        ...))

It may be helpful to think of this as a cross between
`defmethod` (where the (variable type) notation is used in the lambda
list) and `let` (which has an obvious macro-expansion in terms of
`lambda`).

[View source](dispatch-case.lisp#L259)

### `(dispatch-caseql-let (&rest bindings) &body clauses)`

Like `dispatch-case-let`, but using the clause syntax of `dispatch-caseql`.

[View source](dispatch-case.lisp#L316)

## Range

A possibly over-engineered `range` function. Why is it worth all the
fuss? It's used extensively in Serapeum's test suite. The faster
`range` runs, and the less pressure it puts on the garbage collector,
the faster the test suite runs.

[range]: https://docs.python.org/2/library/functions.html#range


### `(range start &optional stop step)`

Return a (possibly specialized) vector of real numbers, starting from START.

With three arguments, return the integers in the interval [start,end)
whose difference from START is divisible by STEP.

START, STOP, and STEP can be any real number, except that if STOP is
greater than START, STEP must be positive, and if START is greater
than STOP, STEP must be negative.

The vector returned has the smallest element type that can represent
numbers in the given range. E.g. the range [0,256) will usually be
represented by a vector of octets, while the range [-10.0,10.0) will
be represented by a vector of single floats. The exact representation,
however, depends on your Lisp implementation.

STEP defaults to 1.

With two arguments, return all the steps in the interval [start,end).

With one argument, return all the steps in the interval [0,end).

[View source](range.lisp#L215)

## Generalized Arrays

Some operations on “generalized arrays.”

Functions on generalized arrays are total: they work on arrays, of course, but also on sequences (which are treated as one-dimensional arrays) and atoms (which are treated as zero-dimensional arrays).

### A note for array programmers

The semantics of generalized arrays in Serapeum is based on the “array theory” formalism of Trenchard More, as implemented in [Nial][]. Note that this is different from the MOA (“Mathematics of Arrays”) formalism on which direct descendants of APL, such as J, are based.

Nial programmers might be surprised that we rely on the v4, rather than the v6, version of array theory. This is because, in Common Lisps, it is possible to have empty arrays of different element types, and such arrays are not considered equivalent.

[Nial]: https://en.wikipedia.org/wiki/Nial


### `(shape array)`

Return the shape of ARRAY, a generalized array.
For a true array this is equivalent to `array-dimensions`.

[View source](generalized-arrays.lisp#L24)

### `(reshape shape array &key element-type displace)`

Return an array that has the same items as ARRAY, a generalized
array, but whose shape is SHAPE.

If the resulting array is smaller than ARRAY, then discard the excess
items.

If the resulting array is larger than ARRAY, fill it with the items of
ARRAY cyclically.

ELEMENT-TYPE specifies an element type to use for the resulting array
if one cannot be inferred from the array itself.

[View source](generalized-arrays.lisp#L103)

### `(ravel array &key displace)`

Return the items of ARRAY as a sequence.

Array theory calls this operation `list`, but the MOA operation is
identical and has a more distinctive name.

[View source](generalized-arrays.lisp#L181)

### `(prod array)`

Return the product of all of the elements of ARRAY, a generalized array.
Operates pairwise for numerical stability.

[View source](generalized-arrays.lisp#L331)

## Units

### `(si-prefix n &key base)`

Given a number, return the prefix of the nearest SI unit.

Three values are returned: the long form, the short form, and the
multiplying factor.

    (si-prefix 1001) => "kilo", "k", 1000d0

BASE can be 1000, 10, 1024, or 2. 1000 is the default, and prefixes
start at kilo and milli. Base 10 is mostly the same, except the
prefixes centi, deci, deca and hecto are also used. Base 1024 uses the
same prefixes as 1000, but with 1024 as the base, as in vulgar file
sizes. Base 2 uses the IEC binary prefixes.

[View source](units.lisp#L59)

### `(human-size-formatter size &key flavor space)`

Auxiliary function for formatting quantities human-readably.
Returns two values: a format control and a list of arguments.

This can be used to integrate the human-readable printing of
quantities into larger format control strings using the recursive
processing format directive (~?):

    (multiple-value-bind (control args)
        (human-size-formatter size)
      (format t "~?" control args))

[View source](units.lisp#L80)

### `(format-human-size stream size &key flavor space)`

Write SIZE to STREAM, in human-readable form.

STREAM is interpreted as by `format`.

If FLAVOR is `:si` (the default) the base is 1000 and SI prefixes are used.

If FLAVOR is `:file`, the base is 1024 and SI prefixes are used.

If FLAVOR is `:iec`, the base is 1024 bytes and IEC prefixes (Ki, Mi,
etc.) are used.

If SPACE is non-nil, include a space between the number and the
prefix. (Defaults to T if FLAVOR is `:si`.)

[View source](units.lisp#L112)

## Exporting

### `(defclass name supers &body (slots . options))`

Like `defclass`, but implicitly export the name of the class and
the names of all accessors (including readers and writers).

You can specify `:export nil' in the definition of a slot to prevent
its accessors from being exported.

[View source](exporting.lisp#L12)

### `(define-values values &body (expr))`

Like `define-values`, with implicit export of VALUES.

[View source](exporting.lisp#L38)

### `(def var &body (&optional val docs))`

Like `def`, with implicit export of VAR.

[View source](exporting.lisp#L63)

### `(define-symbol-macro symbol expansion)`

Like `define-symbol-macro`, with implicit export of SYMBOL.

[View source](exporting.lisp#L65)

### `(deftype name lamda-list &body body)`

Like `deftype`, with implicit export of NAME.

[View source](exporting.lisp#L67)

### `(defconst symbol init &optional docstring)`

Like `defconst`, with implicit export of SYMBOL.

[View source](exporting.lisp#L69)

### `(defconstant name value &optional doc)`

Like `defconstant`, with implicit export of NAME.

[View source](exporting.lisp#L71)

### `(defvar var &optional val doc)`

Like `defvar`, with implicit export of VAR.

[View source](exporting.lisp#L73)

### `(defparameter var val &optional doc)`

Like `defparameter`, with implicit export of VAR.

[View source](exporting.lisp#L75)

### `(defun name lambda-list &body body)`

Like `defun`, with implicit export of NAME.

[View source](exporting.lisp#L77)

### `(defalias name &body body)`

Like `defalias`, with implicit export of NAME.

[View source](exporting.lisp#L79)

### `(defmacro name &body body)`

Like `defmacro`, with implicit export of NAME.

[View source](exporting.lisp#L81)

### `(defgeneric name lambda-list &body options)`

Like `defgeneric`, with implicit export of NAME.

[View source](exporting.lisp#L83)

### `(defmethod name &body args)`

Like `defmethod`, with implicit export of NAME.

[View source](exporting.lisp#L85)

## Docs

### `(render-function-reference-as-markdown package-names system-name &key stream)`

Renders API reference for given `PACKAGE-NAMES` of system named `SYSTEM-NAME`.

`STREAM` argument can be nil, stream, string or a pathname.

If it is a stream, then output will be written to that stream.

If it is nil, then the functio will return output as a string.
In case of string or a pathname, output will be rendered into the
file with that name, superseding it if it is already exists.

[View source](docs.lisp#L87)

### `(update-function-reference filename system &optional packages)`

A short hand for calling `RENDER-FUNCTION-REFERENCE-AS-MARKDOWN`.

It accepts a short `FILENAME` and the result will be written to the `SYSTEM``s folder.

Also, you can omit `PACKAGES` if your system provides only one package with the
same name.

Example usage:

    (ql:quickload :serapeum/docs)
    (serapeum.docs:update-function-reference
        "REFERENCE.md"
        :my-system)



[View source](docs.lisp#L158)

