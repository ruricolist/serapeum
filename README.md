<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Overview](#overview)
- [Installation](#installation)
- [Commentary](#commentary)
  - [A note about strings](#a-note-about-strings)
  - [Dividing sequences](#dividing-sequences)
  - [Binding values in the function namespace](#binding-values-in-the-function-namespace)
  - [Internal definitions](#internal-definitions)
    - [Example: macros that work locally and globally](#example-macros-that-work-locally-and-globally)
    - [Example: block compiling](#example-block-compiling)
  - [Compile-time exhaustiveness checking](#compile-time-exhaustiveness-checking)
    - [Example: enums](#example-enums)
    - [Example: union types](#example-union-types)
  - [CLOS](#clos)
    - [Method combination: standard with context](#method-combination-standard-with-context)
    - [Metaclass: topmost-object-class](#metaclass-topmost-object-class)
- [Function reference](#function-reference)

<!-- markdown-toc end -->

# Overview

[![CI](https://github.com/ruricolist/serapeum/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/ruricolist/serapeum/actions/workflows/ci.yml)

Serapeum is a conservative library of Common Lisp utilities. It is a
supplement, not a competitor, to [Alexandria][]. That means it is safe to
do:

    (defpackage ... (:use #:cl #:alexandria #:serapeum),

without package conflicts.

Alternatively, now that package-local nicknames are generally
available, the `serapeum/bundle` package reexports the symbols of both
(along with other utilities used by Serapeum itself):

    (defpackage ... (:local-nicknames (:util :serapeum/bundle)))

There may already be too many utility libraries for Common Lisp.
Releasing another has become something to apologize for, not
celebrate. But I would rather make my apologies than have to maintain
copy-pasted versions of the same utilities across a dozen systems.
And, though Serapeum is justified even if only I ever use it, the best
way to ensure its quality is to write it as if for general use.

Serapeum is conservative: its goal is to fill in gaps in Common Lisp,
not to redesign it. But it is less conservative than Alexandria.
Alexandria limits itself to utilities with a Common Lisp pedigree.
Serapeum casts a wider net: other dialects of Lisp, and other
languages in the functional and array families, have been drafted.

Alexandria is self-contained. It exists in splendid isolation, without
depending on, or even acknowledging, other libraries. Serapeum tries
to be a good citizen of the Quicklisp era: whenever possible, it
avoids duplicating functionality that can be had elsewhere.

Many of the utilities in Serapeum are original ideas; many ideas are
borrowed from other languages, or from other Lispers. I try to give
credit in the docstrings, but sometimes I have forgotten where I got
an idea or a name. I regard missing credits as bugs: please report
them.

Serapeum is intended to be portable, but it is principally tested
where it is developed, on [SBCL][] and [Clozure CL][]. Patches and bug
reports for other Lisps are always welcome, however, including
implementation-specific optimizations.

# Installation

The preferred way to install Serapeum by using [Quicklisp][]:

    (ql:quickload "serapeum")

If you want the very latest version of Serapeum, you can check out the
repository into your `~/quicklisp/local-projects` directory.

# Serapeum modules

Serapeum is being carefully refactored into modular subsystems. Using
Serapeum as a whole is still recommended. Modularization will make
Serapeum easier to maintain, easier to incrementally adopt, and more discoverable.

You can load Serapeum modules like any other systems:

    (ql:quickload "serapeum/types")

The following modules are currently available:

- [serapeum/binding](REFERENCE.md#binding) - Binding macros.
- [serapeum/conditions](REFERENCE.md#conditions) - Condition handling utilities.
- [serapeum/control-flow](REFERENCE.md#control-flow) - Control flow macros.
- [serapeum/defining-types](REFERENCE.md#defining-types) - Macros for defining types.
- [serapeum/definitions](REFERENCE.md#definitions) - Global definition macros.
- [serapeum/iter](REFERENCE.md#iter) - Iteration constructs and utilities.
- [serapeum/macro-tools](REFERENCE.md#macro-tools) - Tools for writing macros.
- [serapeum/op](REFERENCE.md#op) - The `op’ macro for succint lambdas.
- [serapeum/portability](REFERENCE.md#portability) – “Subtrivial” portability.
- [serapeum/types](REFERENCE.md#types) - Utility types and type utilities.
- [serapeum/box](REFERENCE.md#box) - Box data structure.

# Commentary

One goal of Serapeum is to have excellent documentation. A utility
library is a fork of its language; it deserves documentation of the
same quality as a language reference. If a utility is not worth
documenting, it is not worth having.

The full function reference will be found [here](REFERENCE.md). (It is
in a separate file in deference to documentation browsers, which often
print the README as a preamble to their own function reference).

Most utilities in Serapeum stand alone, but there are some families
that deserve separate introduction.

- [Dividing sequences](#dividing-sequences)
- [Binding values in the function namespace](#binding-values-in-the-function-namespace)
- [Internal definitions](#internal-definitions) and [block compilation](#block-compiling)
- [Compile-time exhaustiveness checking](#compile-time-exhaustiveness-checking)

## A note about strings

Beginning Lispers often ask about Lisp: “Where are the string
utilities?” Since strings in Lisp are a kind of vector, which is in
turn a kind of sequence, the right place to look is under the
documentation for sequences (and vectors).

This is also true for Serapeum: there are many [sequence utilities](REFERENCE.md#sequences),
and a few [vector utilities](REFERENCE.md#vectors), that work equally well on strings.
But there are also many [string-specific utilities](REFERENCE.md#strings).

## Dividing sequences

All recent functional programming languages share a family of useful
sequence-related functions with terrible names. All of them are called
something like “split”, “divide”, or “group”, more or less at random.

For each function, we ensure:

- It is efficient.
- It returns like sequences for like (lists for lists, strings for
  strings, &c.).
- It accommodates generic sequences (`list` and `vector` are not
  necessarily an exhaustive partition of `sequence`).
- It has a distinctive name which does not use any of the weasel words
  “split,” “divide,” or “group.”

The function that returns *runs* of like elements in a sequence is
called `runs`:

    (runs '(head tail head head tail))
    => '((head) (tail) (head head) (tail))

The function that returns a sequence in *batches* of a certain maximum size is
called `batches`:

    (batches (iota 11) 2)
    => ((0 1) (2 3) (4 5) (6 7) (8 9) (10))

The function which groups the like elements of a sequence is called
`assort` (because it returns a sequence *assorted by* some property).

    (assort (iota 10)
            :key (lambda (n) (mod n 3)))
    => '((0 3 6 9) (1 4 7) (2 5 8))

The function that takes a predicate and a sequence, and returns two
sequences – one sequence of the elements for which the function
returns true, and one sequence of the elements for which it returns
false – is (still) called `partition`.

    (partition #'oddp (iota 10))
    => (1 3 5 7 9), (0 2 4 6 8)

The generalized version of `partition`, which takes a number of
functions and returns the items that satisfy each condition, is called
`partitions`.

    (partitions (list #'primep #'evenp) (iota 10))
    => ((2 3 5 7) (0 4 6 8)), (1 9)

Items that do not belong in any partition are returned as a second value.

Serapeum simply re-exports `split-sequence`, which seems to be firmly
rooted under its present name.

## Binding values in the function namespace

`fbind`, `fbind*`, `fbindrec`, and `fbindrec*` bind values in the
function namespace.

`fbind` and `fbindrec` are like `flet` and `labels`, respectively.

    (fbind ((fn (lambda ....))) ...)
    ≡ (flet ((fn ...)) ...)

    (fbindrec ((fn (lambda ...))) ...)
    ≡ (labels ((fn ...)) ...)

`fbind*` and `fbindrec*` have no exact parallels: they bind functions
in sequence, so that each can be used in the construction (not just
the definition, as with `fbindrec`) of the next.

    (fbind* ((flip2 (lambda (fn)
                     (lambda (x y)
                       (funcall fn y x))))
             (xcons (flip2 #'cons)))
      (xcons 2 1))
    => (1 . 2)

These are non-trivial implementations. In many cases, `fbind` can
produce code that is more efficient than using `funcall`, and even
eliminate the overhead of higher-order functions like `compose` and
`curry`. And `fbindrec`, which builds on `fbind`, further implements
the optimizing transformation from Waddell et. al., *Fixing Letrec*. (Note that the macroexpansion of `fbind` may sometimes appear simplistic, using `macroexpand`; this is the happy case when we can prove that the function is never used as a value.)

For binding values in the function namespace at the top level,
Serapeum provides `defalias`:

    (defalias xcons (flip #'cons))

This is equivalent to `(setf (fdefinition ...))`, but also gives the
function a compile-time definition so compilers don’t complain about
its being undefined.

## Internal definitions

The `local` form lets you use top-level definition forms to create local
bindings. You can use `defun` instead of `labels`, `defmacro` instead of
`macrolet`, `def` (which is Serapeum’s macro for top-level lexical
bindings) instead of `let`, and so forth.

This has three advantages:

1. Given a set of variable, function, and macro bindings, you can
   leave it to the compiler to figure out how to nest them. (This
   could be because you are porting a function from a language that
   uses flat bindings, or just because you are writing a very
   complicated function.)

2. You can use macro-defining macros (macros that expand into
   `defmacro`), as well as macros that expand into `defun` forms, to
   create local bindings.

3. You can (using `local*` or `block-compile`) easily switch to block
   compilation of top-level functions.

Serapeum’s implementation of internal definitions is as complete as it
can be while remaining portable. That means full support for
variables, functions, and symbol macros, but restricted support for
macros.

### Example: macros that work locally and globally

For example, memoizing local functions is usually clumsy; given `local`
you can define a single `defmemo` form that supports both `defun`
and `labels`.

    (defmacro defmemo (name params &body body)
      (with-gensyms (memo-table args result result?)
        `(let ((,memo-table (make-hash-table :test 'equal)))
           (defun ,name (&rest ,args)
             (multiple-value-bind (,result ,result?)
                 (gethash ,args ,memo-table)
               (if ,result?
                   ,result
                   (setf (gethash ,args ,memo-table)
                         (apply (lambda ,params
                                  ,@body)
                                  ,args))))))))

At the top level, this expands into an example of “let over defun” (gensyms elided for readability):

    ;; This source form
    (defmemo fibonacci (n)
        (if (<= n 1)
            1
            (+ (fibonacci (- n 1))
               (fibonacci (- n 2)))))

    ;; Expands into...
    (let ((memo-table (make-hash-table :test 'equal)))
      (defun fibonacci (&rest args)
        (multiple-value-bind (result result?)
            (gethash args memo-table)
          (if result? result
              (setf (gethash args memo-table)
                    (apply (lambda (n)
                             (if (<= n 1)
                                 1
                                 (+ (fibonacci (- n 1))
                                    (fibonacci (- n 2)))))
                           args))))))

But within a `local` form, it expands differently. This nearly
identical source form:

    (local
      (defmemo fibonacci (n)
        (if (<= n 1)
            1
            (+ (fibonacci (- n 1))
               (fibonacci (- n 2)))))

      (fibonacci 100))

Expands into this very different code (simplified for readability):

    (let (fn)
      (labels ((fibonacci (&rest args)
                 (apply fn args)))
        (let ((memo-table (make-hash-table :test 'equal)))
          (setf fn
                (named-lambda fibonacci (&rest args)
                  (multiple-value-bind (result result?)
                      (gethash args memo-table)
                    (if result? result
                        (setf (gethash args memo-table)
                              (apply
                               (lambda (n)
                                 (if (<= n 1) 1
                                     (+ (fibonacci (- n 1))
                                        (fibonacci (- n 2)))))
                               args))))))

          (fibonacci 100))))

### Example: block compiling

The macro `local*` is almost the same as `local`, except that it
leaves the last form in the body intact. This is useful for obtaining
block compilation in Lisps that don’t have a syntax for it.

During development, you define functions at the top level inside a `progn`.

     (progn
       (defun aux-fn-1 ...)
       (defun aux-fn-2 ...)
       (defun entry-point ...))

Then, when you decide you want block compilation, simply switch the
`progn` to a `local*`:

     (local*
       (defun aux-fn-1 ...)
       (defun aux-fn-2 ...)
       (defun entry-point ...))

Which expands into something like:

    (labels ((aux-fn-2 ...)
             (aux-fn-1 ...))
      (defun entry-point ...))

This has the slight disadvantage that calls to the entry points,
including self calls, will still be compiled as global calls. If you
want calls to the entry points to be compiled as local calls, you can
use the `block-compile` macro instead.

Using `block-compile`, you can write:

    (block-compile (:entry-points (entry-point))
      (defun aux-fn-1 ...)
      (defun aux-fn-2 ...)
      (defun entry-point ...))

And have it expand into something like:

    (labels ((aux-fn-2 ...)
         (aux-fn-1 ...)
         (entry-point ...))
      (defalias entry-point #'entry-point))

## Compile-time exhaustiveness checking

`etypecase-of` is just like `etypecase`, except that it takes an
additional argument – the type to be matched against – and warns, at
compile time, if the clauses in its body are not an exhaustive
partition of that type.

`ecase-of` is a succint variant of `etypecase` with the same syntax as
`ecase`.

`typecase-of` and `case-of` are `etypecase-of` and `ecase-of`,
respectively, except that they expect, and enforce, the presence of an
`otherwise` clause.

There are also continuable versions of these macros – `ctypecase-of` and
`ccase-of`.

### Example: enums

We may call a type defined using `member` an *enumeration*. Take an
enumeration like this:

```
(deftype switch-state ()
  '(member :on :off :stuck :broken))
```

Now we can use `ecase-of` to take all the states of the switch into
account.

```
(defun flick (switch)
  (ecase-of switch-state (state switch)
    (:on (switch-off switch))
    (:off (switch-on switch))))
=> Warning

(defun flick (switch)
  (ecase-of switch-state (state switch)
    (:on (switch-off switch))
    (:off (switch-on switch))
    ((:stuck :broken) (error "Sorry, can't flick ~a" switch))))
=> No warning
```

Even more usefully, we don’t have to worry about bugs caused by misspellings:

``` lisp
(defun flick (switch)
  (ecase-of switch-state (state switch)
    (:on (switch-off switch))
    (:offf (switch-on switch))          ;Gotcha!
    ((:stuck :broken) (error "Sorry, can't flick ~a" switch))))
=> Warning
```

### Example: union types

```
(defun negative-integer? (n)
  (etypecase-of t n
    ((not integer) nil)
    ((integer * -1) t)
    ((integer 1 *) nil)))
=> Warning

(defun negative-integer? (n)
  (etypecase-of t n
    ((not integer) nil)
    ((integer * -1) t)
    ((integer 1 *) nil)
    ((integer 0) nil)))
=> No warning
```

[This article](https://dev.to/vindarel/compile-time-exhaustiveness-checking-in-common-lisp-with-serapeum-5c5i) has more about exhaustiveness checking in Serapeum.


## CLOS

Serapeum includes some utilities for CLOS. These utilities do nothing
earthshaking, but since the function reference does not include them,
they should be documented somewhere.

### Method combination: standard with context

Serapeum exports a method combination, `serapeum:standard/context`.
You may recognize it as the `wrapping-standard` method combination
due to [Tim Bradshaw](https://github.com/tfeb).

Generic functions defined with `standard/context` behave the same as
ordinary generic functions, except that they allow an extra
qualifier, `:context`. This extra qualifier works almost like
`:around`, except instead of being run in most-specific-first order,
like methods defined with `:around`, methods defined with `:context`
are run in most-specific-last order. Furthermore, `:context` methods
take priority over any other methods, including `:around` methods.

The big idea is that a class can use `:context` methods to make sure
that any methods defined by subclasses – even `:around` methods – run
in a certain dynamic context.

### Metaclass: topmost-object-class

In most cases, when I write a metaclass, I want all of the classes
defined using that metaclass to inherit from a specific class.
Injecting a topmost class is not difficult to do, but it involves a
certain amount of boilerplate.

To eliminate that boilerplate, Serapeum exports a metaclass,
`topmost-object-class`, to use as a base class for your metaclasses.
When you define a metaclass, all you have to do to ensure that classes
defined using your metaclass inherit from a specific class is to
supply the name of the class to inherit from in the definition of the
metaclass. This is much better demonstrated than explained:

``` lisp
;;; The class to inherit from.
(defclass my-topmost-object ()
  ())

;;; The metaclass.
(defclass my-metaclass (serapeum:topmost-object-class)
  ()
  (:default-initargs
   :topmost-class 'my-topmost-object))

(defclass my-class ()
  ()
  (:metaclass my-metaclass))

(typep (make-instance 'my-class) 'my-topmost-object) => t
```

Note that, since the topmost object is usually a standard class, there
is a `validate-superclass` method which allows an instance of
`topmost-object-class` to inherit from a standard class.

# Function reference

The complete reference is in a [separate file](REFERENCE.md).

(Note that the reference is generated from docstrings, and should not
be edited by hand.)

[Alexandria]: http://common-lisp.net/project/alexandria/
[SBCL]: http://sbcl.org
[Clozure CL]: http://clozure.com
[ECL]: https://common-lisp.net/project/ecl/
[Allegro]: https://franz.com/products/allegrocl/
[Quicklisp]: https://www.quicklisp.org/beta/
