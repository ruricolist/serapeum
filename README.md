# Overview

[![Build Status](https://travis-ci.org/TBRSS/serapeum.svg?branch=master)](https://travis-ci.org/TBRSS/serapeum)

Serapeum is a conservative library of Common Lisp utilities. It is a
supplement, not a competitor, to [Alexandria][]. That means it is safe to
do:

    (defpackage ... (:use #:cl #:alexandria #:serapeum),

without package conflicts.

In general, Serapeum tries to provide complete, correct
implementations of utilities for which a complete, correct
implementation is non-trivial.

There may already be too many utility libraries for Common Lisp.
Releasing another has become something to apologize for, not
celebrate. But I would rather make my apologies than have to maintain
copy-pasted versions of the same utilities across a dozen systems.
And, though Serapeum is justified even if only I ever use it, the best
way to ensure its quality is to write it as if for general use.

Serapeum is conservative: it contains only utilities I actually use,
and which have survived refactoring. But it is less conservative than
Alexandria. Alexandria limits itself to utilities with a Common Lisp
pedigree. Serapeum casts a wider net: other dialects of Lisp, and
other languages in the functional and array families, have been
drafted.

Alexandria is self-contained. It exists in splendid isolation, without
depending on, or even acknowledging, other libraries. Serapeum tries
to be a good citizen of the Quicklisp era: whenever possible, it
avoids duplicating functionality that can be had elsewhere.

Some of the utilities in Serapeum are original; others are borrowed
from other languages, or from other Lispers. I try to give credit in
the docstrings, but sometimes I have forgotten where I got an idea or
a name. I regard missing credits as bugs: please report them.

Serapeum is intended to be portable, but it is only tested where it is
developed, on SBCL and Clozure CL. Patches for other Lisps are
welcome, whether bug fixes or unportable enhancements.

# Commentary

One goal of Serapeum is to have excellent documentation. A utility
library is a fork of its language; it deserves documentation of the
same quality as a language reference. If a utility is not worth
documenting, it is not worth having.

The full function reference will be found [here](reference.md). (It is
in a separate file in deference to documentation browsers, which often
print the README as a preamble to their own function reference).

Most utilities in Serapeum stand alone, but there are some families
that deserve separate introduction.

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
the optimizing transformation from Waddell et. al., *Fixing Letrec*.

For binding values in the function namespace at the top level,
Serapeum provides `defalias`:

    (defalias xcons (flip #'cons))
    
This is equivalent to `(setf (fdefinition ...))`, but also gives the
function a compile-time definition so compilers don’t complain about
its being undefined.

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

The function that returns a sequence in *batches* of a certain size is
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

## Compile-time exhaustiveness checking

`etypecase-of` is just like `etypecase`, except that it takes an
additional argument – the type to be matched against – and warns, at
compile time, if the clauses in its body are not an exhaustive
partition of that type.

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

`ecase-of` is a succint variant of `etypecase` with the same syntax as
`ecase`.

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

`typecase-of` and `case-of` are `etypecase-of` and `ecase-of`,
respectively, except that they expect, and enforce, the presence of an
`otherwise` clause.

There are continuable versions of these macros – `ctypecase-of` and
`ccase-of`.

## Internal definitions

The `local` form lets you use top-level definition forms to create local
bindings. You can use `defun` instead of labels, `defmacro` instead of
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

3. You can (using `local*`) easily switch to block compilation of
   top-level functions.
   
Serapeum’s implementation of internal definitions is as complete as it
can be while remaining portable. That means full support for
variables, functions, and symbol macros, but restricted support for
macros.

### Using macros that expand into top-level definitions

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

This expands into `let` and `labels` as you might expect.

    (local
      (defmemo fibonacci (n)
        (if (<= n 1)
            1
            (+ (fibonacci (- n 1))
               (fibonacci (- n 2)))))
    
      (fibonacci 100))
      => 573147844013817084101

### Block compiling

The macro `local*` is almost the same as `local`, except that it
leaves the last form in the body intact. This is useful for obtaining
block compilation in Lisps that don’t have a syntax for it.

During development, you define functions at the top level inside a `progn`.

     (progn
       (defun aux-fn ...)
       (defun entry-point ...))

Then, when you decide you want block compilation, simply switch the
`progn` to a `local*`:

     (local*
       (defun aux-fn ...)
       (defun entry-point ...))

# Function reference

The complete reference is in a [separate file](reference.md).

(Note that the reference is generated from docstrings, and should not
be edited by hand.)

[Alexandria]: http://common-lisp.net/project/alexandria/
