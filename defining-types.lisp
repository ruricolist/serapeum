(defpackage :serapeum/defining-types
  (:documentation "Macros for defining types.")
  #+sb-package-locks (:lock t)
  (:use
   :cl
   :alexandria
   :serapeum/definitions
   :serapeum/macro-tools
   :trivia)
  (:import-from
   :serapeum/control-flow
   :check-exhaustiveness)
  (:import-from
   :serapeum/types
   :declaim-freeze-type)
  (:export
   #:deconstruct
   #:defcondition
   #:defconstructor
   #:defstruct-read-only
   #:defunion
   #:defunit
   #:match-of
   #:read-eval-prefix))
(in-package :serapeum/defining-types)

(defmacro defcondition (name supers &body (slots &rest options))
  "Alias for `define-condition'.

Like (define-condition ...), but blissfully conforming to the same
nomenclatural convention as every other definition form in Common
Lisp."
  `(define-condition ,name ,supers
     ,slots
     ,@options))

(defgeneric read-only-struct-slot-names (x)
  (:method append ((x t)) nil)
  (:method-combination append))

(defstruct
    (%read-only-struct
     (:constructor nil)                 ;Abstract.
     (:copier nil)
     (:predicate nil))
  "Abstract base class for read-only structures.")

(defmethod make-load-form ((self %read-only-struct) &optional env)
  (let* ((slot-names (read-only-struct-slot-names self))
         (slot-names (remove-duplicates slot-names)))
    (make-load-form-saving-slots self
                                 :slot-names slot-names
                                 :environment env)))

(defun read-only-include-clause (name clause &optional env)
  "Rewrite CLAUSE, an include clause, for use with a read-only
structure."
  (ematch clause
    ((list)
     `(:include %read-only-struct))
    ((list* :include type slot-redefs)
     (multiple-value-bind (sub? sure?) (subtypep type '%read-only-struct env)
       (when (and (not sub?) sure?)
         (error "Read-only struct ~a can only inherit from other structs defined as read-only."
                name)))
     ;; Do we need to specify that the slots are read-only? No, since
     ;; we can only inherit from read-only structures, and the
     ;; specification says that read-only slots cannot be redefined as
     ;; mutable by their heirs. However, we still need to process the
     ;; slots, so we can support skipping the initform to supply a
     ;; `:type' option (which, of course, must be a subtype of the
     ;; slot's type in the parent).
     `(:include ,type
                ,@(mapcar #'read-only-slotdef slot-redefs)))))

(defun read-only-slotdef (slot)
  "Rewrite SLOT, a structure slot definition, to be read-only.
Note that if SLOT is of an odd length, then it is treated as being
without an initialization form, and a default initialization form that
raises an error is supplied."
  (let ((slot (ensure-list slot)))
    (multiple-value-bind (name initform args)
        (if (oddp (length slot))
            ;; Name (1) + keyword arguments (2n) = 2n+1.
            (destructuring-bind (name . args) slot
              (values name `(required-argument ',name) args))
            ;; Name (1) + initform (1) + keyword arguments (2n) = 2n+2.
            (destructuring-bind (name initform . args) slot
              (values name initform args)))
      (destructuring-bind (&key (read-only t read-only-supplied?) &allow-other-keys) args
        (declare (ignore read-only))
        (when read-only-supplied?
          (simple-style-warning "Redundant read-only declaration in slot definition ~s"
                                slot))
        (let ((args (remove-from-plist args :read-only)))
          `(,name
            ,initform
            :read-only t
            ,@args))))))

(defun include+opts (opts)
  (flet ((car-safe (x) (if (consp x) (car x) x)))
    (if-let (clause (find :include opts :key #'car-safe))
      (values clause (remove clause opts))
      (values nil opts))))

(defmacro defstruct-read-only (name-and-opts &body slots
                               &environment env)
  "Easily define a defstruct with no mutable slots.

The syntax of `defstruct-read-only' is as close as possible to that of
`defstruct'. Given an existing structure definition, you can usually
make it immutable simply by switching out `defstruct' for
`defstruct-read-only'.

There are only a few syntactic differences:

1. To prevent accidentally inheriting mutable slots, and preserve its
   own usefulness as a marker of the programmer's intent,
   `defstruct-read-only' only allows inheritance from other classes
   defined using `defstruct-read-only'.

2. The `:type' option may not be used.

3. The `:copier' option is disabled, because it would be useless.

4. Slot definitions can use slot options without having to provide an
   initform. In this case, any attempt to make an instance of the
   struct without providing a value for that slot will signal an
   error.

    (my-slot :type string)
    ≡ (my-slot (required-argument 'my-slot) :read-only t :type string)

The idea here is simply that an unbound slot in an immutable data
structure does not make sense.

A read-only struct is always externalizable; it has an implicit
definition for `make-load-form'.

On Lisps that support it, the structure is also marked as \"pure\":
that is, instances may be moved into read-only memory.

`defstruct-read-only' is designed to stay as close to the syntax of
`defstruct' as possible. The idea is to make it easy to flag data as
immutable, whether in your own code or in code you are refactoring. In
new code, however, you may sometimes prefer `defconstructor', which is
designed to facilitate working with immutable data."
  (flet ((car-safe (x) (if (consp x) (car x) nil)))
    (let ((docstring (and (stringp (first slots)) (pop slots))))
      (destructuring-bind (name . opts) (ensure-list name-and-opts)
        (when-let (copier (find :copier opts :key #'ensure-car))
          (if (null (second copier))
              (removef opts copier)
              (error "Read only struct ~a does not need a copier."
                     name)))
        (when-let (clause (find :type opts :key #'car-safe))
          (error "Read-only structs may not use the ~s option: ~a"
                 :type clause))
        (multiple-value-bind (include-clause opts)
            (include+opts opts)
          (let* ((slot-defs (mapcar #'read-only-slotdef slots))
                 (slot-names (mapcar #'first slot-defs)))
            `(progn
               (defstruct (,name (:copier nil)
                                 ;; Mark as OK to save in pure memory.
                                 #+(or sbcl cmucl) (:pure t)
                                 ,@opts
                                 ,(read-only-include-clause name include-clause env))
                 ,@(unsplice docstring)
                 ,@slot-defs)
               (defmethod read-only-struct-slot-names append ((self ,name))
                 ',slot-names)
               ',name)))))))

;;; TODO Is constructor= worth exporting? Or would that be too
;;; aggressive?

;;; It is arguably good style, when defining a generic function for
;;; extensibility, not to call that generic function internally.
;;; Instead, you call a wrapper function which, in turn, calls the
;;; generic function. That way you can always be sure of the ultimate
;;; dynamic environment the generic function will run in.

(defun constructor= (x y)
  (or (equal x y)
      (%constructor= x y)))

(defgeneric %constructor= (x y)
  (:method (x y)
    (equal x y)))

(defun read-eval-prefix (object stream)
  "A helper for making objects readable.

The obvious way to give an object a readable representation is to use
the sharp-dot reader macro. However, methods are supposed to consult
the values of `*print-readably*' and `*read-eval*' before doing so.
This function takes care of that for you.

If `*print-readably*' is false, return an empty string.

If `*print-readably*' is true, and `*read-eval*' is also true, return
the string \"#.\".

If `*print-readably*' is true, but `*read-eval*' is not true, signal
an error."
  ;; "If `*read-eval*' is false and `*print-readably*' is
  ;; true, any method for `print-object' that would output a
  ;; reference to the `#.' reader macro either outputs
  ;; something different or signals an error of type
  ;; `print-not-readable'."
  (if *print-readably*
      (if *read-eval*
          "#."
          (error 'print-not-readable
                 :object object
                 :stream stream))
      ""))

(defun print-constructor (object stream fields)
  (let ((readable? *print-readably*))
    (write-string (read-eval-prefix object stream) stream)
    (write-char #\( stream)
    (prin1 (type-of object) stream)
    (dolist (field fields)
      (write-char #\Space stream)
      (when readable?
        (unless (constantp field)
          (write-char #\' stream)))
      (prin1 field stream))
    (write-char #\) stream)
    (values)))

(defgeneric constructor-values/generic (x))

(defun deconstruct (x)
  "If X is a type defined with `defconstructor', return its slots as
multiple values."
  (constructor-values/generic x))

;;; NB If you ever figure out how to safely support inheritance in
;;; read-only structs, you should *still* not allow constructors to
;;; inherit from one another. Cf. Scala, where they forbid case class
;;; inheritance for good reasons.

(defmacro defconstructor (type-name &body slots
                          &environment env)
  "A variant of `defstruct' for modeling immutable data.

The structure defined by `defconstructor' has only one constructor,
which takes its arguments as required arguments (a BOA constructor).
Thus, `defconstructor' is only appropriate for data structures that
require no initialization.

The printed representation of an instance resembles its constructor:

    (person \"Common Lisp\" 33)
    => (PERSON \"Common Lisp\" 33)

While the constructor is BOA, the copier takes keyword arguments,
allowing you to override the values of a selection of the slots of the
structure being copied, while retaining the values of the others.

    (defconstructor person
      (name string)
      (age (integer 0 1000)))

    (defun birthday (person)
      (copy-person person :age (1+ (person-age person))))

    (birthday (person \"Common Lisp\" 33))
    => (PERSON \"Common Lisp\" 34)

Obviously the copier becomes more useful the more slots the type has.

When `*print-readably*' is true, the printed representation is
readable:

    (person \"Common Lisp\" 33)
    => #.(PERSON \"Common Lisp\" 33)

\(Why override how a structure is normally printed? Structure types
are not necessarily readable unless they have a default \(`make-X')
constructor. Since the type defined by `defconstructor' has only one
constructor, we have to take over to make sure it re-readable.)

Besides being re-readable, the type is also externalizable, with a
method for `make-load-form':

    (make-load-form (person \"Common Lisp\" 33))
    => (PERSON \"Common Lisp\" 33)

Users of Trivia get an extra benefit: defining a type with
`defconstructor' also defines a symmetrical pattern for destructuring
that type.

    (trivia:match (person \"Common Lisp\" 33)
      ((person name age)
       (list name age)))
    => (\"Common Lisp\" 33)

Note that the arguments to the pattern are optional:

    (trivia:match (person \"Common Lisp\" 33)
      ((person name) name))
    => \"Common Lisp\"

If you don't use Trivia, you can still do destructuring with
`deconstruct', which returns the slots of a constructor as multiple
values:

    (deconstruct (person \"Common Lisp\" 33))
    => \"Common Lisp\", 33

Note also that no predicate is defined for the type, so to test for
the type you must either use `typep' or pattern matching as above.

While it is possible to inherit from a type defined with
`defconstructor' (this is Lisp, I can't stop you), it's a bad idea. In
particular, on Lisps which support it, a type defined with
`defconstructor' is declared to be frozen (sealed), so your new
subtype may not be recognized in type tests that have already been
compiled.

Because `defconstructor' is implemented on top of
`defstruct-read-only', it shares the limitations of
`defstruct-read-only'. In particular it cannot use inheritance.

The design of `defconstructor' is mostly inspired by Scala's [case
classes](https://docs.scala-lang.org/tour/case-classes.html), with
some implementation tricks from `cl-algebraic-data-type'."
  (let* ((super (env-super env))
         (docstring
           (and (stringp (first slots))
                (pop slots)))
         (slots
           (loop for slot in slots
                 collect (match slot
                           ((list (and slot-name (type symbol))
                                  type)
                            (list slot-name type))
                           (otherwise
                            (error "Constructor slots must have both ~
                            a name and a type.~%Constructor: ~a"
                                   type-name)))))
         (constructor type-name)
         (slot-names (mapcar #'first slots))
         (readers
           (mapcar (curry #'symbolicate type-name '-)
                   slot-names))
         (copier-name (symbolicate 'copy- type-name)))
    `(progn
       ;; Make sure the constructor is inlined. This is necessary on
       ;; SBCL to allow instances to be stack-allocated.
       (declaim (inline ,constructor))

       ;; Actually define the type.
       (defstruct-read-only
           (,type-name
            ,@(unsplice (and super `(:include ,super)))
            (:constructor ,constructor ,slot-names)
            (:predicate nil)
            (:print-function
             (lambda (object stream depth)
               (declare (ignore depth))
               ,(if (null readers)
                    `(print-constructor object stream nil)
                    (with-unique-names (fields)
                      `(let ((,fields
                               (list
                                ,@(loop for reader in readers
                                        collect `(,reader object)))))
                         (declare (dynamic-extent ,fields))
                         (print-constructor object stream ,fields)))))))
         ,@(unsplice docstring)
         ,@(loop for (slot-name slot-type) in slots
                 collect `(,slot-name :type ,slot-type)))

       ;; Freeze the type when possible.
       (declaim-freeze-type ,type-name)

       ;; Define the copier.
       (declaim (inline ,copier-name))
       ;; The gensym is needed in case one of the slots has the same
       ;; name as the type itself.
       ,(let ((orig (gensym (string type-name))))
          `(defun ,copier-name
               (,orig &key
                        ,@(loop for (slot-name nil) in slots
                                for reader in readers
                                collect `(,slot-name (,reader ,orig))))
             ,(format nil "Copy an instance of ~:@(~a~), optionally ~
overriding some or all of its slots." type-name)
             (declare (ignorable ,orig))
             ;; A copier without slots should be identity.
             ,(if (null readers)
                  orig
                  `(,type-name ,@slot-names))))

       ;; Define a load form.
       (defmethod make-load-form ((self ,type-name) &optional env)
         (declare (ignorable self env))
         (list ',type-name
               ,@(loop for reader in readers
                       collect `(quote-unless-constant (,reader self)
                                                       env))))

       ;; Define a comparison method.
       (defmethod %constructor= ((o1 ,type-name) (o2 ,type-name))
         ,(if readers
              `(and ,@(loop for reader in readers
                            collect `(constructor= (,reader o1)
                                                   (,reader o2))))
              t))

       ;; Define a destructor.
       (defmethod constructor-values/generic ((x ,type-name))
         (values ,@(loop for reader in readers
                         collect `(,reader x))))

       ;; Define a pattern to match.
       (defpattern ,type-name
           ,(if slot-names
                `(&optional ,@slot-names)
                ())
         (list
          'and
          (list 'type ',type-name)
          ,@(loop for reader in readers
                  for name in slot-names
                  collect `(list 'trivia:access '',reader ,name))))
       ',type-name)))

(defun quote-unless-constant (value &optional env)
  (if (constantp value env)
      value
      `(quote ,value)))

(defstruct (%unit
            (:constructor nil)          ;Abstract.
            (:copier nil)
            (:predicate nil)
            (:include %read-only-struct))
  "Abstract base class for unit classes.")

(defun print-unit (object stream depth)
  (declare (ignore depth))
  (format stream
          "~a~s"
          (read-eval-prefix object stream)
          (class-name (class-of object))))

(defvar *units* (make-hash-table))

(let ((units-lock (bt:make-lock)))
  (defun intern-unit (name ctor)
    (bt:with-lock-held (units-lock)
      (or (gethash name *units*)
          (setf (gethash name *units*)
                (funcall ctor))))))

(defmacro defunit (name &optional docstring
                   &environment env)
  "Define a unit type.

A unit type is a type with only one instance.

You can think of a unit type as a singleton without state.

Unit types are useful for many of the same purposes as quoted symbols
\(or keywords) but, unlike a symbol, a unit type is tagged with its
own individual type."
  (let ((ctor (symbolicate '%make- name))
        (super (env-super env '%unit)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defstruct (,name
                     (:include ,super)
                     (:constructor ,ctor)
                     (:copier nil)
                     (:predicate nil)
                     (:print-function print-unit))
           ,@(unsplice docstring)))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defmethod make-load-form ((x ,name) &optional env)
           (declare (ignore env))
           '(intern-unit ',name ',ctor)))
       (defmethod %constructor= ((x ,name) (y ,name))
         t)
       (defconst ,name (intern-unit ',name ',ctor))
       (declaim-freeze-type ,name)
       (fmakunbound ',ctor)
       ',name)))

;;; Use an unlocked symbol to work around an SBCL bug.
(define-symbol-macro serapeum/unlocked:%union nil)

(defun env-super (env &optional default)
  "Look for the superclass bound in ENV."
  (or (macroexpand-1 'serapeum/unlocked:%union env) default))

(defmacro defunion (union &body variants)
  "Define an algebraic data type.

Each expression in VARIANTS is either a symbol \(in which case it
defines a unit type, as with `defunit') or a list \(in which case it
defines a read-only structure, as with `defconstructor').

UNION is defined as a type equivalent to the disjunction of all the
member types. A class is also defined, with the same name, but with
angle brackets around it."
  (let* ((docstring (and (stringp (first variants))
                         (pop variants)))
         (ctors (remove-if-not #'listp variants))
         (units (remove-if-not #'atom variants))
         (types (append units (mapcar #'first ctors)))
         (super (symbolicate '< union '>)))
    ;; CCL (and maybe other lisps) evaluates `(:include)' options at
    ;; macroexpansion time and does some environment augmenting and checking
    ;; that fails to account for our `env-super' trick.
    ;; So define the abstract base struct via a `progn' at the toplevel.
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defstruct (,super
                     (:constructor nil)
                     (:copier nil)
                     (:predicate nil)
                     (:include %read-only-struct))
           ,@(unsplice docstring)))
       ;; NB The declarations are not currently used, due to an SBCL bug.
       (locally (declare #+sbcl (sb-ext:disable-package-locks %union))
         (symbol-macrolet ((serapeum/unlocked:%union ,super))
           (declare #+sbcl (sb-ext:enable-package-locks %union))
           (deftype ,union ()
             ,@(unsplice docstring)
             '(or ,@types))
           ,@(loop for type in units
                   collect `(defunit ,type))
           ,@(loop for (type . slots) in ctors
                   collect `(defconstructor ,type ,@slots))
           (declaim-freeze-type ,super)
           ',union)))))

(declaim (ftype (function (t t) (values t t))))
(defun pattern-type (pattern union)
  "Return two values: the type and the final pattern."
  (match pattern
    ((list 'eql type)
     (values `(eql ,type) `(eql ,type)))
    ((and lit (type (or number keyword)))
     (pattern-type `(eql ,lit) union))
    ((list 'quote object)
     (pattern-type `(eql ',object) union))
    ((and type (type symbol))
     (if (string= pattern "_")
         (values union `(and _ (type ,union)))
         (values type `(and _ (type ,type)))))
    ((list 'assure type _)
     (values type `(and _ (type ,type))))
    ((list* 'or patterns)
     (multiple-value-bind (types patterns)
         (loop for each in patterns
               for (type . pats)
                 = (multiple-value-list
                    (pattern-type each union))
               collect type into types
               append pats into patterns
               finally (return (values types patterns)))
       (values `(or ,@types)
               `(or ,@patterns))))
    ((list 'not pat)
     (multiple-value-bind (type pattern)
         (pattern-type pat union)
       (values `(not ,type)
               `(not ,pattern))))
    ((list* type _)
     (values type pattern))
    (otherwise (error "Cannot infer a type from pattern ~a" pattern))))

(defun check-match-exhaustive (union clauses env)
  (let* ((clauses
           ;; Collecting a list of (type . (pattern . body)) items.
           (loop for (pattern . nil) in clauses
                 collect (list (pattern-type pattern union)))))
    (check-exhaustiveness 'typecase union clauses env)))

(defmacro match-of (union expr &body clauses &environment env)
  "Do pattern matching on an algebraic data type.

UNION should be an algebraic data type.

Each clause in CLAUSES has a pattern as its first element.

If the pattern is a symbol, it matches a unit type.

If the pattern is a list, it matches a constructor.

If the pattern is an underscore, it introduces a default or
fallthrough clause.

If the pattern is a list that starts with `or', it is a disjunction of
other patterns."
  (check-match-exhaustive union clauses env)
  `(ematch ,expr
     ,@(loop for (pattern . body) in clauses
             collect `(,(nth-value 1 (pattern-type pattern union))
                       ,@body))))
