;;; Macros for defining types.
(in-package :serapeum)

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

(defun read-only-include-clause (clause)
  "Rewrite CLAUSE, an include clause, for use with a read-only
structure."
  (ematch clause
    ((list)
     `(:include %read-only-struct))
    ((list* :include type slot-redefs)
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

;;; Why not allow `defstruct-read-only' to use inheritance? I would
;;; like to, but I haven't figured out yet how to make it work
;;; reliably. The point of `defstruct-read-only' is that when you see
;;; it you know *for sure* that the instances are immutable -- that
;;; all the slots are read-only. But where there is inheritance, there
;;; are no guarantees. Even if we could check that the class being
;;; inherited from has only read-only slots at the time the initial
;;; definition, there is nothing to prevent the superclass (or any
;;; other class in the inheritance chain) from being redefined with
;;; mutable slots in the future.

(defmacro defstruct-read-only (name-and-opts &body slots)
  "Easily define a defstruct with no mutable slots.

The syntax of `defstruct-read-only' is as close as possible to that of
`defstruct'. Given an existing structure definition, you can usually
make it immutable simply by switching out `defstruct' for
`defstruct-read-only'.

There are only a few syntactic differences:

1. To prevent accidentally inheriting mutable slots,
   `defstruct-read-only' does not allow inheritance.

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
        (when-let (clause (find :include opts :key #'car-safe))
          (error "Read-only struct ~a cannot use inheritance: ~s."
                 name clause))
        (when (find :copier opts :key #'ensure-car)
          (error "Read only struct ~a does not need a copier."
                 name))
        (when-let (clause (find :type opts :key #'car-safe))
          (error "Read-only structs may not use the ~s option: ~a"
                 :type clause))
        (multiple-value-bind (include-clause opts)
            (if-let (clause (find :include opts :key #'car-safe))
              (values clause (remove clause opts))
              (values nil opts))
          (let* ((slot-defs (mapcar #'read-only-slotdef slots))
                 (slot-names (mapcar #'first slot-defs)))
            `(progn
               (defstruct (,name (:copier nil)
                                 ;; Mark as OK to save in pure memory.
                                 #+(or sbcl cmucl) (:pure t)
                                 ,@opts
                                 ,(read-only-include-clause include-clause))
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
  ;; "If `*read-eval*' is false and `*print-readably*' is
  ;; true, any method for `print-object' that would output a
  ;; reference to the `#.' reader macro either outputs
  ;; something different or signals an error of type
  ;; `print-not-readable'."
  (if *print-readably*
      (if *read-eval*
          "#."
          (print-unreadable-object (object stream :type t)))
      ""))

(defun print-constructor (object stream fields)
  (write-string (read-eval-prefix object stream) stream)
  (write-char #\( stream)
  (prin1 (type-of object) stream)
  (dolist (field fields)
    (write-char #\Space stream)
    (prin1 field stream))
  (write-char #\) stream)
  (values))

(defgeneric constructor-values/generic (x))

(defun deconstruct (x)
  (constructor-values/generic x))

;;; NB If you ever figure out how to safely support inheritance in
;;; read-only structs, you should *still* not allow constructors to
;;; inherit from one another. Cf. Scala, where they forbid case class
;;; inheritance for good reasons.

(defmacro defconstructor (type-name &body slots)
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
subtype may not be recognized in type tests.

Because `defconstructor' is implemented on top of
`defstruct-read-only', it shares the limitations of
`defstruct-read-only'. In particular it cannot use inheritance.

The design of `defconstructor' is mostly inspired by Scala's [case
classes](https://docs.scala-lang.org/tour/case-classes.html), with
some implementation tricks from `cl-algebraic-data-type'."
  (check-type type-name symbol)
  (let* ((docstring
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
                            a name and a type.")))))
         (constructor type-name)
         (slot-names (mapcar #'first slots))
         (conc-name
           (symbolicate type-name '-))
         (readers
           (mapcar (curry #'symbolicate conc-name)
                   slot-names))
         (copier-name (symbolicate 'copy- type-name)))
    `(progn
       ;; Make sure the constructor is inlined. This is necessary on
       ;; SBCL to allow instances to be stack-allocated.
       (declaim (inline ,constructor))

       ;; Actually define the type.
       (defstruct-read-only
           (,type-name
            (:constructor ,constructor ,slot-names)
            (:conc-name ,conc-name)
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
       (defun ,copier-name
           (,type-name &key
                         ,@(loop for (slot-name nil) in slots
                                 for reader in readers
                                 collect `(,slot-name (,reader ,type-name))))
         ,(fmt "Copy ~:@(~a~), optionally overriding ~
some or all of its slots." type-name)
         (declare (ignorable ,type-name))
         ;; A copier without slots should be identity.
         ,(if (null readers)
              type-name
              `(,type-name ,@slot-names)))

       ;; Define a load form.
       (defmethod make-load-form ((self ,type-name) &optional env)
         (declare (ignore env))
         (declare (ignorable self))
         (list ',type-name
               ,@(loop for reader in readers
                       collect `(,reader self))))

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

;;; Why use CLOS for unit types? Structures have two benefits: low
;;; memory use and fast slot access. Neither benefit applies to unit
;;; types. Since each class has only one instance, memory usage
;;; doesn't matter. And since unit types have no slots, fast slot
;;; access is irrelevant. We might as well use CLOS, and take
;;; advantage of the machinery of `make-instance' to make sure there
;;; can only ever be one instance, without any ugly hacks.

(defclass unit-object ()
  ()
  (:documentation "Superclass for instances of unit classes.
Gives us a place to hang specializations for `print-object' and
`make-load-form'."))

(defmethod print-object ((self unit-object) stream)
  (format stream
          "~a~s"
          (read-eval-prefix self stream)
          (class-name (class-of self))))

(defmethod make-load-form ((self unit-object) &optional env)
  (declare (ignore env))
  `(make-instance ,(class-of self)))

(defmethod shared-initialize ((self unit-object) slot-names &rest initargs)
  (declare (ignore slot-names))
  (when initargs
    (error "A unit object cannot be initialized.")))

(defclass unit-class (topmost-object-class)
  ((lock :initform (bt:make-lock))
   (instance :initform nil :reader unit-class-instance))
  (:default-initargs :topmost-class 'unit-object))

;;; Note that `topmost-object-class' permits inheriting from a
;;; standard object.

;;; Unit classes should never be superclasses.
(defmethod c2mop:validate-superclass
    (c1 (c2 unit-class))
  (declare (ignore c1))
  nil)

;;; NB For some reason, on Clozure, specializing `allocate-instance'
;;; directly causes printing to go into an infinite loop if the class
;;; has been redefined.

(defmethod make-instance ((class unit-class) &rest initargs)
  (declare (ignore initargs))
  (with-slots (instance lock) class
    ;; "Double-checked locking".
    (or instance
        (bt:with-lock-held (lock)
          (or instance
              (setf instance (call-next-method)))))))

(defmacro defunit (name)
  "Define a unit type.

A unit type is a type that only allows one value.

Or: a unit type is a singleton without state.

Or: a unit type is a product type with no factors.

Or: a unit type is a unique object, like a symbol, but tagged with its
own individual type."
  `(progn
     (defclass ,name () ()
       (:metaclass unit-class))
     (unless (c2mop:class-finalized-p (find-class ',name))
       (c2mop:finalize-inheritance (find-class ',name)))
     (declaim-freeze-type ,name)
     (defmethod %constructor= ((x ,name) (y ,name))
       t)
     (define-symbol-macro ,name
         (load-time-value (make-instance ',name) t))))

(defmacro defunion (union &body variants)
  "Define an algebraic data type.

VARIANTS defines the subtypes of UNION. Each expression in VARIANTS is
either a symbol \(in which case it defines a unit type, as with
`defunit') or a list \(in which case it defines a structure, as with
`defconstructor'.

AKA a tagged union or a discriminated union."
  (let* ((ctors (filter #'listp variants))
         (units (filter #'atom variants))
         (types (append units (mapcar #'first ctors))))
    `(progn
       ,@(loop for type in units
               collect `(defunit ,type))
       ,@(loop for (type . slots) in ctors
               collect `(defconstructor ,type ,@slots))
       (deftype ,union ()
         '(or ,@types)))))

(declaim (ftype (function (t t) (values t t))))
(defun pattern-type (pattern union)
  "Return two values: the type and the final pattern."
  (match pattern
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

(defmacro match-of (union expr &body clauses &environment env)
  "Do pattern matching on an algebraic data type.

UNION should be an algebraic data type.

Each clause in CLAUSES has a pattern as its first element. The pattern
may be a symbol (in which case it matches a unit type) or a list (in
which case it matches against a constructor). Specifically, an
underscore introduces a default or fallthrough clause, and the pattern
may also be a disjunction of other types (an `or' type)."
  (once-only (expr)
    ;; The complexity  here comes from allowing  multiple clauses with
    ;; the same type (but different patterns).
    (let* ((types-and-patterns
             ;; Collecting a list of (type . (pattern . body)) items.
             (loop for (pattern . body) in clauses
                   collect (multiple-value-bind (type pattern)
                               (pattern-type pattern union)
                             (assert (and type pattern))
                             `(,type
                               (,pattern ,@body)))))
           ;; Group the clauses by their type.
           (type-groups
             (assort types-and-patterns :key #'first
                                        :test (lambda (x y)
                                                ;; `type=' doesn't take an env.
                                                (and (subtypep x y env)
                                                     (subtypep y x env)))))
           ;; Transform the groups into a list of (type (pattern . body)*).
           (type-groups
             (mapcar (lambda (group)
                       (cons (caar group)
                             (mappend #'cdr group)))
                     type-groups))
           ;; Turn the groups into clauses, with the patterns roped into
           ;; ematch forms.
           (merged-typecase-clauses
             (loop for (type . clauses) in type-groups
                   collect `(,type
                             (ematch ,expr
                               ,@clauses)))))
      `(etypecase-of ,union ,expr
         ,@merged-typecase-clauses))))
