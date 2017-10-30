(in-package :serapeum)

;;;# Lexical globals

;;; `def' and `defconst' are both applications of the same idea: using
;;; symbol macros to get lexical behavior from global variables (via
;;; `defparameter' and `defconstant', respectively).

(defmacro def (var &body (&optional val documentation))
  "The famous \"deflex\".

Define a top level (global) lexical VAR with initial value VAL,
which is assigned unconditionally as with DEFPARAMETER. If a DOC
string is provided, it is attached to both the name |VAR| and the name
*STORAGE-FOR-DEFLEX-VAR-|VAR|* as a documentation string of kind
'VARIABLE. The new VAR will have lexical scope and thus may be
shadowed by LET bindings without affecting its dynamic (global) value.

The original `deflex' is due to Rob Warnock.

This version of `deflex' differs from the original in the following ways:

- It is possible for VAL to close over VAR.
- On implementations that support it (SBCL, CCL, and LispWorks, at the
moment) this version creates a backing variable that is \"global\" or
\"static\", so there is not just a change in semantics, but also a
gain in efficiency.
- If VAR is a list that starts with `values`, each element is treated as
a separate variable and initialized as if by `(setf (values VAR...)
VAL)`."
  ;; From the ISO-COMPATIBILITY issue writeup: "DEFINE-SYMBOL-MACRO
  ;; can be used to define global lexicals, by having a global lexical
  ;; be a symbol macro that expands into a reference to a globally
  ;; allocated cell that is not subject to dynamic binding." So not
  ;; only was this use of `define-symbol-macro' foreseen, it is also a
  ;; major reason why it was included in CL at all. (The same goes for
  ;; `defconst' below.)
  (ematch var
    ((list 'values)
     `(progn ,val))
    ((list 'values var)
     `(def ,var ,val ,@(unsplice documentation)))
    ((list* 'values vars)
     `(mvdef ,vars ,val ,@(unsplice documentation)))
    ((and var (type (and symbol (not null))))
     (let* ((s0 (symbol-name '#:*storage-for-deflex-var-))
            (s1 (symbol-name var))
            (s2 (symbol-name '#:*))
            (s3 (symbol-package var))	; BUGFIX [see above]
            (backing-var (intern (concatenate 'string s0 s1 s2) s3)))
       ;; Note: The DEFINE-SYMBOL-MACRO must precede VAL so VAL can close
       ;; over VAR.
       `(progn
          (define-symbol-macro ,var ,backing-var)
          (global-vars:define-global-parameter* ,backing-var ,val
            ,@(unsplice documentation))
          ',var)))))

(defmacro mvdef (vars &body (&optional expr documentation))
  `(progn
     ,@(loop for var in vars
             collect `(def ,var nil ,@(unsplice documentation)))
     (setf (values ,@vars) ,expr)
     ',vars))

(defmacro define-values (values &body (expr))
  "Like `def', but for multiple values.
Each variable in VALUES is given a global, lexical binding, as with
`def', then set all at once, as with `multiple-value-setq'."
  `(progn
     ,@(loop for v in values collect `(def ,v nil))
     (setf (values ,@values) ,expr)
     (values ,@(loop for v in values collect `(quote ,v)))))

(defun same-literal-p (x y)
  "A predicate that compares whether X and Y have the same literal
  representation."
  (or (equal x y)
      ;; Crude, but reliable.
      (handler-case
          ;; "If ‘*read-eval*’ is false and ‘*print-readably*’ is true,
          ;; any such method that would output a reference to the
          ;; "‘#.’" reader macro will either output something else or
          ;; will signal an error (as described above)."
          (let ((*read-eval* t))
            (string= (write-to-string x :readably t)
                     (write-to-string y :readably t)))
        (print-not-readable () nil))))

(defmacro defconst (symbol init &optional docstring)
  "Define a constant, lexically.

`defconst' defines a constant using a strategy similar to `def', so
you don’t have to +cage+ your constants.

The constant is only redefined on re-evaluation if INIT has a
different literal representation than the old value.

The name is from Emacs Lisp."
  (let ((backing-var (symbolicate '#:+storage-for-deflex-var- symbol '+)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (define-constant ,backing-var ,init
         :test 'same-literal-p
         :documentation ,docstring)
       (define-symbol-macro ,symbol ,backing-var))))

;;;# Emacs-alikes

;;;## `defsubst'

;;; This is of course by way of Emacs, although it's actually much
;;; older.

(defmacro defsubst (name params &body body)
  "Define an inline function.

     (defsubst fn ...)
     ≡ (declaim (inline fn))
       (defun fn ...)

The advantage of a separate defining form for inline functions is that
you can't forget to declaim the function inline before defining it –
without which it may not actually end up being inlined.

From Emacs and other ancient Lisps."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,params
       ,@body)))

;;;## `defalias'

;;; In Emacs `defalias' is really just `fset': both the name and the
;;; value are evaluated. Here we only evaluate the value.

(defmacro defalias (alias &body (def &optional docstring)
                    &environment env)
  "Define a value as a top-level function.

     (defalias string-gensym (compose #'gensym #'string))

Like (setf (fdefinition ALIAS) DEF), but with a place to put
documentation and some niceties to placate the compiler.

Name from Emacs Lisp."
  ;; NB `uiop:defun*' is a version of defun intended to portably
  ;; support redefinition at compile time. We leverage it here to make
  ;; sure we get consistent behavior everywhere.
  (multiple-value-bind (env decls lambda)
      (let-over-lambda def env)
    ;; If we can expand DEF into a lambda (possibly with some
    ;; variable bindings around it) then splice that lambda in
    ;; directly as the body of the function. (This is the same
    ;; function we use to resolve lambdas in `fbind'.)
    (if lambda
        (ematch lambda
          ((list* (and _ (eql 'lambda)) args body)
           `(let ,env
              (declare ,@decls)
              (uiop:defun* ,alias ,args
                ,@(unsplice docstring)
                ,@body))))
        (with-unique-names (temp)
          `(let ((,temp (ensure-function ,def)))
             (declare (type function ,temp))
             ;; Give the function a temporary definition at
             ;; compile time so the compiler doesn't complain
             ;; about its being undefined.
             (uiop:defun* ,alias (&rest args)
               (apply ,temp args))
             (setf (fdefinition ',alias) ,temp)
             ,@(unsplice
                (and docstring
                     `(setf (documentation ',alias 'function) ,docstring))))))))

;;;# Etc

(defmacro defplace (name args &body (form &optional docstring))
  "Define NAME and (SETF NAME) in one go.

Note that the body must be a single, setf-able expression."
  (with-gensyms (value)
    `(progn
       (defun ,name ,args
         ,@(unsplice docstring)
         ,form)
       (defun (setf ,name) (,value ,@args)
         (setf ,form ,value)))))

(defmacro defcondition (name supers &body (slots &rest options))
  "Alias for `define-condition'.

Like (define-condition ...), but blissfully conforming to the same
nomenclatural convention as every other definition form in Common
Lisp."
  `(define-condition ,name ,supers
     ,slots
     ,@options))

(defstruct
    (%read-only-struct
     (:constructor nil)                 ;Abstract.
     (:copier nil)
     (:predicate nil))
  "Abstract base class for read-only structures.")

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
;;; definition, there is nothing to prevent the superclass from being
;;; redefined in the future.

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
          `(defstruct (,name (:copier nil)
                             ;; Mark as OK to save in pure memory.
                             #+(or sbcl cmucl) (:pure t)
                             ,@opts
                             ,(read-only-include-clause include-clause))
             ,@(unsplice docstring)
             ,@(mapcar #'read-only-slotdef slots)))))))

(defmacro defvar-unbound (var &body (docstring))
  "Define VAR as if by `defvar' with no init form, and set DOCSTRING
as its documentation.

I believe the name comes from Edi Weitz."
  `(progn
     (defvar ,var)
     (setf (documentation ',var 'variable) ,docstring)
     ',var))

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
          (error 'print-not-readable
                 :stream stream
                 :object object))
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
                 collect (ematch slot
                           ((and slot-name (type symbol))
                            (list slot-name t))
                           ((list (and slot-name (type symbol))
                                  type)
                            (list slot-name type)))))
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
         (,type-name ,@slot-names))

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

       (trivia:defpattern ,type-name
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
