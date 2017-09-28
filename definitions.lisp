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
- In implementations that support it (SBCL and CCL, at the moment) this
version creates a backing variable that is \"global\" or \"static\",
so there is not just a change in semantics, but also a gain in
efficiency.
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

(defmacro defalias (alias &body (def &optional docstring))
  "Define a value as a top-level function.

     (defalias string-gensym (compose #'gensym #'string))

Like (setf (fdefinition ALIAS) DEF), but with a place to put
documentation and some niceties to placate the compiler.

Name from Emacs Lisp."
  (with-unique-names (temp)
    `(progn
       (declaim (notinline ,alias))
       (let ((,temp (ensure-function ,def)))
         (declare (type function ,temp))
         ;; Give the function a temporary definition at compile time
         ;; so the compiler doesn't complain about its being
         ;; undefined.
         (defun ,alias (&rest args)
           (apply ,temp args))
         (setf (fdefinition ',alias) ,temp)
         ,@(unsplice
            (and docstring
                 `(setf (documentation ',alias 'function) ,docstring))))
       ',alias)))

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

`defstruct-read-only' is designed to stay as close to the syntax of
`defstruct' as possible. The idea is to make it easy to flag data as
immutable, whether in your own code or in code you are refactoring. In
your own code, however, you may sometimes prefer `defconstructor',
which is designed to facilitate working with immutable data."
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

;;; TODO Is this worth exporting?
(defgeneric constructor= (x y)
  (:method (x y)
    (equal x y)))

;;; How about this?
(defgeneric constructor-ref (x idx)
  (:method (x (idx integer))
    (error "Illegal index for ~a" x)))

(defgeneric constructor-len (x))

(defun print-constructor (object stream &rest fields)
  (declare (dynamic-extent fields))
  (when *print-escape*
    (write-string "#." stream))
  (write-char #\( stream)
  (prin1 (type-of object) stream)
  (dolist (field fields)
    (write-char #\Space stream)
    (prin1 (funcall field object) stream))
  (write-char #\) stream))

(defmacro defconstructor (class &body slots)
  (let* ((docstring
           (and (stringp (first slots))
                (pop slots)))
         (slots
           (loop for slot in slots
                 collect (ematch slot
                           ((and _ (type symbol))
                            (list slot t))
                           ((list (and _ (type symbol)) _)
                            slot))))
         (constructor class)
         (slot-names (mapcar #'first slots))
         (conc-name
           (symbolicate class '-))
         (readers
           (mapcar (curry #'symbolicate conc-name)
                   slot-names))
         (copier-name (symbolicate 'copy- class)))
    `(progn
       ;; Make sure the constructor is inlined. This is necessary on
       ;; SBCL to allow instances to be stack-allocated.
       (declaim (inline ,constructor))

       ;; Actually define the type.
       (defstruct-read-only
           (,class
            (:constructor ,constructor ,slot-names)
            (:conc-name ,conc-name)
            (:predicate nil)
            (:print-function
             (lambda (object stream depth)
               (declare (ignore depth))
               (print-constructor object stream
                                  ,@(loop for reader in readers
                                          collect `(function ,reader))))))
         ,@(unsplice docstring)
         ,@(loop for (slot-name slot-type) in slots
                 collect `(,slot-name :type ,slot-type)))

       ;; Freeze the type when possible.
       (declaim-freeze-type ,class)

       ;; Define the copier.
       (defun ,copier-name
           (,class &key
                     ,@(loop for (slot-name nil) in slots
                             for reader in readers
                             collect `(,slot-name (,reader ,class))))
         (,class ,@slot-names))

       (defmethod constructor-len ((x ,class))
         ,(length readers))

       ;; Define a comparison method.
       (defmethod constructor= ((o1 ,class) (o2 ,class))
         (and ,@(loop for reader in readers
                      collect `(constructor= (,reader o1)
                                             (,reader o2)))))

       ,@(loop for i from 0
               for reader in readers
               collect `(defmethod constructor-ref ((x ,class) (idx (eql ,i)))
                          (,reader x)))

       (trivia:defpattern ,class ,slot-names
         (list
          'and
          (list 'type ',class)
          ,@(loop for reader in readers
                  for name in slot-names
                  collect `(list 'trivia:access '',reader ,name))))
       ',class)))
