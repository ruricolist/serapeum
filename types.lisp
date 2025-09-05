(defpackage :serapeum/types
  (:documentation "Utility types and type utilities.")
  #+sb-package-locks (:lock t)
  (:use
   :cl
   :alexandria
   :serapeum/macro-tools
   :trivia)
  (:import-from
   :introspect-environment
   :constant-form-value
   :typexpand)
  (:import-from
   :serapeum/macro-tools
   :policy-quality
   :speed-matters?
   :variable-type)
  (:import-from
   :serapeum/portability
   :with-simple-vector)
  (:export
   :*boolean-bypass*
   :->
   :assure
   :assuref
   :boolean-if
   :boolean-unless
   :boolean-when
   :input-stream
   :nor
   :octet
   :octet-vector
   :output-stream
   :proper-subtype-p
   :proper-supertype-p
   :soft-alist-of
   :soft-list-of
   :supertypep
   :true
   :tuple
   :vref
   :wholenum
   :with-boolean
   :with-item-key-function
   :with-member-test
   :with-simple-vector-dispatch
   :with-string-dispatch
   :with-subtype-dispatch
   :with-two-arg-test
   :with-type-dispatch
   :with-vector-dispatch))

(in-package :serapeum/types)

(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-vector (&optional length)
  "An array of octets."
  `(simple-array octet (,length)))

(deftype input-stream ()
  '(and stream (satisfies input-stream-p)))

(deftype output-stream ()
  '(and stream (satisfies output-stream-p)))

(deftype wholenum ()
  "A whole number. Equivalent to `(integer 0 *)'."
  '(integer 0 *))

(deftype nor (&rest types)
  `(not (or ,@types)))

(defpattern nor (&rest patterns)
  `(not (or ,@patterns)))

(deftype tuple (&rest types)
  "A proper list where each element has the same type as the corresponding element in TYPES.

    (typep '(1 :x #\c) '(tuple integer keyword character)) => T

As a shortcut, a quoted form among TYPES is expanded to an `eql' type specifier.
    (tuple 'function symbol)
    ≡ (tuple (eql function) symbol)

Literal keywords, numbers, and characters are also treated as `eql' type specifiers.
    (tuple :name 1 #\a symbol)
    ≡ (tuple (eql :name) (eql 1) (eql #\a) symbol)"
  (reduce (lambda (x y)
            (match x
              ((or (list 'quote form)
                   (and form (type (or keyword number character))))
               (setf x `(eql ,form))))
            `(cons ,x ,y))
          types
          :from-end t
          :initial-value 'null))

(defpattern tuple (&rest args)
  `(list ,@args))

(deftype -> (args &optional values)
  "The type of a function from ARGS to VALUES."
  `(function ,args ,@(when values
                       (list values))))

(defmacro -> (functions (&rest args) &optional values)
  "Declaim the ftype of one or multiple FUNCTIONS from ARGS to VALUES.

     (-> mod-fixnum+ (fixnum fixnum) fixnum)
     (defun mod-fixnum+ (x y) ...)

     (-> (mod-float+ mod-single-float+) (float float) float)
     (defun mod-float+ (x y) ...)
     (defun mode-single-float+ (x y) ...)"
  `(declaim (ftype (-> (,@args) ,@(when values
                                    (list values)))
		   ,@(if (consp functions)
			 (if (find-symbol (symbol-name (first functions))
					  :cl)
			     (list functions)
			     functions)
			 (list functions)))))

(defmacro declaim-freeze-type (&rest types)
  "Declare that TYPES is not going to change.

On Lisps that understand it, this is roughly equivalent to \"sealing\"
a type in an OOP language: a promise that the type will never have any
new subtypes, so tests for the type can be open-coded."
  `(progn
     ,@(loop for type in types
             collect `(declaim-freeze-type-1 ,type))))

(defmacro declaim-freeze-type-1 (type)
  (declare (ignorable type))
  #+sbcl  `(declaim (sb-ext:freeze-type ,type))
  #+cmucl `(declaim (ext:freeze-type ,type)))

(defmacro declaim-constant-function (&rest fns)
  "Declare that FNs are constant functions, for the benefit of Lisps
that understand such declarations."
  (declare (ignorable fns))
  #+cmucl
  `(progn
     ,@(loop for fn in fns
             collect `(declaim (ext:constant-function ,fn)))))

(defmacro truly-the (type &body (expr))
  #+sbcl `(sb-ext:truly-the ,type ,expr)
  #+cmucl `(ext:truly-the ,type ,expr)
  #-(or sbcl cmucl) `(the ,type ,expr))

(declaim (notinline %require-type %require-type-for))

(defun read-new-value ()
  "Read and evaluate a value."
  (format *query-io* "~&New value: ")
  (list (eval (read *query-io*))))

(defmacro wrong-type (datum type restart &body (report))
  `(restart-case
       (error 'type-error
              :datum ,datum
              :expected-type ,type)
     (,restart (new)
       :report ,report
       :interactive read-new-value
       new)))

(defun require-type (datum spec)
  #.+merge-tail-calls+
  (if (typep datum spec)
      datum
      (%require-type datum spec)))

(define-compiler-macro require-type (&whole call datum spec)
  (if (constantp spec)
      (let ((type (eval spec)))
        (once-only (datum)
          `(if (typep ,datum ,spec)
               ,datum
               (truly-the ,type
                 (%require-type ,datum ,spec)))))
      call))

(defun %require-type (datum spec)
  #.+merge-tail-calls+
  (let ((new (wrong-type datum spec use-value
               "Supply a value to use instead")))
    (require-type new spec)))

(defun require-type-for (datum spec place)
  #.+merge-tail-calls+
  (if (typep datum spec)
      datum
      (%require-type-for datum spec place)))

(define-compiler-macro require-type-for (&whole call datum spec place)
  (if (constantp spec)
      (let ((type (eval spec)))
        (once-only (datum)
          `(if (typep ,datum ,spec)
               ,datum
               (truly-the ,type
                 (%require-type-for ,datum ,spec ,place)))))
      call))

(defun %require-type-for (datum spec place)
  #.+merge-tail-calls+
  (let ((new (wrong-type datum spec store-value
               (lambda (s) (format s "Supply a new value for ~s" place)))))
    (require-type-for new spec place)))

(deftype assure (type-spec)
  type-spec)

(defpattern assure (type binding)
  `(and ,binding (type ,type)))

(defmacro assure (type-spec &body (form) &environment env)
  "Macro for inline type checking.

`assure' is to `the' as `check-type' is to `declare'.

     (the string 1)    => undefined
     (assure string 1) => error

The value returned from the `assure' form is guaranteed to satisfy
TYPE-SPEC. If FORM does not return a value of that type, then a
correctable error is signaled. You can supply a value of the correct
type with the `use-value' restart.

Note that the supplied value is *not* saved into the place designated
by FORM. (But see `assuref'.)

Using `values' types is supported, with caveats:
- The types of `&rest' arguments are enforced using `soft-list-of'.
- Types defined with `deftype' that expand into values types may not be checked in some Lisps.

From ISLISP."
  (match (typexpand type-spec env)
    ((list* 'values typespecs)
     `(assure-values ,typespecs ,form))
    (otherwise
     ;; The type nil contains nothing, so it renders the form
     ;; meaningless.
     (assert (not (subtypep type-spec nil)))
     (let ((exp (macroexpand form env)))
       ;; A constant expression.
       (when (constantp exp)
         (let ((val (constant-form-value exp)))
           (unless (typep val type-spec)
             (warn "Constant expression ~s is not of type ~a"
                   form type-spec))))
       ;; A variable.
       (when (symbolp exp)
         (let ((declared-type (variable-type exp env)))
           (unless (subtypep type-spec declared-type)
             (warn "Required type ~a is not a subtypep of declared type ~a"
                   type-spec declared-type)))))
     #+sbcl
     ;; For SBCL, create a temporary binding so SBCL will print a
     ;; warning if it can infer the declared type is wrong.
     (with-unique-names (assure-temp)
       `(let ((,assure-temp ,form))
          (declare (type ,type-spec ,assure-temp))
          ;; `values' is hand-holding for SBCL.
          (the ,type-spec (values (require-type ,assure-temp ',type-spec)))))
     #-sbcl
     `(the ,type-spec (require-type ,form ',type-spec)))))

(defmacro assure-values (typespecs &body (form))
  (flet ((lambda-list-keyword? (x)
           (member x lambda-list-keywords)))
    (let* ((types (remove-if #'lambda-list-keyword? typespecs))
           (lambda-list
             (loop for spec in typespecs
                   if (lambda-list-keyword? spec)
                     collect spec
                   else collect (gensym))))
      (multiple-value-bind (required optional rest)
          (parse-ordinary-lambda-list lambda-list)
        (let ((optional
                (loop for (arg default nil) in optional
                      for supplied? = (gensym)
                      collect `(,arg ,default ,supplied?))))
          `(multiple-value-call
               (lambda (,@required
                   ,@(and optional `(&optional ,@optional))
                   ,@(and rest `(&rest ,rest)))
                 (multiple-value-call #'values
                   ,@(loop for arg in required
                           for type in types
                           collect `(assure ,type ,arg))
                   ,@(let ((types (nthcdr (length required) types)))
                       (loop for (arg nil arg-supplied?) in optional
                             for type in types
                             collect `(if ,arg-supplied?
                                          (assure ,type ,arg)
                                          (values))))
                   ,@(and rest
                          `((values-list
                             (assure (soft-list-of ,(lastcar types))
                               ,rest))))))
             ,form))))))

(defmacro assuref (place type-spec)
  "Like `(progn (check-type PLACE TYPE-SPEC) PLACE)`, but evaluates
PLACE only once."
  (with-gensyms (temp)
    (let ((ts type-spec))
      `(the ,ts
            (values
             (let ((,temp ,place))
               (if (typep ,temp ',ts)
                   ,temp
                   (setf ,place (require-type-for ,temp ',ts ',place)))))))))

;;; These are helpful for development.
(progn
  (defmacro variable-type-in-env (&environment env var)
    `(values ',(variable-type var env)
             ;; So it's not unused.
             ,var))

  (defmacro policy-quality-in-env (&environment env qual)
    `',(policy-quality qual env)))

(defun simplify-subtypes (subtypes)
  (let* ((unique (remove-duplicated-subtypes subtypes))
         (sorted (sort-subtypes unique))
         (unshadowed (remove-shadowed-subtypes sorted)))
    unshadowed))

(defun remove-duplicated-subtypes (subtypes)
  (remove-duplicates subtypes :test #'type=))

(-> supertypep (t t &optional t) (values boolean boolean))
(defun supertypep (supertype type &optional env)
  "Is SUPERTYPE a supertype of TYPE?
That is, is TYPE a subtype of SUPERTYPE?"
  (subtypep type supertype env))

(-> proper-subtype-p (t t &optional t) (values boolean boolean))
(defun proper-subtype-p (subtype type &optional env)
  "Is SUBTYPE a proper subtype of TYPE?

This is, is it true that SUBTYPE is a subtype of TYPE, but not the
same type?"
  ;; You might expect this would be as simple as

  ;; (and (subtypep subtype type)
  ;;      (not (subtypep type subtype)))

  ;; but the implementation must be more complicated to give a
  ;; meaningful answer for the second value: "are you sure?".
  (multiple-value-bind (subtype? valid1)
      (subtypep subtype type env)
    (if (not subtype?)
        (values nil valid1)
        (multiple-value-bind (same? valid2)
            (subtypep type subtype env)
          (if (not same?)
              ;; The second value is always true when the first value
              ;; is true.
              (values t t)
              (values nil (and valid1 valid2)))))))

(-> proper-supertype-p (t t &optional t) (values boolean boolean))
(defun proper-supertype-p (supertype type &optional env)
  "Is SUPERTYPE a proper supertype of TYPE?

That is, is it true that every value of TYPE is also of type
SUPERTYPE, but not every value of SUPERTYPE is of type TYPE?"
  (proper-subtype-p type supertype env))

(defun sort-subtypes (subtypes)
  "Sort SUBTYPES such that subtypes always precede supertypes."
  (let ((sorted (stable-sort subtypes #'proper-subtype-p)))
    (prog1 sorted
      ;; Subtypes must always precede supertypes.
      (assert
       (loop for (type1 . rest) on sorted
             never (loop for type2 in rest
                           thereis (proper-subtype-p type2 type1)))))))

(defun remove-shadowed-subtypes (subtypes)
  "Remove shadowed types in SUBTYPES.
Subtypes are shadowed when they are subtypes of the disjunction of all
preceding types."
  (assert (equal subtypes (sort-subtypes subtypes)))
  (labels ((rec (subtypes supertypes)
             (if (null subtypes)
                 (nreverse supertypes)
                 (let ((type (first subtypes))
                       (supertype (cons 'or supertypes)))
                   (if (type= type supertype)
                       ;; Type is shadowed, ignore it.
                       (rec (cdr subtypes) supertypes)
                       (rec (cdr subtypes)
                            (cons type supertypes)))))))
    (rec subtypes nil)))

(defun subtypes-exhaustive? (type subtypes &optional env)
  "Does the disjunction of SUBTYPES exhaust TYPE?
SUBTYPES must all be subtypes of TYPE."
  (loop for subtype in subtypes
        unless (subtypep subtype type env)
          do (error "~s is not a subtype of ~s" subtype type))
  (type= type `(or ,@subtypes)))

(defparameter *vref-by-type*
  (stable-sort
   (list '(simple-bit-vector . sbit)
         '(bit-vector . bit)
         '(string . char)
         '(simple-string . schar)
         '(simple-vector . svref)
         '(t . aref))
   #'proper-subtype-p
   :key #'car))

(defun type-vref (type)
  (let ((sym (cdr (assoc type *vref-by-type* :test #'subtypep))))
    (assert (and (symbolp sym) (not (null sym))))
    sym))

(defmacro vref (vec index &environment env)
  "When used globally, same as `aref'.

Inside of a with-type-dispatch form, calls to `vref' may be bound to
different accessors, such as `char' or `schar', or `bit' or `sbit',
depending on the type being specialized on."
  (if (symbolp vec)
      (let* ((type (variable-type vec env))
             (vref (type-vref type)))
        `(,vref ,vec ,index))
      `(aref ,vec ,index)))

(defmacro with-vref (type &body body)
  ;; Although this macro is only intended for internal use, package
  ;; lock violations can still occur when functions it is used in are
  ;; inlined.
  (let ((vref (type-vref type)))
    (if (eql vref 'aref)
        `(progn ,@body)
        `(locally (declare #+sbcl (sb-ext:disable-package-locks vref))
           (macrolet ((vref (v i) (list ',vref v i)))
             (declare #+sbcl (sb-ext:enable-package-locks vref))
             ,@body)))))

(defmacro with-type-declarations-trusted (&environment env (&key) &body body)
  ;; The way to do this in SBCL and CMUCL is to use truly-the.
  (case uiop:*implementation-type*
    (:ccl
     ;; Try to force CCL to trust our declarations. According to
     ;; <https://trac.clozure.com/ccl/wiki/DeclareOptimize>, that
     ;; requires safety<3 and speed>=safety. But, if the
     ;; pre-existing values for safety and speed are acceptable,
     ;; we don't want to overwrite them.
     (multiple-value-bind (speed safety)
         (let ((speed  (policy-quality 'speed env))
               (safety (policy-quality 'safety env)))
           (if (and (< safety 3)
                    (>= speed safety))
               (values speed safety)
               (let* ((safety (min safety 2))
                      (speed  (max speed safety)))
                 (values speed safety))))
       (assert (and (< safety 3)
                    (>= speed safety)))
       `(locally
            (declare
             (optimize (speed ,speed)
                       (safety ,safety)))
          ,@body)))
    (:ecl
     ;; According to
     ;; <https://common-lisp.net/project/ecl/static/manual/ch02.html>,
     ;; ECL only trusts type declarations (and inference) when safety
     ;; <= 1.
     (let* ((current-safety (policy-quality 'safety env))
            (capped-safety (min current-safety 1)))
       `(locally (declare (optimize (safety ,capped-safety)))
          ,@body)))
    ;; If you know how to make a particular Lisp trust type
    ;; declarations, feel free to make a pull request, or open an
    ;; issue.
    (t `(progn ,@body))))

(defmacro with-type-dispatch ((&rest types) var &body body
                              &environment env)
  "A macro for writing fast sequence functions (among other things).

In the simplest case, this macro produces one copy of BODY for each
type in TYPES, with the appropriate declarations to induce your Lisp
to optimize that version of BODY for the appropriate type.

Say VAR is a string. With this macro, you can trivially emit optimized
code for the different kinds of string that VAR might be. And
then (ideally) instead of getting code that dispatches on the type of
VAR every time you call `aref', you get code that dispatches on the
type of VAR once, and then uses the appropriately specialized
accessors. (But see `with-string-dispatch'.)

But that's the simplest case. Using `with-type-dispatch' also provides
*transparent portability*. It examines TYPES to deduplicate types that
are not distinct on the current Lisp, or that are shadowed by other
provided types. And the expansion strategy may differ from Lisp to
Lisp: ideally, you should not have to pay for good performance on
Lisps with type inference with pointless code bloat on other Lisps.

There is an additional benefit for vector types. Around each version
of BODY, the definition of `vref' is shadowed to expand into an
appropriate accessor. E.g., within a version of BODY where VAR is
known to be a `simple-string', `vref' expands into `schar'.

Using `vref' instead of `aref' is obviously useful on Lisps that do
not do type inference, but even on Lisps with type inference it can
speed compilation times (compiling `aref' is relatively slow on SBCL).

Within `with-type-dispatch', VAR should be regarded as read-only.

Note that `with-type-dispatch' is intended to be used around
relatively expensive code, particularly loops. For simpler code, the
gains from specialized compilation may not justify the overhead of the
initial dispatch and the increased code size.

Note also that `with-type-dispatch' is relatively low level. You may
want to use one of the other macros in the same family, such as
`with-subtype-dispatch', `with-string-dispatch', or so forth.

The design and implementation of `with-type-dispatch' is based on a
few sources. It replaces a similar macro formerly included in
Serapeum, `with-templated-body'. One possible expansion is based on
the `string-dispatch' macro used internally in SBCL. But most of the
credit should go to the paper \"Fast, Maintable, and Portable Sequence
Functions\", by Irène Durand and Robert Strandh."
  `(locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
     ,(let* ((types (simplify-subtypes types))
             (var-type (variable-type var env))
             ;; If the var has a declared type, remove excluded types.
             (types (remove-if-not (lambda (type)
                                     (subtypep type var-type env))
                                   types)))
        (cond ((null types)
               `(locally ,@body))
              ((not (speed-matters? env))
               `(locally ,@body))
              ;; The advantage of the CMUCL/SBCL way (I hope) is that the
              ;; compiler can decide /not/ to bother inlining if the type
              ;; is such that it cannot do any meaningful optimization.
              ((or #+(or cmucl sbcl) t)
               ;; Cf. sb-impl::string-dispatch.
               (with-unique-names ((fun type-dispatch-fun))
                 `(flet ((,fun (,var)
                           (with-read-only-vars (,var)
                             ,@body)))
                    (declare (inline ,fun))
                    (etypecase ,var
                      ,@(loop for type in types
                              collect `(,type (,fun (truly-the ,type ,var))))))))
              ((or #+ccl t)
               `(with-type-declarations-trusted ()
                  (etypecase ,var
                    ,@(loop for type in types
                            collect `(,type
                                      (locally (declare (type ,type ,var))
                                        (with-read-only-vars (,var)
                                          (with-vref ,type
                                            ,@body))))))))
              (t
               `(with-type-declarations-trusted ()
                  (etypecase ,var
                    ,@(loop for type in types
                            collect `(,type
                                      (let ((,var ,var))
                                        (declare (type ,type ,var))
                                        (with-read-only-vars (,var)
                                          (with-vref ,type
                                            ,@body))))))))))))

(defmacro with-subtype-dispatch (type (&rest subtypes) var &body body
                                 &environment env)
  "Like `with-type-dispatch', but SUBTYPES must be subtypes of TYPE.

Furthermore, if SUBTYPES are not exhaustive, an extra clause will be
added to ensure that TYPE itself is handled."
  (let* ((types
           (if (subtypes-exhaustive? type subtypes env)
               subtypes
               (append subtypes (list type)))))
    `(with-type-dispatch ,types ,var
       ,@body)))

(defmacro with-string-dispatch ((&rest types) var &body body)
  "Like `with-subtype-dispatch' with an overall type of `string'."
  `(with-subtype-dispatch string
       ;; Always specialize for (simple-array character (*)).
       ((simple-array character (*))
        (simple-array base-char (*))
        ,@types)
       ,var
     ,@body))

(defmacro with-vector-dispatch ((&rest types) var &body body)
  "Like `with-subtype-dispatch' with an overall type of `vector'."
  ;; Always specialize for simple vectors.
  `(with-subtype-dispatch vector (simple-vector ,@types) ,var
     ,@body))

(defmacro with-simple-vector-dispatch ((&rest types)
                                       (var
                                        start
                                        end)
                                       &body body)
  "Like `with-vector-dispatch' but on implementations that support it, the underlying simple vector of a displaced array is first dereferenced, so the type is guaranteed to be a subtype of simple-array (but not actually `simple-vector`).

START and END are the offset of the original vector's data in the array it is displaced to."
  (let ((inner
          `(with-subtype-dispatch (simple-array * (*))
               ;; Always specialize for simple vectors.
               (simple-vector ,@types) ,var
             ,@body)))
    (if (or #+(or sbcl cmu scl openmcl allegro) t)
        `(with-simple-vector ((,var ,var)
                              (,start 0)
                              (,end (length ,var)))
           ,inner)
        `(let* ((,var ,var)
                (,start 0)
                (,end (length ,var)))
           ,inner))))

(defvar *boolean-bypass* nil
  "Bypasses macroexpand-time branching of WITH-BOOLEAN. The bypass inhibits all
macroexpand-time branching and instead defers all checks in expanded code to
runtime in the following manner:
\
* WITH-BOOLEAN -> PROGN
* BOOLEAN-IF -> IF
* BOOLEAN-WHEN -> WHEN
* BOOLEAN-UNLESS -> UNLESS")

(define-symbol-macro %in-branching% nil)

(define-symbol-macro %all-branches% ())

(defmacro with-boolean ((&rest branches) &body body
                        &environment env)
  "Establishes a lexical environment in which it is possible to use
macroexpand-time branching. Within the lexical scope of
`with-boolean', it is possible to use `boolean-if', `boolean-when',
and `boolean-unless' to conditionalize whether some forms are included
at compilation time. (You may also use `:if', `:when', or `:unless'
for brevity.)
\
The first argument must be a list of symbols which name variables. This macro
will expand into a series of conditionals"
  (cond (*boolean-bypass*
         `(progn ,@body))
        (t (let ((all-branches (macroexpand-1 '%all-branches% env)))
             `(macrolet ((:if (cond then else)
                              (list 'boolean-if cond then else))
                         (:when (cond &body then)
                           (list* 'boolean-when cond then))
                         (:unless (cond &body then)
                           (list* 'boolean-unless cond then)))
                (locally
                    (declare #+sbcl (sb-ext:disable-package-locks
                                     %in-branching% %all-branches%)
                             #+sbcl (sb-ext:muffle-conditions
                                     sb-ext:code-deletion-note))
                  (symbol-macrolet ((%in-branching% t)
                                    (%all-branches% (,@branches ,@all-branches))
                                    (%true-branches% ()))
                    (locally (declare #+sbcl (sb-ext:enable-package-locks
                                              %in-branching% %all-branches%))
                      (%with-boolean ,branches ,@body)))))))))

(defmacro %with-boolean ((&rest branches) &body body
                         &environment env)
  (cond ((not (null branches))
         (destructuring-bind (branch . other-branches) branches
           (check-type branch symbol)
           (let ((true-branches (macroexpand-1 '%true-branches% env)))
             `(if ,branch
                  (symbol-macrolet
                      ((%true-branches% (,branch . ,true-branches)))
                    (%with-boolean (,@other-branches)
                      ,@body))
                  (%with-boolean (,@other-branches)
                    ,@body)))))
        ((= 0 (length body))
         `(progn))
        ((= 1 (length body))
         (car body))
        (t `(progn ,@body))))

(defun conditional-error (name)
  `(simple-program-error
    "~A must be used inside the lexical scope established by ~
     WITH-BOOLEAN."
    ',name))

(defun missing-branch (name)
  `(simple-program-error
    "The macroexpand-time branch ~S was not defined in any encloding
     WITH-BOOLEAN form."
    ',name))

(defmacro boolean-if (branch then &optional else &environment env)
  "Chooses between the forms to include based on whether a macroexpand-time
branch is true. The first argument must be a symbol naming a branch in the
lexically enclosing WITH-BOOLEAN form.
\
It is an error to use this macro outside the lexical environment established by
WITH-BOOLEAN."
  (cond (*boolean-bypass*
         `(if ,branch ,then ,else))
        ((not (member branch (macroexpand-1 '%all-branches% env)))
         (missing-branch branch))
        ((not (macroexpand-1 '%in-branching% env))
         (conditional-error 'if))
        ((member branch (macroexpand-1 '%true-branches% env))
         then)
        (t (or else `(progn)))))

(defmacro boolean-when (branch &body body &environment env)
  "Includes some forms based on whether a macroexpand-time branch is true. The
first argument must be a symbol naming a branch in the lexically enclosing
WITH-BOOLEAN form.
\
It is an error to use this macro outside the lexical environment established by
WITH-BOOLEAN."
  (cond (*boolean-bypass*
         `(when ,branch ,@body))
        ((not (member branch (macroexpand-1 '%all-branches% env)))
         (missing-branch branch))
        ((not (macroexpand-1 '%in-branching% env))
         (conditional-error 'when))
        ((not (member branch (macroexpand-1 '%true-branches% env)))
         `(progn))
        ((= 0 (length body))
         `(progn))
        ((= 1 (length body))
         (car body))
        (t `(progn ,@body))))

(defmacro boolean-unless (branch &body body &environment env)
  "Includes some forms based on whether a macroexpand-time branch is false. The
first argument must be a symbol naming a branch in the lexically enclosing
WITH-BOOLEAN form.
\
It is an error to use this macro outside the lexical environment established by
WITH-BOOLEAN."
  (cond (*boolean-bypass*
         `(unless ,branch ,@body))
        ((not (member branch (macroexpand-1 '%all-branches% env)))
         (missing-branch branch))
        ((not (macroexpand-1 '%in-branching% env))
         (conditional-error 'unless))
        ((member branch (macroexpand-1 '%true-branches% env))
         `(progn))
        ((= 0 (length body))
         `(progn))
        ((= 1 (length body))
         (car body))
        (t `(progn ,@body))))

(defmacro with-nullable ((var type) &body body)
  `(with-type-dispatch (null ,type) ,var
     ,@body))

(defmacro with-two-arg-test ((test) &body body
                             &environment env)
  "Specialize BODY on the most common two-arg test functions."
  (check-type test symbol)
  (let ((default
          `(let ((,test (ensure-function ,test)))
             (macrolet ((,test (x y) (list 'funcall ',test x y)))
               ,@body))))
    (if (not (speed-matters? env)) default
        `(cond ((eql ,test #'eq)
                (macrolet ((,test (x y) `(eq ,x ,y)))
                  ,@body))
               ((eql ,test #'eql)
                (macrolet ((,test (x y) `(eql ,x ,y)))
                  ,@body))
               ((eql ,test #'equal)
                (macrolet ((,test (x y) `(equal ,x ,y)))
                  ,@body))
               ;; ((eql ,test #'equalp)
               ;;  (macrolet ((,test (x y) `(equalp ,x ,y)))
               ;;    ,@body))
               (t ,default)))))

(defmacro with-member-test ((test-fn &key key test test-not) &body body
                            &environment env)
  "Emit BODY multiple times with specialized, inline versions of
`member' bound to TEST-FN."
  (if (not (speed-matters? env))
      (with-unique-names (ukey utest utest-not)
        `(let ((,ukey ,key)
               (,utest ,test)
               (,utest-not ,test-not))
           (macrolet ((,test-fn (item list)
                        (list 'multiple-value-call
                              '(function member)
                              item list
                              :key ',ukey
                              (list 'if ',utest
                                    '(values :test ',utest)
                                    '(values))
                              (list 'if ',utest-not
                                    '(values :test-not ',utest-not)
                                    '(values)))))
             ,@body)))
      (let ((test? test)
            (test (or test (gensym (string 'test))))
            (test-not? test-not)
            (test-not (or test-not (gensym (string 'test-not))))
            (key? key)
            (key (or key (gensym (string 'key)))))
        `(let ((,test (canonicalize-test
                       ,(and test? test)
                       ,(and test-not? test-not)))
               (,key ,(and key? key)))
           (with-item-key-function (,key)
             (with-two-arg-test (,test)
               (macrolet ((,test-fn (x l)
                            (let ((test ',test)
                                  (key ',key))
                              (with-unique-names (ul ux mem)
                                `(let ((,ul ,l)
                                       (,ux ,x))
                                   (declare (optimize (safety 0) (debug 0))
                                            (list ,ul))
                                   (block ,mem
                                     (tagbody loop
                                        (when ,ul
                                          (unless (,test ,ux (,key (first ,ul)))
                                            (setf ,ul (cdr ,ul))
                                            (go loop)))
                                        (return-from ,mem ,ul))))))))
                 ,@body)))))))

(defmacro with-item-key-function ((key &optional (key-form key))
                                  &body body &environment env)
  "For each of the most common key functions used in sequences, emit a
copy of BODY with KEY bound to a local macro that calls KEY-FORM.

If current optimization declarations favor space over speed, or
compilation speed over runtime speed, then BODY is only emitted once."
  (check-type key symbol)
  (with-unique-names (ukey)
    `(let* ((,key (canonicalize-key ,key-form))
            (,ukey ,key))
       ,@(expect-form-list
          (if (not (speed-matters? env))
              `((macrolet ((,key (x) (list 'funcall ',ukey x)))
                  ,@body))
              `((cond ((eql ,ukey #'identity)
                       (macrolet ((,key (x) x))
                         ,@body))
                      (t (macrolet ((,key (x) (list 'funcall ',ukey x)))
                           ,@body)))))))))

(deftype true () '(eql t))
(deftype false () '(eql nil))

(declaim (ftype (function (t) boolean) true))
(declaim (inline true))
(defun true (x)
  "Coerce X to a boolean.
That is, if X is null, return `nil'; otherwise return `t'.

Based on an idea by Eric Naggum."
  (not (null x)))

(define-compiler-macro true (x)
  `(not (null ,x)))

;;; Should this be exported?
(declaim (type (integer 0 *) *soft-list-cutoff*))
(defparameter *soft-list-cutoff* 20)

(deftype soft-list-of (type)
  "A soft constraint for the elements of a list.

The elements are restricted only as far as is practical, which is not
very far, using heuristics which will not be specified here because
they may improve over time. That said, since the goal of this type is
to be practically useful, it will avoid any checks that would be O(n)
in the length of the list."
  `(or null
       (cons ,type
             (and list
                  ,(if (subtypep type 'cons)
                       `(satisfies proper-alist?)
                       (multiple-value-bind (allows-null? for-sure?)
                           (subtypep 'null type)
                         (if (and (not allows-null?)
                                  for-sure?)
                             '(satisfies proper-list-without-nil?)
                             '(satisfies proper-list?))))))))

(deftype soft-alist-of (key-type value-type)
  "A soft constraint for the elements of an alist.

Equivalent to `(soft-list-of (cons KEY-TYPE VALUE-TYPE))`."
  `(soft-list-of (cons ,key-type ,value-type)))

(declaim (ftype (function (t &optional (integer 0 *))
                          (values boolean &optional))
                proper-list? proper-list-without-nil?))

(defun proper-list? (x &optional (cutoff *soft-list-cutoff*))
  (or (null x)
      (and (consp x)
           (loop for tail on x
                 repeat cutoff
                 do (typecase tail
                      (null)
                      (cons)
                      ;; Improper list.
                      (t (return nil)))
                 finally (return t)))))

(defun proper-alist? (x &optional (cutoff *soft-list-cutoff*))
  (or (null x)
      (and (consp x)
           (loop for tail on x
                 repeat cutoff
                 do (typecase tail
                      (null)
                      (cons
                       (unless (consp (car tail))
                         (return nil)))
                      ;; Improper list.
                      (t (return nil)))
                 finally (return t)))))

(defun proper-list-without-nil? (x &optional (cutoff *soft-list-cutoff*))
  (or (null x)
      (and (consp x)
           (loop for tail on x
                 repeat cutoff
                 do (typecase tail
                      ;; Done.
                      (null)
                      ;; Contains null.
                      (cons
                       (when (null (car tail))
                         (return nil)))
                      ;; Improper list.
                      (t (return nil)))
                 finally (return t)))))

(defmacro seq-dispatch (seq &body (list-form array-form &optional other-form))
  "Efficiently dispatch on the type of SEQ."
  (declare (ignorable other-form))
  (let* ((list-form
           `(with-read-only-vars (,seq)
              ,list-form))
         (array-form
           `(with-read-only-vars (,seq)
              ,array-form))
         (list-form
           `(let ((,seq (truly-the list ,seq)))
              (declare (ignorable ,seq))
              ,list-form))
         (vector-form
           ;; Create a separate branch for simple vectors.
           `(if (simple-vector-p ,seq)
                (let ((,seq (truly-the simple-vector ,seq)))
                  (declare (ignorable ,seq))
                  (with-vref simple-vector
                    ,array-form))
                (let ((,seq (truly-the vector ,seq)))
                  (declare (ignorable ,seq))
                  ,array-form))))
    #+ccl `(ccl::seq-dispatch ,seq ,list-form ,vector-form)
    ;; Only SBCL and ABCL support extensible sequences right now.
    #+(or sbcl abcl)
    (once-only (seq)
      `(if (listp ,seq)
           ,list-form
           ,(if other-form
                `(if (arrayp ,seq)
                     ,vector-form
                     ,other-form)
                ;; Duplicate the array form so that, hopefully, `elt'
                ;; will be compiled to `aref', &c.
                `(if (arrayp ,seq)
                     ,vector-form
                     ,array-form))))
    #-(or sbcl abcl ccl)
    `(if (listp ,seq) ,list-form ,vector-form)))

(defmacro vector-dispatch (vec &body (bit-vector-form vector-form))
  "Efficiently dispatch on the type of VEC.
The first form provides special handling for bit vectors. The second
form provides generic handling for all types of vectors."
  `(cond ((typep ,vec 'simple-bit-vector)
          (let ((,vec (truly-the simple-bit-vector ,vec)))
            (declare (ignorable ,vec))
            (with-vref simple-bit-vector
              ,bit-vector-form)))
         ((typep ,vec 'bit-vector)
          (let ((,vec (truly-the bit-vector ,vec)))
            (declare (ignorable ,vec))
            (with-vref bit-vector
              ,bit-vector-form)))
         ;; Omitted so we can safely nest within with-vector-dispatch.
         ;; ((typep ,vec 'simple-vector)
         ;;  (let ((,vec (truly-the simple-vector ,vec)))
         ;;    (declare (ignorable ,vec))
         ;;    (with-vref simple-vector
         ;;      ,vector-form)))
         (t
          (let ((,vec (truly-the vector ,vec)))
            (declare (ignorable ,vec))
            ,vector-form))))
