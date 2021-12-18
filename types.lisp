;;; Definitions of types.
(in-package :serapeum)

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

(deftype -> (args values)
  "The type of a function from ARGS to VALUES."
  `(function ,args ,values))

(defmacro -> (function args values)
  "Declaim the ftype of FUNCTION from ARGS to VALUES.

     (-> mod-fixnum+ (fixnum fixnum) fixnum)
     (defun mod-fixnum+ (x y) ...)"
  `(declaim (ftype (-> ,args ,values) ,function)))

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
     ;; `values' is hand-holding for SBCL.
     `(the ,type-spec (values (require-type ,form ',type-spec))))))

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

This is, is it true that SUBTYPE is a subtype of TYPE, but not the same type?"
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
  (let ((sorted (stable-sort subtypes #'proper-subtype-p)))
    (prog1 sorted
      ;; Subtypes must always precede supertypes.
      (assert
       (loop for (type1 . rest) on sorted
             never (loop for type2 in rest
                           thereis (proper-subtype-p type2 type1)))))))

(defun remove-shadowed-subtypes (subtypes)
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
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
  (let* ((types (simplify-subtypes types))
         (var-type (variable-type var env))
         ;; If the var has a declared type, remove excluded types.
         (types (remove-if-not (lambda (type)
                                 (subtypep type var-type env))
                               types)))
    (cond ((null types)
           `(locally ,@body))
          ((or (policy> env 'space 'speed)
               (policy> env 'compilation-speed 'speed))
           (simple-style-warning "Not using type dispatch due to optimize declarations.")
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
                                        ,@body)))))))))))

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

;; Cut and paste from babel-encodings. TODO: Think about if and how
;; this could be usefully exposed.
(progn

  (defmacro with-simple-vector (((v vector) (s start) (e end)) &body body)
    "If VECTOR is a displaced or adjustable array, binds V to the
underlying simple vector, adds an adequate offset to START and
END and binds those offset values to S and E.  Otherwise, if
VECTOR is already a simple array, it's simply bound to V with no
further changes.

START and END are unchecked and assumed to be within bounds.

Note that in some Lisps, a slow copying implementation is
necessary to obtain a simple vector thus V will be bound to a
copy of VECTOR coerced to a simple-vector.  Therefore, you
shouldn't attempt to modify V."
    #+sbcl
    `(sb-kernel:with-array-data ((,v ,vector) (,s ,start) (,e ,end))
       ,@body)
    #+(or cmu scl)
    `(lisp::with-array-data ((,v ,vector) (,s ,start) (,e ,end))
       ,@body)
    #+openmcl
    (with-unique-names (offset)
      `(multiple-value-bind (,v ,offset)
           (ccl::array-data-and-offset ,vector)
         (let ((,s (+ ,start ,offset))
               (,e (+ ,end ,offset)))
           ,@body)))
    #+allegro
    (with-unique-names (offset)
      `(excl::with-underlying-simple-vector (,vector ,v ,offset)
         (let ((,e (+ ,end ,offset))
               (,s (+ ,start ,offset)))
           ,@body)))
    ;; slow, copying implementation
    #-(or sbcl cmu scl openmcl allegro)
    (once-only (vector)
      `(funcall (if (adjustable-array-p ,vector)
                    #'call-with-array-data/copy
                    #'call-with-array-data/fast)
                ,vector ,start ,end
                (lambda (,v ,s ,e) ,@body))))

  (defun call-with-array-data/fast (vector start end fn)
    (multiple-value-bind (data offset)
        (undisplace-array vector)
      (funcall fn data (+ offset start) (+ offset end))))

  (defun call-with-array-data/copy (vector start end fn)
    (funcall fn (replace (make-array (- end start) :element-type
                                     (array-element-type vector))
                         vector :start2 start :end2 end)
             0 (- end start))))

;;; Are these worth exporting?

(defmacro with-boolean ((var) &body body)
  "Emit BODY twice: once for the case where VAR is true, once for the
  case where VAR is false.

This lets you write an algorithm naturally, testing the value of VAR
as much as you like -- perhaps even in each iteration of a loop --
knowing that VAR will only actually be tested once.

Around each specialized body VAR is bound to a symbol macro whose
value is `t' or `nil'. This allows macros to recognize VAR as a
constant."
  (check-type var symbol)
  `(if ,var
       (symbol-macrolet ((,var t))
         ,@body)
       (symbol-macrolet ((,var nil))
         ,@body)))

(defmacro with-nullable ((var type) &body body)
  `(with-type-dispatch (null ,type) ,var
     ,@body))

(defmacro with-test-fn ((test) &body body
                        &environment env)
  "Specialize BODY on the most common test functions."
  (check-type test symbol)
  (if (or (policy> env 'space 'speed)
          (policy> env 'compilation-speed 'speed))
      `(locally ,@body)
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
             (t (let ((,test (ensure-function ,test)))
                  (macrolet ((,test (x y) (list 'funcall ',test x y)))
                    ,@body))))))

(defmacro with-item-key-function ((key &optional (key-form key))
                                  &body body &environment env)
  "For each of the most common key functions used in sequences, emit a
copy of BODY with KEY bound to a local macro that calls KEY-FORM.

If current optimization declarations favor space over speed, or
compilation speed over runtime speed, then BODY is only emitted once."
  (check-type key symbol)
  `(let ((,key (canonicalize-key ,key-form)))
     ,@(sane-body-for-splice
        (if (or (policy> env 'space 'speed)
                (policy> env 'compilation-speed 'speed))
            `((macrolet ((,key (x) (list 'funcall ',key x)))
                ,@body))
            `((cond ((eql ,key #'identity)
                     (macrolet ((,key (x) x))
                       ,@body))
                    (t (macrolet ((,key (x) (list 'funcall ',key x)))
                         ,@body))))))))

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
                  ,(cond ((subtypep type 'cons)
                          `(satisfies proper-alist?))
                         ((subtypep 'null type)
                          '(satisfies proper-list?))
                         (t
                          '(satisfies proper-list-without-nil?)))))))

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
