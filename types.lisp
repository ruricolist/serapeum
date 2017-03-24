(in-package :serapeum)

(deftype wholenum ()
  "A whole number. Equivalent to `(integer 0 *)'."
  '(integer 0 *))

(deftype tuple (&rest types)
  "A proper list where each element has the same type as the corresponding element in TYPES.

    (typep '(1 :x #\c) '(tuple integer keyword character)) => T

As a shortcut, a quoted form among TYPES is expanded to an `eql' type specifier.
    (tuple 'function symbol)
    ≡ (tuple (eql function) symbol)

The same shortcut works for keywords.
    (tuple :name symbol)
    ≡ (tuple (eql :name) symbol)"
  (reduce (lambda (x y)
            (match x
              ((or (list 'quote form)
                   (and form (type keyword)))
               (setf x `(eql ,form))))
            `(cons ,x ,y))
          types
          :from-end t
          :initial-value 'null))

(deftype -> (args values)
  "The type of a function from ARGS to VALUES."
  `(function ,args ,values))

(defmacro -> (function args values)
  "Declaim the ftype of FUNCTION from ARGS to VALUES.

     (-> mod-fixnum+ (fixnum fixnum) fixnum)
     (defun mod-fixnum+ (x y) ...)"
  `(declaim (ftype (-> ,args ,values) ,function)))

(defmacro declaim-freeze-type (type)
  "Declare that TYPE is not going to change, for the benefit of Lisps
  that understand such declarations."
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
  (declare (optimize (debug 0)))
  (if (typep datum spec)
      datum
      (%require-type datum spec)))

(define-compiler-macro require-type (&whole call datum spec)
  (if (constantp spec)
      (once-only (datum)
        `(if (typep ,datum ,spec)
             ,datum
             (%require-type ,datum ,spec)))
      call))

(defun %require-type (datum spec)
  (declare (optimize (debug 0)))
  (let ((new (wrong-type datum spec use-value
               "Supply a value to use instead")))
    (require-type new spec)))

(defun require-type-for (datum spec place)
  (declare (optimize (debug 0)))
  (if (typep datum spec)
      datum
      (%require-type-for datum spec place)))

(define-compiler-macro require-type-for (&whole call datum spec place)
  (if (constantp spec)
      (once-only (datum)
        `(if (typep ,datum ,spec)
             ,datum
             (%require-type-for ,datum ,spec ,place)))
      call))

(defun %require-type-for (datum spec place)
  (let ((new (wrong-type datum spec store-value
               (lambda (s) (format s "Supply a new value for ~s" place)))))
    (require-type-for new spec place)))

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

From ISLISP."
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
  `(the ,type-spec (values (require-type ,form ',type-spec))))

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

(defun simplify-subtypes (subtypes)
  (let* ((unique (remove-duplicated-subtypes subtypes))
         (sorted (sort-subtypes unique))
         (unshadowed (remove-shadowed-subtypes sorted)))
    unshadowed))

(defun remove-duplicated-subtypes (subtypes)
  (remove-duplicates subtypes :test #'type=))

(defun proper-subtypep (subtype type)
  (and (subtypep subtype type)
       (not (subtypep type subtype))))

(defun sort-subtypes (subtypes)
  (let ((sorted (stable-sort subtypes #'proper-subtypep)))
    (prog1 sorted
      ;; Subtypes must always precede supertypes.
      (assert
       (loop for (type1 . rest) on sorted
             never (loop for type2 in rest
                           thereis (proper-subtypep type2 type1)))))))

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

(defmacro with-templated-body (&environment env (var expr)
                                            (&key ((:type overtype) (required-argument :type))
                                                  (subtypes (required-argument :subtypes))
                                                  in-subtypes)
                               &body body)
  "A macro that emits BODY once for each subtype in SUBTYPES.

Suppose you are writing a function that takes a string or a number. On
the one hand, you want the function to be generic, and work with any
kind of string or number. On the other hand, you know Lisp can produce
more efficient code for certain subtypes.

The ideal would be to be able to write the code generically, but still
have Lisp compile \"fast paths\" for subtypes it can handle more
efficiently. E.g. `fixnum' instead of `integer', or `(simple-array
character (*))' instead of `string'.

You could write code to do this by hand, but there would be pitfalls.
One is that how a type is divided up can vary between Lisps, resulting
in spurious warnings. Another is code bloat -- the naive way of
handling templating, by repeating the same code inline, drastically
increases the size of the disassembly.

The idea of `with-templated-body' is to provide a high-level way to
ask for this kind of compilation. It checks that SUBTYPES are really
subtypes of TYPE; it telescopes duplicated subtypes; it eliminates the
default case if the subtypes are exhaustive; and it arranges for each
specialization of BODY to be called out-of-line. It also permits
supplying declarations to be used in the specializations, but not in
the default case.

Note that `with-templated-body' is intended to be used around
relatively expensive code, particularly loops. For simpler code, the
gains from specialized compilation may not justify the overhead of
type dispatch and a (local) function call.

This is not a macro that lends itself to trivial examples. If you want
to understand how to use it, the best idea is to look at how it is
used elsewhere in Serapeum."
  (let* ((subtypes
           ;; Remove duplicate and shadowed types.
           (simplify-subtypes subtypes))
         (subtypes-exhaustive?
           (type= `(or ,@subtypes) overtype)))
    (assert (every (lambda (type)
                     (subtypep type overtype env))
                   subtypes))
    `(let ((,var ,expr))
       ;; The idea here is that the local functions will be
       ;; lambda-lifted by the Lisp compiler, thus saving space, while
       ;; any actual closures can be made dynamic-extent.
       ,(let* ((fns (make-gensym-list (length subtypes) 'template-fn-))
               (default? (not subtypes-exhaustive?))
               (default (and default? (gensym (string 'default))))
               (qfns
                 (append (loop for fn in fns
                               collect `(function ,fn))
                         (and default? `((function ,default))))))
          `(flet (,@(loop for fn in fns
                          for type in subtypes
                          collect `(,fn (,var)
                                        (declare (type ,type ,var))
                                        ,in-subtypes
                                        ,@body))
                  ,@(unsplice
                        (and default?
                         `(,default (,var)
                           (declare (type ,overtype ,var))
                           ,@body))))
             (declare (notinline ,@fns ,@(unsplice default)))
             (declare (dynamic-extent ,@qfns))
             ;; Give Lisp permission to ignore functions if it can
             ;; infer a type for EXPR.
             (declare (ignorable ,@qfns))
             (etypecase ,var
               ,@(loop for type in subtypes
                       for fn in fns
                       collect `(,type (,fn ,var)))
               ,@(unsplice
                  (unless subtypes-exhaustive?
                    `(,overtype (,default ,var))))))))))
