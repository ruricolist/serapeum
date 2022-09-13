(in-package :serapeum)

(deftype subclass-union (class-name)
  "Return a disjunction of all the direct subtypes of CLASS.
This can be useful for exhaustiveness checking over the direct
subtypes of a class.

This should not be used for classes meant to be subclassed by library
users."
  (let ((class (find-class class-name t)))
    `(or ,@(mapcar #'class-name (c2mop:class-direct-subclasses class)))))

(eval-always
  (defstruct (struct-to-try-instantiating
              (:constructor nil)
              (:copier nil)
              (:predicate nil))))

(deftype object ()
  ;; Test (once at load time, not every time the type is expanded) if
  ;; this Lisp supports make-instance with structure classes.
  (if (load-time-value
       (ignore-errors (make-instance 'struct-to-try-instantiating)))
      '(or structure-object standard-object)
      'standard-object))

(-> make ((or class symbol) &rest t &key &allow-other-keys)
  (values object &optional))
(defsubst make (class &rest initargs &key &allow-other-keys)
  "Shorthand for `make-instance'.
Unlike `make-instance', this is not a generic function, so it can do
more compile-time argument checking.

Also unlike `make-instance', `make' is defined to always return a
single value. It also declares its return type (as `standard-object',
or also `structure-object' if the implementation allows
`make-instance' on structures). This may allow the compiler to warn
you if you (e.g.) try to treat the return value as a list or number.

After Eulisp."
  (declare (type (or class symbol) class)
           (dynamic-extent initargs))
  (values (apply #'make-instance class initargs)))

(define-compiler-macro make (class &rest initargs &key &allow-other-keys
                                   &environment env)
  (multiple-value-bind (real-class constant?)
      (eval-if-constant class env)
    (when constant?
      (unless (typep real-class '(or class symbol))
        (warn "~s cannot designate a class" class))))
  `(the object (values (make-instance ,class ,@initargs))))

(defpattern make (class &rest initargs)
  (ematch class
    ((list 'quote class-name)
     `(,class-name ,@initargs))))

(-> class-name-of (t) (values symbol &optional))
(defun class-name-of (x)
  "The class name of the class of X."
  (assure symbol (class-name (class-of x))))

(-> class-name-safe (t) (values symbol &optional))
(defun class-name-safe (x)
  "The class name of the class of X.
If X is a class, the name of the class itself."
  (if (typep x 'class)
      (values (assure symbol (class-name x)))
      (class-name-of x)))

(-> find-class-safe ((or symbol class) &optional t)
    (values (or class null) &optional))
(defun find-class-safe (x &optional env)
  "The class designated by X.
If X is a class, it designates itself."
  (typecase x
    (class x)
    (symbol (find-class x nil env))
    (t nil)))

(declaim (inline slot-value-safe))
(-> slot-value-safe (t symbol &optional t)
    (values t boolean boolean &optional))
(defun slot-value-safe (instance slot-name &optional default)
  "Like `slot-value', but doesn't signal errors.
Returns three values:
1. The slot's value (or nil),
2. A boolean that is T if the slot exists and *was* bound,
3. A boolean that is T if the slot exists.

Note that this function does call `slot-value' \(if the slot exists),
so if there is a method on `slot-unbound' for the class it will be
invoked. In this case the second value will still be `nil', however."
  (declare (symbol slot-name))
  (if (not (slot-exists-p instance slot-name))
      (values default nil nil)
      (let ((boundp (and (slot-boundp instance slot-name) t)))
        (handler-case
            (values (slot-value instance slot-name)
                    boundp
                    t)
          (unbound-slot ()
            (values default nil t))))))

(defsetf slot-value-safe (instance slot-name &optional default) (value)
  (declare (ignore default))
  `(setf (slot-value ,instance ,slot-name) ,value))



(defmacro defmethods (class (self . slots) &body body)
  "Concisely define methods that specialize on the same class.

You can already use `defgeneric' to define an arbitrary number of
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

Similarly, you can use `defmethods' to define methods that specialize
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

Note in particular that `self' can appear in any position, and that
you can freely specialize the other arguments.

Just as in `with-slots', slots can be renamed:

    (defmethods my-class (self (abscissa x) (ordinate y))
      ...)

You can also use `defmethods' in place of `with-accessors', by using a
function-quote:

    (defmethods my-class (self (x #'my-class-x)
                               (y #'my-class-y))
      ...)

\(The difference from using `with-slots' is the scope of the slot
bindings: they are established *outside* of the method definition,
which means argument bindings shadow slot bindings:

    (some-method \"foo\" (make 'my-class :x \"bar\"))
    => \"foo\"

Since slot bindings are lexically outside the argument bindings, this
is surely correct, even if it makes `defmethods' slightly harder to
explain in terms of simpler constructs.)

Is `defmethods' trivial? Yes, in terms of its implementation. This
docstring is far longer than the code it documents. But you may find
it does a lot to keep heavily object-oriented code readable and
organized, without any loss of power.

Note that `defmethods' may also be useful when converting state
machines written using `labels' into an object-oriented style.

This construct is very loosely inspired by impl blocks in Rust."
  (mvlet* ((slot-names slot-binds
            (loop for slot in slots
                  if (listp slot)
                    collect slot into slot-binds
                    and collect (first slot) into slot-names
                  else
                    collect slot into slot-names
                    and collect (list slot slot) into slot-binds
                  finally (return (values slot-names slot-binds))))
           (body decls
            (parse-body body))
           (slot-decls decls
            (partition-declarations slot-names decls)))
    `(macrolet ((:method (name &body args)
                  (mvlet* ((class ',class)
                           (self ',self)
                           (slot-binds ',slot-binds)
                           (qualifiers lambda-list body
                            (parse-defmethod-args args))
                           (body decls docstring (parse-body body :documentation t))
                           (ll-with-self (substitute (list self class) self lambda-list)))
                    (when (equal ll-with-self lambda-list)
                      (error "No binding for ~s in ~s" self lambda-list))
                    `(symbol-macrolet ,(loop for (alias ref) in slot-binds
                                             collect (ematch ref
                                                       ((and ref (type symbol))
                                                        `(,alias (slot-value ,self ',ref)))
                                                       ((list 'function fn)
                                                        `(,alias (,fn ,self)))))
                       ,@',slot-decls
                       (defmethod ,name ,@qualifiers ,ll-with-self
                         ,@(unsplice docstring)
                         ,@decls
                         ,@body)))))
       ,@decls
       ,@body)))
