(in-package :serapeum)

(defsubst make (class &rest initargs &key &allow-other-keys)
  "Shorthand for `make-instance'.
After Eulisp."
  (declare (dynamic-extent initargs))
  (apply #'make-instance class initargs))

(define-compiler-macro make (class &rest initargs &key &allow-other-keys)
  (when (constantp class)
    (unless (typep (eval class) '(or class symbol))
      (warn "~s cannot designate a class" class)))
  `(make-instance ,class ,@initargs))

(defun class-name-safe (x)
  "The class name of the class of X.
If X is a class, the name of the class itself."
  (if (typep x 'class)
      (class-name x)
      (class-name (class-of x))))

(defun find-class-safe (x &optional env)
  "The class designated by X.
If X is a class, it designates itself."
  (typecase x
    (class x)
    (symbol (find-class x nil env))
    (t nil)))



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
organized, without any loss of power."
  (multiple-value-bind (body decls) (parse-body body)
    (multiple-value-bind (slot-decls decls) (partition-declarations slots decls)
      `(macrolet ((:method (name &body body)
                    (let* ((class ',class)
                           (self ',self)
                           (slots ',slots)
                           (qualifier (when (not (listp (car body))) (pop body)))
                           (args (pop body))
                           (docstring (when (stringp (car body)) (pop body)))
                           (args-with-self (substitute (list self class) self args)))
                      (when (equal args-with-self args)
                        (warn "No binding for ~s in ~s" self args))
                      `(symbol-macrolet ,(loop for slot in slots
                                               ;; Same as with-slots, use
                                               ;; (x y) alias slot Y to
                                               ;; var X.
                                               for alias = (if (listp slot) (first slot) slot)
                                               for slot-name = (if (listp slot) (second slot) slot)
                                               collect `(,alias (slot-value ,self ',slot-name)))
                         ,@',slot-decls
                         (defmethod ,name ,@(unsplice qualifier) ,args-with-self
                           ,@(unsplice docstring)
                           ,@body)))))
         ,@decls
         ,@body))))
