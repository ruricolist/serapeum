(in-package :serapeum)

;;; * Functions

(defun eqs (x)
  "Return a one-argument function that tests if its argument is `eq' to X."
  (lambda (y) (eq x y)))

(define-compiler-macro eqs (x)
  (once-only (x)
    `(lambda (y) (eq ,x y))))

(defun eqls (x)
  "Return a one-argument function that tests if its argument is `eql' to X."
  (lambda (y) (eql x y)))

(define-compiler-macro eqls (x)
  (once-only (x)
    `(lambda (y) (eql ,x y))))

(defun equals (x)
  "Return a one-argument function that tests if its argument is `equal' to X."
  (lambda (y)
    (equal y x)))

(define-compiler-macro equals (x)
  (once-only (x)
    `(lambda (y) (equal ,x y))))

(-> partial (function &rest t) function)
(defun partial (fn &rest args)
  "Partial application.

Unlike `alexandria:curry', which is only inlined when you ask it to
be, `partial' is always inlined if possible.

From Clojure."
  ;; Allow Lisp to use the compiler macro.
  (declare (inline curry))
  ;; `values' keeps SBCL from complaining that the type is too
  ;; complex.
  (values
   ;; For most actual uses of partial, the compiler macro for curry
   ;; will take care of compiling the function efficiently. However,
   ;; we can improve the worst case by explicitly handling the most
   ;; common cases -- cases with small numbers of arguments.
   (match args
     (() (ensure-function fn))
     ((list arg) (curry fn arg))
     ((list arg1 arg2) (curry fn arg1 arg2))
     ((list arg1 arg2 arg3) (curry fn arg1 arg2 arg3))
     (otherwise (apply #'curry fn args)))))

(define-compiler-macro partial (fn &rest args)
  (if (null args)
      `(ensure-function ,fn)
      ;; The declaration is needed to make sure the compiler macro
      ;; actually gets used.
      `(locally (declare (inline curry))
         (curry ,fn ,@args))))

(-> trampoline (function &rest t) (not function))
(defun trampoline (fn &rest args)
  "Use the trampoline technique to simulate mutually recursive functions.

Call FN with supplied ARGS, if any.

If FN returns a functions, call that function with no arguments.
Repeat until the return value is not a function, and finally return
that non-function value.

Note that, to return a function as a final value, you must wrap it in
some data structure and unpack it.

Most likely to be useful for Lisp implementations that do not provide
tail call elimination.

From Clojure."
  (if args
      (trampoline (apply fn args))
      (nlet trampoline ((fn fn))
        (if (functionp fn)
            (trampoline (funcall fn))
            fn))))

(defmacro define-train (name args &body body)
  "Define a higher-order function and its compiler macro at once.

When defining a higher-order function it is often a good idea to
write a compiler macro so compilers can inline the resulting lambda
form.

For the special case of a fixed-arity function that only takes other
functions as arguments, you can use `define-train' to define the
function and the compiler macro in one go. The catch is that you have
to write the single definition as a macro.

E.g., if `complement' did not exist, you could define it like so:

    (define-train complement (fn)
      `(lambda (&rest args)
         (not (apply ,fn args))))

Besides providing an implicit compiler macro, `define-train' also
inserts the proper declarations to ensure the compiler recognizes the
function arguments as functions, avoiding runtime type checks.

The term \"train\" is from J."
  (multiple-value-bind (body decls docstring)
      (parse-body body :documentation t)
    `(progn
       (define-compiler-macro ,name ,args
         ,@decls
         (rebinding-functions ,args
           ,@body))
       (defun ,name ,args
         ,@(unsplice docstring)
         (macrolet ((,name ,args
                      ,@decls
                      (rebinding-functions ,args
                        ,@body)))
           (,name ,@args))))))

;;; It would of course be possible to define `flip' to be variadic,
;;; but the binary case can be handled more efficiently, and I have
;;; not seen any other uses for it.

(define-train flip (f)
  "Flip around the arguments of a binary function.

That is, given a binary function, return another, equivalent function
that takes its two arguments in the opposite order.

From Haskell."
  `(lambda (x y)
     (funcall ,f y x)))

(defun nth-arg (n)
  "Return a function that returns only its NTH argument, ignoring all others.

If you've ever caught yourself trying to do something like

    (mapcar #'second xs ys)

then `nth-arg' is what you need.

If `hash-table-keys' were not already defined by Alexandria, you could
define it as:

    (defun hash-table-keys (table)
      (maphash-return (nth-arg 0) table))"
  (lambda (&rest args)
    (declare (dynamic-extent args))
    (nth n args)))

(define-compiler-macro nth-arg (n)
  (let ((leading (loop repeat n collect (gensym))))
    (with-gensyms (arg rest)
      `(lambda (,@leading ,arg &rest ,rest)
         (declare (ignore ,@leading ,rest))
         ,arg))))

(defconst +alist-breakeven+ 25)

(defun distinct (&key (key #'identity)
                      (test 'equal)
                      (synchronized nil))
  "Return a closure returning only values it has not seen before.

    (defalias test (distinct))
    (test 'foo) => foo, t
    (test 'foo) => nil, nil

The second value is T when the value is distinct.

TEST must be a valid test for a hash table.

This has many uses, for example:

    (count-if (distinct) seq)
    ≡ (length (remove-duplicates seq))

If SYNCHRONIZED is non-nil, then `distinct' can safely be used from
multiple threads. Otherwise it is not thread-safe.

Note the closure returned by `distinct' changes how it tracks unique
items based on the number of items it is tracking, so it is suitable
for all sizes of set."
  (unless (hash-table-test-p test)
    (error "Not a hash table test: ~a" test))
  (let ((set '())
        (set-len 0)
        (dict nil)
        (test (ensure-function test)))
    (declare ((integer 0 #.+alist-breakeven+) set-len))
    (labels ((dict-init ()
               (set-hash-table (shiftf set nil) :test test :strict nil))
             (distinct ()
               (with-item-key-function (key)
                 (lambda (arg)
                   (let ((key (key arg)))
                     ;; Swap the representation based on the number of items
                     ;; being tracked.
                     (if (< set-len +alist-breakeven+)
                         (if (member key set :test test)
                             (values nil nil)
                             (progn
                               (push key set)
                               (incf set-len)
                               (values arg t)))
                         (let ((dict (or dict (setf dict (dict-init)))))
                           (declare (hash-table dict))
                           (if (nth-value 1 (gethash key dict))
                               (values nil nil)
                               (progn
                                 (setf (gethash key dict) t)
                                 (values arg t))))))))))
      (declare (dynamic-extent #'dict-init))
      (let ((distinct (distinct)))
        (if (not synchronized) distinct
            (let ((lock (bt:make-lock)))
              (lambda (arg)
                (bt:with-lock-held (lock)
                  (funcall distinct arg)))))))))

(defun throttle (fn wait &key synchronized memoized)
  "Wrap FN so it can be called no more than every WAIT seconds.
If FN was called less than WAIT seconds ago, return the values from the
last call. Otherwise, call FN normally and update the cached values.

WAIT, of course, may be a fractional number of seconds.

The throttled function is not thread-safe by default; use SYNCHRONIZED
to get a version with a lock.

You can pass MEMOIZED if you want the function to remember values
between calls."
  (labels ((ready? (last)
             (<= (- wait (- (get-universal-time) last)) 0))
           (throttle/simple (fn)
             (let ((last 0)
                   (cache '(nil))
                   (fn (ensure-function fn)))
               (lambda (&rest args)
                 (when (ready? last)
                   (setf last (get-universal-time)
                         cache (multiple-value-list (apply fn args))))
                 (values-list cache))))
           (cleanup-cache (cache)
             (maphash (lambda (k v)
                        (when (ready? (car v))
                          (remhash k cache)))
                      cache))
           (throttle/memoized (fn)
             (let ((cache (make-hash-table :test 'equal)))
               (lambda (&rest args)
                 (cleanup-cache cache)
                 (destructuring-bind (last . values)
                     (gethash args cache '(0 nil))
                   (if (not (ready? last))
                       (values-list values)
                       (values-list
                        (cdr
                         (setf (gethash args cache)
                               (cons (get-universal-time)
                                     (multiple-value-list (apply fn args)))))))))))
           (throttle/synchronized (fn)
             (let ((thunk (throttle fn wait :synchronized nil :memoized memoized)))
               (let ((lock (bt:make-lock)))
                 (lambda (&rest args)
                   (bt:with-lock-held (lock)
                     (apply thunk args)))))))
    (if synchronized
        (throttle/synchronized fn)
        (if memoized
            (throttle/memoized fn)
            (throttle/simple fn)))))

(define-train once (fn)
  "Return a function that runs FN only once, caching the results
forever."
  `(let ((cache '())
         (first-run t))
     (lambda (&rest args)
       (block nil
         (tagbody
            (when (null first-run)
              (go :not-first-run))
          :first-run
            (setf first-run nil
                  cache (multiple-value-list (apply ,fn args)))
          :not-first-run
            (return (values-list cache)))))))

(defun fuel (level)
  "Return a function to count 'fuel' consumption down from the initial level.

The function takes one argument and subtracts its value from the
current fuel level.

The two return values are a boolean indicating whether the available
fuel has been exceeded followed by the current fuel level (which may
be negative.)"
  (lambda (consumption)
    (let ((old-level level)
          (remaining (decf level consumption)))
      ;; Signal an error in the special case when LEVEL is a large
      ;; float, CONSUMPTION is a small float, and subtraction does
      ;; nothing.
      (when (and (floatp consumption)
                 (not (zerop consumption))
                 (= old-level level))
        (error "Fuel not consumed: level ~a, consumption ~a"
               level consumption))
      (values (>= remaining 0) remaining))))

(defun juxt (&rest fns)
  "Clojure's `juxt'.

Return a function which returns a list where each element is the
result of applying one of FNS to the arguments.

It’s actually quite simple, but easier to demonstrate than to explain.
The classic example is to use `juxt` to implement `partition`:

    (defalias partition* (juxt #'filter #'remove-if))
    (partition* #'evenp '(1 2 3 4 5 6 7 8 9 10))
    => '((2 4 6 8 10) (1 3 5 7 9))

The general idea is that `juxt` takes things apart."
  (lambda (&rest args)
    (declare (dynamic-extent args))
    (loop for fn in fns
          collect (apply fn args))))

(define-compiler-macro juxt (&rest fns)
  (let ((gs (loop for nil in fns collect (gensym "FN"))))
    (with-gensyms (args)
      `(let ,(loop for g in gs
                   for fn in fns
                   collect `(,g (ensure-function ,fn)))
         (lambda (&rest ,args)
           (declare (dynamic-extent ,args))
           (list ,@(loop for g in gs collect `(apply ,g ,args))))))))

(defun dynamic-closure (symbols fn)
  "Create a dynamic closure.

Some ancient Lisps had closures without lexical binding. Instead, you
could \"close over\" pieces of the current dynamic environment. When
the resulting closure was called, the symbols closed over would be
bound to their storage at the time the closure was created. These
bindings would persist through subsequent invocations and could be
mutated. The result was something between a closure and a
continuation.

This particular piece of Lisp history is worth reviving, I think, if
only for use with threads. For example, to start a thread and
propagate the current value of `*standard-output*':

     (bt:make-thread (dynamic-closure '(*standard-output*) (lambda ...)))
     = (let ((temp *standard-output*))
         (bt:make-thread
          (lambda ...
            (let ((*standard-output* temp))
              ...))))"
  (check-type fn function)
  (let ((symbols (remove-if-not #'boundp symbols)))
    (if (null symbols) fn
        (let ((storage (map 'vector #'symbol-value symbols)))
          (lambda (&rest args)
            (declare (dynamic-extent args))
            (progv symbols (coerce storage 'list)
              (multiple-value-prog1 (apply fn args)
                (map-into storage #'symbol-value symbols))))))))

;;; See http://www.jsoftware.com/papers/fork.htm.

(define-train hook (f g)
  "Monadic hook.
From J.

The hook of f is defined as f(y,g(y)).

For example, you can use a hook to test whether a number is an
integer, by asking whether it is equal to its own floor.

    (hook #'= #'floor)
    (funcall * 2.0)
    => T

AKA Schoenfinkel's S combinator."
  `(lambda (y)
     (funcall ,f y (funcall ,g y))))

(define-train fork (g f h)
  "Monadic fork.

The monadic fork of f, g, and h is defined as

   (f g h) y <-> (f y) g (h y)

The usual example of a monadic fork is defining the mean. Assuming a
`sum' function defined as

   (defun sum (xs)
    (reduce #'+ xs))

you can write a (numerically unstable) `mean' using `fork'.

    (fork #'/ #'sum #'length)
    (funcall * '(1.0 2.0 3.0 4.0))
    => 2.5

From J."
  `(lambda (y)
     (funcall ,g
              (funcall ,f y)
              (funcall ,h y))))

(define-train hook2 (f g)
  "Dyadic hook.

The usual (only?) example of a dyadic hook is an `hour' function that
takes an hour and a count of minutes and returns a fractional count of
hours.

    (hook2 #'+ (partial (flip #'/) 60))
    (funcall * 3.0 15.0)
    => 3.25

From J."
  `(lambda (x y)
     (funcall ,f x
              (funcall ,g y))))

(define-train fork2 (g f h)
  "Dyadic fork.

The dyadic fork of f, g, and h is defined as:

    x (f g h) y <-> (x f y) g (x h y)

For example, say you wanted a \"plus or minus\" operator. Given
numbers x and y, it returns a list of x+y and x-y. This can easily be
written as a dyadic fork.

    (fork2 #'list #'+ #'-)
    (funcall * 10 2)
    => '(12 8)

From J."
  `(lambda (x y)
     (funcall ,g
              (funcall ,f x y)
              (funcall ,h x y))))

(define-train capped-fork (g h)
  "J's capped fork (monadic).

Like a monadic fork, but F is omitted.

Effectively the composition of G and H."
  `(lambda (y)
     (funcall ,g (funcall ,h y))))

(define-train capped-fork2 (g h)
  "J's capped fork (dyadic).

Like a dyadic fork, but F is omitted."
  `(lambda (x y)
     (funcall ,g (funcall ,h x y))))

(defun fnil (fn &rest defaults)
  "Return a function that ORs its arguments with DEFAULTS.

If the first argument is nil, then the first default in DEFAULTS is
used instead; if the second argument is nil, then the second default
in DEFAULTS is used instead; and so on until we run out of DEFAULTS.

The minimum arity is equal to the length of DEFAULTS.

This has a compiler macro for reasonable efficiency.

From Clojure."
  (declare (optimize (debug 0)))
  (ensuring-functions (fn)
    (lambda (&rest args)
      (multiple-value-call fn
        (values-list
         (loop for default in defaults
               for arg = (pop args)
               collect (or default args)))
        (values-list args)))))

(define-compiler-macro fnil (fn &rest defaults)
  (when (null defaults)
    (return-from fnil `(progn ,fn)))
  (with-unique-names (rest gfn)
    (let ((temps (make-gensym-list (length defaults) 'temp))
          (args (make-gensym-list (length defaults) 'arg)))
      `(let (,@(mapcar #'list temps defaults)
             (,gfn (ensure-function ,fn)))
         (lambda (,@args &rest ,rest)
           (apply ,gfn
                  ,@(loop for arg in args
                          for temp in temps
                          collect `(or ,arg ,temp))
                  ,rest))))))

;;; TODO It would be better if these had shorter names.

(define-train variadic->unary (fn)
  "Return a function that takes a single argument, a list, and
applies FN to it.

Practically equivalent to `(curry #'apply FN arguments...)'."
  (with-unique-names (list)
    `(lambda (,list)
       (declare (list ,list))
       (apply ,fn ,list))))

(define-train unary->variadic (fn)
  "Return a function that takes any number of arguments and calls FN
on them as a list.

Wraps a function that expects a single argument, a list, so it can be
used variadically."
  (with-unique-names (args)
    `(lambda (&rest ,args)
       (funcall ,fn ,args))))

(defun mvconstantly (&rest values)
  "Like `constantly', but returns all of VALUES as multiple values.
If there are not VALUES, returns nothing."
  (cond ((null values)
         (lambda (&rest args)
           (declare (ignore args))
           (values)))
        ((null (cdr values))
         (constantly (car values)))
        (t
         (lambda (&rest args)
           (declare (ignore args))
           (values-list values)))))

(define-compiler-macro mvconstantly (&rest values)
  (cond ((null values)
         `(lambda (&rest args)
            (declare (ignore args))
            (values)))
        ((null (cdr values))
         `(constantly ,(car values)))
        (t
         #+cmucl `(constantly ,@values)
         #-cmucl
         (let ((temps (make-gensym-list (length values))))
           `(let ,(mapcar #'list temps values)
              (lambda (&rest args)
                (declare (ignore args))
                (values ,@temps)))))))


(define-compiler-macro do-nothing (&rest args)
  `(progn ,@args (values)))

(-> do-nothing (&rest t) (values &optional))
(declaim (inline do-nothing))
(defun do-nothing (&rest args)
  "Do nothing and return nothing.
This function is meant as a placeholder for a function argument.

From LispWorks."
  (declare (ignore args))
  (values))

(-> repeat-until-stable ((or symbol function) t
                         &key (:test (or symbol function))
                         (:max-depth (or null (integer 0))))
    (values t (or null (integer 0))))
(defun repeat-until-stable (fn x &key (test 'eql) max-depth)
  "Takes a single-argument FN and calls (fn x), then (fn (fn x)), and so on
until the result doesn't change according to TEST. If MAX-DEPTH is specified
then FN will be called at most MAX-DEPTH times even if the result is still changing.

Returns two values, the stable result of FN and the remainder of
MAX-DEPTH. \(If this value is 0, the result is unstable.)"
  (declare ((or symbol function) fn test)
           ((or (integer 0) null) max-depth))
  (let ((fn (ensure-function fn)))
    (with-two-arg-test (test)
      (with-boolean (max-depth)
        (nlet repeat-until-stable ((x x)
                                   (max-depth max-depth))
          (if (eql 0 max-depth)
              (values x max-depth)
              (let ((next (funcall fn x)))
                (if (funcall test next x)
                    (values x max-depth)
                    (repeat-until-stable next
                                         (boolean-when max-depth
                                           (1- max-depth)))))))))))
