(in-package :serapeum)
(in-readtable :fare-quasiquote)

;;; * Functions

(defun eqs (x)
  "A predicate for equality (under EQ) with X."
  (lambda (y) (eq x y)))

(define-compiler-macro eqs (x)
  (once-only (x)
    `(lambda (y) (eq ,x y))))

(defun eqls (x)
  "A predicate for equality (under EQL) with X."
  (lambda (y) (eql x y)))

(define-compiler-macro eqls (x)
  (once-only (x)
    `(lambda (y) (eql ,x y))))

(defun equals (x)
  "A predicate for equality (under EQUAL) with X."
  (lambda (y)
    (equal y x)))

(define-compiler-macro equals (x)
  (once-only (x)
    `(lambda (y) (equal ,x y))))

(defmacro define-train (name args &body body)
  "Define a function that takes only a fixed number of functions as arguments and returns another functions.

Also define a compiler macro that inlines the resulting lambda
expression, so compilers can eliminate it.

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

(defun flip (f)
  "Flip around the arguments of a binary function.

That is, given a binary function, return another, equivalent function
that takes its two arguments in the opposite order.

From Haskell."
  (let ((f (ensure-function f)))
    (lambda (x y)
      (funcall f y x))))

(define-compiler-macro flip (fn)
  (rebinding-functions (fn)
    `(lambda (x y)
       (funcall ,fn y x))))

(defun nth-arg (n)
  "Return a function that returns only its NTH argument, ignoring all others.

If you've ever caught yourself trying to do something like

    (mapcar #'second xs ys)

then `nth-arg` is what you need.

If `hash-table-keys` were not already defined by Alexandria, you could
define it thus:

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

(defun distinct (&key (key #'identity)
                      (test 'equal))
  "Return a function that echoes only values it has not seen before.

    (defalias test (distinct))
    (test 'foo) => foo, t
    (test 'foo) => nil, nil

The second value is T when the value is distinct.

TEST must be a valid test for a hash table.

This has many uses, for example:

    (count-if (distinct) seq)
    ≡ (length (remove-duplicates seq))"
  (check-type test ok-hash-table-test)
  (let ((dict (make-hash-table :test test))
        (key (ensure-function key)))
    (lambda (arg)
      (if (nth-value 1 (gethash arg dict))
          (values nil nil)
          (values (setf (gethash arg dict)
                        (funcall key arg))
                  t)))))

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

(defun once (fn)
  "Return a function that runs FN only once, caching the results
forever."
  (let ((cache '(nil))
        (first-run t))
    (lambda (&rest args)
      (if (not first-run)
          (values-list cache)
          (setf first-run nil
                cache (multiple-value-list (apply fn args)))))))

(defun juxt (&rest fns)
  "Clojure's `juxt'.

Return a function of one argument, which, in turn, returns a list
where each element is the result of applying one of FNS to the
argument.

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
bound to their values at the time the closure was created. These
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
  (if (null symbols)
      fn
      (let ((values (mapcar #'symbol-value symbols)))
        (lambda (&rest args)
          (declare (dynamic-extent args))
          (progv symbols values
            (multiple-value-prog1
                (apply fn args)
              (map-into values #'symbol-value symbols)))))))

;;; See http://www.jsoftware.com/papers/fork.htm.

(define-train hook (f g)
  "Monadic hook.
From J.

AKA Schoenfinkel's S combinator."
  `(lambda (y)
     (funcall ,f y (funcall ,g y))))

(define-train fork (g f h)
  "Monadic fork.
From J."
  `(lambda (y)
     (funcall ,g
              (funcall ,f y)
              (funcall ,h y))))

(define-train hook2 (f g)
  "Dyadic hook.
From J."
  `(lambda (x y)
     (funcall ,f x
              (funcall ,g y))))

(define-train fork2 (g f h)
  "Dyadic fork.
From J."
  `(lambda (x y)
     (funcall ,g
              (funcall ,f x y)
              (funcall ,h x y))))

(define-train capped-fork (g f h)
  "J's capped fork (monadic)."
  f                                     ;Ignore f.
  `(lambda (y)
     (funcall ,g (funcall ,h y))))

(define-train capped-fork2 (g f h)
  "J's capped fork (dyadic)."
  f                                     ;Ignore f.
  `(lambda (x y)
     (funcall ,g (funcall ,h x y))))
