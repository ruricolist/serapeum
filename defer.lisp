(in-package :serapeum)

(deftype extent-condition ()
  '(member :exit :success :failure))

(declaim (inline make-extent))
(defstruct guarded-extent
  (name nil :type (and symbol (not null)))
  (guards nil :type list)
  (success nil :type boolean))

(defvar *guarded-extents* ()
  "The current guarded extents.")

(defun enclosing-extent ()
  (car *guarded-extents*))

(defun find-guarded-extent (name)
  (if name
      (cdr (assoc name *guarded-extents* :key #'guarded-extent-name))
      (first *guarded-extents*)))

(declaim (inline %make-extent-guard))
(defstruct-read-only (extent-guard (:constructor make-extent-guard (thunk)))
  (thunk :type (function () (values &optional)))
  (called (box nil) :type box))

(-> execute-extent-guard (extent-guard) (values &optional))
(defun execute-extent-guard (extent-guard)
  (unwind-protect
       (funcall (extent-guard-thunk extent-guard))
    (setf (unbox (extent-guard-called extent-guard)) t)))

(-> execute-extent-guards (guarded-extent) (values &optional))
(defun execute-extent-guards (guarded-extent)
  (nlet execute-extent-guards ((extent-guards (guarded-extent-guards guarded-extent)))
    (when extent-guards
      (unwind-protect
           (execute-extent-guard (first extent-guards))
        (execute-extent-guards (rest extent-guards)))))
  (values))

(defmacro unwind-protect/without-interrupts (protected &body cleanup)
  "Like `unwind-protect', but try to guarantee cleanup forms cannot be
interrupted."
  #+sbcl
  `(sb-sys:without-interrupts
     (unwind-protect
          (sb-sys:without-interrupts
            ,protected)
       ,@cleanup))
  ;; CCL at least guarantees no interrupts in cleanup. TODO Does
  ;; anyone else?
  #+ccl
  `(unwind-protect ,protected ,@cleanup)
  ;; TODO.
  #-(or ccl sbcl)
  `(unwind-protect ,protected ,@cleanup))

(defmacro with-defer ((&key (as nil)) &body body)
  (with-unique-names (guarded-extent)
    `(let* ((,guarded-extent (make-guarded-extent :name (or ,as ',(gensym))))
            (*guarded-extents* (cons ,guarded-extent *guarded-extents*)))
       (unwind-protect/without-interrupts
           (multiple-value-prog1
               (locally ,@body)
             (setf (guard-extent-success ,guarded-extent) t))
         (execute-extent-guards ,guarded-extent)))))

(defun call-deferred (extent-name extent-condition fn &rest args)
  (let ((fn (ensure-function fn))
        (extent (if extent-name
                    (find-guarded-extent extent-name)
                    (enclosing-extent))))
    (ecase-of extent-condition extent-condition
      (:exit
       (push (make-extent-guard (lambda ()
                                  (apply fn args)
                                  (values)))
             extent))
      (:success
       (call-deferred
        extent-name :exit
        (lambda (&rest args)
          (when (guarded-extent-success extent)
            (apply fn args)))))
      (:failure
       (call-deferred
        extent-name :exit
        (lambda (&rest args)
          (unless (guarded-extent-success extent)
            (apply fn args))))))))

(defmacro defer ((fn . args) &key (on :exit) (to nil))
  "Define a single function call as an unconditional extent
guard (see `with-defer').

    (defer (fn x y z))

The function's arguments (`x', `y', `z') are executed immediately, but
the function itself (`fn') is not called until the enclosing extent
exits.

Running the deferred condition can be conditionalized by passing a
keyword argument:

    ;; Only call the deferred function on abnormal exit.
    (defer (fn x y z) :on :failure)
    ;; Only call the deferred function on normal exit.
    (defer (fn x y z) :on :success)

The call can be deferred to a particular named extent with the `:to`
keyword argument.

    (with-defer (:as 'outer)
      (with-defer ()
        (defer (cleanup x) :to 'outer)))
"
  `(call-deferred ,to ,on #',fn ,@args))

(comment
  (lambda ()
    (with-defer ()
      (local
        (def x (open "foo"))
        (defer (close x))))))

(comment
  (lambda ()
    (defun open-managed-file (&rest args)
      (let ((handle (apply #'open args)))
        (defer (close handle))
        handle))
    (with-defer ()
      (local
        (def x (open "foo"))
        (defer (close x))))))
