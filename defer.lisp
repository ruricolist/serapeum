(in-package :serapeum)

(deftype extent-condition ()
  '(member :exit :success :failure))

(declaim (inline make-extent))
(defstruct guarded-extent
  (name nil :type (and symbol (not null)) :read-only t)
  (guards nil :type list)
  (success nil :type boolean))

(defvar *static-extent*
  (make-guarded-extent :name :static))

(defvar *guarded-extents*
  (list *static-extent*)
  "The current guarded extents.")

(exit-hooks:add-exit-hook #'execute-static-extent)

(defun enclosing-extent ()
  (car *guarded-extents*))

(defun find-guarded-extent (extent)
  (or (etypecase extent
        (null (first *guarded-extents*))
        (symbol (cdr (assoc extent *guarded-extents* :key #'guarded-extent-name)))
        (guarded-extent extent))
      (error "No extent: ~a" extent)))

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
          (sb-sys:with-local-interrupts
            ,protected)
       (sb-sys:without-interrupts
         ,@cleanup)))
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

(defun defer (fn arg &key (on :exit) to)
  "Define a single function call as an extent guard (see `with-defer').

    (defer #'fn x)

The function's argument (`x') is executed immediately, but
the function itself (`fn') is not called until the enclosing extent
exits.

Running the deferred condition can be conditionalized by passing a
keyword argument:

    ;; Only call the deferred function on abnormal exit.
    (defer #'fn x :on :failure)
    ;; Only call the deferred function on normal exit.
    (defer #'fn x :on :success)

The call can be deferred to a particular named extent with the `:to`
keyword argument.

    (with-defer (:as 'outer)
      (with-defer ()
        (defer #'cleanup x :to 'outer)))

Returns the target extent."
  (let ((fn (ensure-function fn))
        (extent (if to
                    (find-guarded-extent to)
                    (enclosing-extent))))
    (ecase-of extent-condition on
      (:exit
       (lret ((guard
               (make-extent-guard (lambda ()
                                    (funcall fn arg)
                                    (values)))))
         (push guard (guarded-extent-guards extent))))
      (:success
       (defer (lambda (arg)
                (when (guarded-extent-success extent)
                  (funcall fn arg)))
              arg
              :on :exit
              :to extent))
      (:failure
       (defer (lambda (arg)
                (unless (guarded-extent-success extent)
                  (funcall fn arg)))
              arg
              :on :exit
              :to extent)))))


(comment
  (lambda ()
    (with-defer ()
      (local
        (def x (open "foo"))
        (defer #'close x)))))

(comment
  (lambda ()
    (defun open-managed-file (&rest args)
      (let ((handle (apply #'open args)))
        (defer #'close handle)
        handle))
    (with-defer (
                 )
      (local
        (def x (open "foo"))
        (defer #'close x)))))
