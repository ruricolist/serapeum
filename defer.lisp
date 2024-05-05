(in-package :serapeum)

(deftype extent-condition ()
  '(member :exit :success :failure))

(declaim (inline make-extent))
(defstruct guarded-extent
  (name nil :type symbol)
  (guards nil :type list)
  (success nil :type boolean))

(defvar *static-extent* (make-guarded-extent))

(defun execute-static-extent ()
  (execute-extent-guards *static-extent*))

(exit-hooks:add-exit-hook #'execute-static-extent)

(defvar *guarded-extents* (list *static-extent*)
  "The current guarded extents.")

(defun enclosing-extent ()
  (car *guarded-extents*))

(defun find-guarded-extent (name)
  (if name
      (cdr (assoc name *guarded-extents* :key #'guarded-extent-name))
      (first *guarded-extents*)))

(declaim (inline %make-extent-guard))
(defstruct-read-only (extent-guard (:constructor %make-extent-guard))
  (thunk :type (function () (values &optional)))
  (called (box nil) :type box))

(defun finalize-extent-guard (extent-guard)
  (let ((called (extent-guard-called extent-guard)))
    (tg:finalize extent-guard
                 (lambda ()
                   (unless (unbox called)
                     (error "Extent guard thunk was never called"))))))

(defun make-extent-guard (thunk)
  (lret ((extent-guard (%make-extent-guard :thunk thunk)))
    (finalize-extent-guard extent-guard)))

(-> execute-extent-guard (extent-guard) (values &optional))
(defun execute-extent-guard (extent-guard)
  (unwind-protect
       (funcall (extent-guard-thunk extent-guard))
    (setf (unbox (extent-guard-called extent-guard)) t)))

(-> execute-extent-guards (guarded-extent) (values &optional))
(defun execute-extent-guards (guarded-extent)
  (nlet execute-extent-guards ((extent-guards (guarded-extent-guards guarded-extent)))
    (unwind-protect
         (execute-extent-guard (first extent-guards))
      (execute-extent-guards (rest extent-guards))))
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

(defmacro with-guarded-extent ((&key (name nil)) &body body)
  (with-unique-names (guarded-extent)
    `(let* ((,guarded-extent (make-guarded-extent :name ',name))
            (*guarded-extents* (cons ,guarded-extent *guarded-extents*)))
       (unwind-protect/without-interrupts
           (multiple-value-prog1
               (locally ,@body)
             (setf (guard-extent-success ,guarded-extent) t))
         (execute-extent-guards ,guarded-extent)))))

(defmacro with-extent-guard ((&key (on :exit) (extent nil extent-provided-p)) &body body)
  (with-unique-names (guarded-extent)
    `(let ((,guarded-extent
             ,(if extent-provided-p
                  `(find-guarded-extent ,extent)
                  `(enclosing-extent))))
       ,(ecase-of extent-condition on
          (:exit
           `(push
             (make-extent-guard
              (lambda ()
                ,@body
                (values)))
             ,guarded-extent))
          (:success
           `(with-extent-guard (:extent ,extent)
              (when (guarded-extent-success ,guarded-extent)
                ,@body)))
          (:failure
           `(with-extent-guard (:extent ,extent)
              (unless (guarded-extent-success ,guarded-extent)
                ,@body)))))))

(defun call-deferred (extent-name fn &rest args)
  (with-extent-guard (:on :exit :extent extent-name)
    (apply fn args)))

(defmacro defer ((fn . args))
  "Define a single function call as an unconditional extent
guard.

    (defer (fn x y z))
    ≅ (with-extent-guard (:on :exit)
        (fn x y z))

The function's arguments are executed immediately, but the
function itself is not called until the extent guard is run."
  `(defer-to nil (,fn ,@args)))

(defmacro defer-to (name (fn . args))
  (with-unique-names (temp-args)
    `(let ((,temp-args (list ,@args)))
       (call-deferred ,name #',fn ,temp-args))))

(comment
  (lambda ()
    (with-guarded-extent ()
      (local
        (def x (open "foo"))
        (defer (close x))
        (with-extent-guard () (close x))))))

(comment
  (lambda ()
    (defun open-managed-file (&rest args)
      (let ((handle (apply #'open args)))
        (defer (close handle))
        handle))
    (with-guarded-extent ()
      (local
        (def x (open "foo"))
        (with-extent-guard () (close x))))))
