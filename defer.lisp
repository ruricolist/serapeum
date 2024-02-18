(in-package :serapeum)

(deftype scope-condition ()
  '(member :exit :success :failure))

(defvar-unbound *guarded-scope*
  "The current guarded scope.")

(declaim (inline make-guarded-scope))
(defstruct guarded-scope
  (guards nil :type list)
  (success nil :type boolean))

(declaim (inline %make-scope-guard))
(defstruct-read-only (scope-guard (:constructor %make-scope-guard))
  (thunk :type (function () (values &optional)))
  (called (box nil) :type box))

(defun finalize-scope-guard (scope-guard)
  (let ((called (scope-guard-called scope-guard)))
    (tg:finalize scope-guard
                 (lambda ()
                   (unless (unbox called)
                     (error "Scope guard thunk was never called"))))))

(defun make-scope-guard (thunk)
  (lret ((scope-guard (%make-scope-guard :thunk thunk)))
    (finalize-scope-guard scope-guard)))

(-> execute-scope-guard (scope-guard) (values &optional))
(defun execute-scope-guard (scope-guard)
  (unwind-protect
       (funcall (scope-guard-thunk scope-guard))
    (setf (unbox (scope-guard-called scope-guard)) t)))

(-> execute-scope-guards (guarded-scope) (values &optional))
(defun execute-scope-guards (guarded-scope)
  (nlet execute-scope-guards ((scope-guards (guarded-scope-guards guarded-scope)))
    (unwind-protect
         (execute-scope-guard (first scope-guards))
      (execute-scope-guards (rest scope-guards))))
  (values))

(defmacro unwind-protect* (protected &body cleanup)
  "Like `unwind-protect', but try to guarantee cleanup forms cannot be
interrupted."
  #+sbcl
  `(sb-sys:without-interrupts
     (unwind-protect
          (sb-sys:with-local-interrupts
            ,protected)
       ,@cleanup))
  ;; CCL at least guarantees no interrupts in cleanup. TODO Does
  ;; anyone else?
  #+ccl
  `(unwind-protect ,protected ,@cleanup)
  ;; TODO.
  #-(or ccl sbcl)
  `(unwind-protect ,protected ,@cleanup))

(defmacro with-guarded-scope ((&key) &body body)
  (with-unique-names (guarded-scope)
    `(let* ((,guarded-scope (make-guarded-scope))
            (*guarded-scope* ,guarded-scope))
       (unwind-protect*
           (multiple-value-prog1
               (locally ,@body)
             (setf (guard-scope-success ,guarded-scope) t))
         (execute-scope-guards ,guarded-scope)))))

(defmacro with-scope-guard ((&key (on :exit)) &body body)
  (with-unique-names (guarded-scope)
    `(let ((,guarded-scope *guarded-scope*))
       ,(ecase-of scope-condition on
          (:exit
           `(push
             (make-scope-guard
              (lambda ()
                ,@body
                (values)))
             ,guarded-scope))
          (:success
           `(with-scope-guard ()
              (when (guarded-scope-success ,guarded-scope)
                ,@body)))
          (:failure
           `(with-scope-guard ()
              (unless (guarded-scope-success ,guarded-scope)
                ,@body)))))))

(defun call-deferred (fn &rest args)
  (with-scope-guard (:on :exit)
    (apply fn args)))

(defmacro defer ((fn . args))
  "Define a single function call as an uncondiional scope
guard.

The function's arguments are executed immediately, but the
function itself is not called until the scope guard is run."
  (with-unique-names (temp-args)
    `(let ((,temp-args (list ,@args)))
       (call-deferred #',fn ,temp-args))))

(comment
  (lambda ()
    (with-guarded-scope ()
      (local
        (def x (open "foo"))
        (with-scope-guard () (close x))))))
