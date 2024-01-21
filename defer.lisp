(in-package :serapeum)

(deftype scope-condition ()
  '(member :exit :success :failure))

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

(defloop execute-scope-guards (scope-guards)
  (unwind-protect
      (execute-scope-guard (first scope-guards))
    (execute-scope-guards (rest scope-guards))))

(defmacro unwind-protect* (protected &body cleanup)
  "Like `unwind-protect', but try to guarantee cleanup forms cannot be
interrupted."
  #+sbcl
  `(sb-sys:without-interrupts
    (unwind-protect
        (sb-sys:with-local-interrupts
         ,protected)
      ,@cleanup))
  ;; CCL at least guarantees no interrupts in cleanup.
  #+ccl
  `(unwind-protect ,protected ,@cleanup)
  ;; TODO.
  #-(or ccl sbcl)
  `(unwind-protect ,protected ,@cleanup))

(defmacro with-guarded-scope ((&key) &body body)
  (with-unique-names (success scope-guards)
    `(let ((,success nil)
           (,scope-guards '()))
       (macrolet ((with-scope-guard ((&key (on :exit)) &body body)
                    (ecase-of scope-condition on
                      (:exit
                       `(push
                         (make-scope-guard
                          (lambda ()
                            ,@body
                            (values)))
                         ,',scope-guards))
                      (:success
                       `(with-scope-guard ()
                          (when ,',success
                            ,@body)))
                      (:failure
                       `(with-scope-guard ()
                          (unless ,',success
                            ,@body))))))
         (unwind-protect*
             (multiple-value-prog1
                 (locally ,@body)
               (setf ,success t))
           (execute-scope-guards ,scope-guards))))))

(defmacro with-defer ((&rest kwargs &key &allow-other-keys)
                      &body body)
  "Bind the `defer' macro within a `with-guarded-scope' form.
Using `defer' defines a single function call as an uncondiional scope
guard. The function's arguments are executed immediately, but the
function itself is not called until the scope guard is run."
  `(flet ((call/deferred (fn args)
            (with-scope-guard (:on :exit)
              (apply fn args))))
     (macrolet ((defer ((fn . args))
                  (with-unique-names (temp-args)
                    `(let ((,temp-args (list ,@args)))
                       (call/deferred ,fn ,temp-args)))))
       (with-guarded-scope (,kwargs)
         ,@body))))

(comment
  (lambda ()
    (with-guarded-scope ()
      (local
        (def x (open "foo"))
        (with-scope-guard () (close x))))))
