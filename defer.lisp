(in-package :serapeum)

(deftype scope-condition ()
  '(member :exit :success :failure))

(declaim (inline make-guarded-scope))
(defstruct guarded-scope
  (name nil :type symbol)
  (guards nil :type list)
  (success nil :type boolean))

(defvar *static-scope* (make-guarded-scope))

(defun execute-static-scope ()
  (execute-scope-guards *static-scope*))

(exit-hooks:add-exit-hook #'execute-static-scope)

(defvar *guarded-scopes* (list *static-scope*)
  "The current guarded scopes.")

(defun enclosing-scope ()
  (car *guarded-scopes*))

(defun find-guarded-scope (name)
  (if name
      (cdr (assoc name *guarded-scopes* :key #'guarded-scope-name))
      (first *guarded-scopes*)))

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

(defmacro with-guarded-scope ((&key (name nil)) &body body)
  (with-unique-names (guarded-scope)
    `(let* ((,guarded-scope (make-guarded-scope :name ',name))
            (*guarded-scopes* (cons ,guarded-scope *guarded-scopes*)))
       (unwind-protect/without-interrupts
           (multiple-value-prog1
               (locally ,@body)
             (setf (guard-scope-success ,guarded-scope) t))
         (execute-scope-guards ,guarded-scope)))))

(defmacro with-scope-guard ((&key (on :exit) (scope nil scope-provided-p)) &body body)
  (with-unique-names (guarded-scope)
    `(let ((,guarded-scope
             ,(if scope-provided-p
                  `(find-guarded-scope ,scope)
                  `(enclosing-scope))))
       ,(ecase-of scope-condition on
          (:exit
           `(push
             (make-scope-guard
              (lambda ()
                ,@body
                (values)))
             ,guarded-scope))
          (:success
           `(with-scope-guard (:scope ,scope)
              (when (guarded-scope-success ,guarded-scope)
                ,@body)))
          (:failure
           `(with-scope-guard (:scope ,scope)
              (unless (guarded-scope-success ,guarded-scope)
                ,@body)))))))

(defun call-deferred (scope-name fn &rest args)
  (with-scope-guard (:on :exit :scope scope-name)
    (apply fn args)))

(defmacro defer ((fn . args))
  "Define a single function call as an unconditional scope
guard.

    (defer (fn x y z))
    ≅ (with-scope-guard (:on :exit)
        (fn x y z))

The function's arguments are executed immediately, but the
function itself is not called until the scope guard is run."
  `(defer-to nil (,fn ,@args)))

(defmacro defer-to (name (fn . args))
  (with-unique-names (temp-args)
    `(let ((,temp-args (list ,@args)))
       (call-deferred ,name #',fn ,temp-args))))

(comment
  (lambda ()
    (with-guarded-scope ()
      (local
        (def x (open "foo"))
        (defer (close x))
        (with-scope-guard () (close x))))))

(comment
  (lambda ()
    (defun open-managed-file (&rest args)
      (let ((handle (apply #'open args)))
        (defer (close handle))
        handle))
    (with-guarded-scope ()
      (local
        (def x (open "foo"))
        (with-scope-guard () (close x))))))
