(in-package #:serapeum)

(export '(synchronized monitor))

(eval-when (:compile-toplevel :load-toplevel)
  (defconstant +lock-class+ (class-of (bt:make-lock))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lock-form (object objectp env)
    (cond ((not objectp)
           `(load-time-value (bt:make-lock "Anonymous critical section")))
          ((constantp object env)
           `(load-time-value (monitor ,object)))
          (t `(monitor ,object)))))

(defmacro synchronized ((&optional (object nil objectp)) &body body &environment env)
  "Run BODY holding a unique lock associated with OBJECT.
If no OBJECT is provided, run BODY as an anonymous critical section."
  (let ((form (lock-form object objectp env)))
    (with-gensyms (lock)
      `(let ((,lock ,form))
         (bt:with-lock-held (,lock)
           ,@body)))))

(defvar *monitors*
  (tg:make-weak-hash-table :weakness :key))

(defvar *monitors-lock* (bt:make-lock "Monitor lock"))

(defgeneric monitor (object)
  (:documentation "Return a unique lock associated with OBJECT."))

(defmethod monitor ((object #.+lock-class+))
  object)

(defmethod monitor ((object t))
  ;; Clozure has lock-free hash tables.
  (#+ccl progn #-ccl synchronized #-ccl ()
    (ensure-gethash object *monitors*
                    (bt:make-lock "Monitor"))))
