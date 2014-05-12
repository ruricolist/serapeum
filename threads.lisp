(in-package #:serapeum)

(export '(synchronized monitor))

(defconstant +lock-class+ (class-of (bt:make-lock)))

(defmacro synchronized ((&optional (object nil objectp)) &body body &environment env)
  "Run BODY holding a unique lock associated with OBJECT.
If no OBJECT is provided, run BODY as an anonymous critical section."
  (let ((lock (cond ((not objectp)
                     `(load-time-value (bt:make-lock "Anonymous critical section")))
                    ((constantp object env)
                     `(load-time-value (monitor ,object)))
                    (t `(monitor ,object)))))
    `(bt:with-lock-held (,lock)
       ,@body)))

(defvar *monitors*
  (tg:make-weak-hash-table :weakness :key))

(defvar *monitors-lock* (bt:make-lock "Monitor lock"))

(defgeneric monitor (object))

(defmethod monitor ((object #.+lock-class+))
  object)

(defmethod monitor ((object t))
  ;; Clozure has lock-free hash tables.
  (#+ccl progn #-ccl synchronized #-ccl ()
    (ensure-gethash object *monitors*
                    (bt:make-lock "Monitor"))))
