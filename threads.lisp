(in-package #:serapeum)

(eval-when (:compile-toplevel :load-toplevel)
  (defconstant +lock-class+ (class-of (bt:make-lock))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lock-form (object objectp env string)
    (cond ((not objectp)
           (let ((string (or string "Anonymous critical section")))
             `(load-time-value (bt:make-lock ,string))))
          ((constantp object env)
           `(load-time-value
             (monitor* ,object ,string)))
          (t `(monitor* ,object ,string)))))

(defmacro synchronized ((&optional (object nil objectp)) &body body &environment env)
  "Run BODY holding a unique lock associated with OBJECT.
If no OBJECT is provided, run BODY as an anonymous critical section.

If BODY begins with a literal string, attach the string to the lock
object created (as the argument to `bt:make-lock')."
  (multiple-value-bind (string? body)
      (if (stringp (first body))
          (values (first body) (rest body))
          (values nil body))
    (let* ((form (lock-form object objectp env string?)))
      (with-gensyms (lock)
        `(let ((,lock ,form))
           (bt:with-lock-held (,lock)
             ,@body))))))

(defvar *monitors*
  (tg:make-weak-hash-table :weakness :key))

(defgeneric monitor (object)
  (:documentation "Return a unique lock associated with OBJECT."))

(defmethod monitor ((object #.+lock-class+))
  object)

(defmethod monitor ((object t))
  nil)

(defun monitor* (object string)
  (or (monitor object)
      (let ((string (or string "Monitor")))
        (flet ((ensure-monitor (object string)
                 (ensure-gethash object *monitors*
                                 (bt:make-lock string))))

          ;; Clozure has lock-free hash tables.
          #+ccl (ensure-monitor object string)
          #-ccl (synchronized ()
                  (ensure-monitor object string))))))
