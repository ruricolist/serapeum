(in-package #:serapeum)

;;; NB We used to use non-recursive locks here, but it turns out all
;;; languages providing a `synchronized' keyword (Java, Objective-C,
;;; C#, D) use recursive locks, so that is what we use now.

(eval-when (:compile-toplevel :load-toplevel)
  (defconstant +lock-class+ (class-of (bt:make-recursive-lock))))

;;; We need more space for locks than you might expect. In, say, Java,
;;; only a handful of locks exist at a time. But in Lisp I often use
;;; `synchronized' on symbols, which of course are rarely \(if ever)
;;; garbage-collected. Thus our use of a hash table instead of a list.

(defvar *monitors*
  (tg:make-weak-hash-table
   :weakness :key
   ;; This should be plenty big enough to never need resizing.
   :size 512))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lock-form (object objectp env string)
    (cond ((not objectp)
           (let ((string (or string "Anonymous critical section")))
             `(load-time-value (bt:make-recursive-lock ,string))))
          ((constantp object env)
           `(load-time-value
             (ensure-monitor ,object ,string)))
          (t `(ensure-monitor ,object ,string)))))

;;; TODO A natural extension to the `synchronized' syntax would be to
;;; allow multiple locks to be taken at once. This might be useful if
;;; they could be implicitly re-ordered, using some kind of global
;;; order, to avoid deadlocks.

(defmacro synchronized ((&optional (object nil objectp)) &body body &environment env)
  "Run BODY holding a unique lock associated with OBJECT.
If no OBJECT is provided, run BODY as an anonymous critical section.

If BODY begins with a literal string, attach the string to the lock
object created (as the argument to `bt:make-recursive-lock')."
  (multiple-value-bind (string? body)
      (if (stringp (first body))
          (values (first body) (rest body))
          (values nil body))
    (let* ((form (lock-form object objectp env string?)))
      (with-gensyms (lock)
        `(let ((,lock ,form))
           (bt:with-recursive-lock-held (,lock)
             ,@body))))))

(defgeneric monitor (object)
  (:documentation "Return a unique lock associated with OBJECT."))

(defmethod monitor ((object #.+lock-class+))
  object)

(defmethod monitor ((object t))
  nil)

(defun ensure-monitor (object string)
  (or (monitor object)
      (let ((string (or string "Monitor")))
        (flet ((ensure-monitor (object string)
                 (ensure-gethash object *monitors*
                                 (bt:make-recursive-lock string))))
          ;; Clozure has lock-free hash tables.
          #+ccl (ensure-monitor object string)
          #-ccl (synchronized ()
                  (ensure-monitor object string))))))

(defclass synchronized ()
  ((monitor :initform (bt:make-recursive-lock)
            :reader monitor))
  (:documentation "Mixin for a class with its own monitor."))
