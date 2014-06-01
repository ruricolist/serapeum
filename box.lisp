(in-package #:serapeum)

(export '(box unbox))

;;; TODO Weak boxes.

(declaim (inline box))                  ;Allow dynamic-extent.
(defstruct (box (:constructor box (value))
                (:predicate boxp))
  "A box: a minimal mutable cell, like a cons without a cdr. Use
  unbox to access the current value."
  value)

(setf (documentation 'box 'function)
      "Box a value.")

(defmethod print-object ((self box) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~a" (unbox self))))

(defmethod make-load-form ((self box) &optional env)
  (declare (ignore env))
  (values `(box)
          `(setf (unbox ',self) ,(unbox self))))

(defsubst unbox (x)
  "The value in the box X."
  (box-value x))

(defsubst (setf unbox) (value x)
  "Put VALUE in box X."
  (setf (box-value x) value))
