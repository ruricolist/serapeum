(in-package #:serapeum)

(declaim (inline box))                  ;Allow dynamic-extent.
(defstruct (box (:constructor box (value))
                (:predicate boxp))
  "A box is just a mutable cell.

You create a box using `box' and get and set its value using the
accessor `unbox'.

    (def a-box (box t))
    (unbox a-box) => t
    (setf (unbox a-box) nil)
    (unbox a-box) => nil

At the moment, boxes are implemented as structures, but that may
change. In particular, you should not depend on being able to
recognize boxes using a type or predicate."
  value)

(declaim-freeze-type box)

(setf (documentation 'box 'function)
      "Box a value.")

(defsubst unbox (x)
  "The value in the box X."
  (box-value x))

(defsubst (setf unbox) (value x)
  "Put VALUE in box X."
  (setf (box-value x) value))

;;; The compiler macros are included for CAS.

(define-compiler-macro unbox (x)
  `(box-value ,x))

(define-compiler-macro (setf unbox) (value x)
  `(setf (box-value ,x) ,value))

(defmethod print-object ((self box) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~a" (unbox self)))
  self)

(defmethod make-load-form ((self box) &optional env)
  (declare (ignore env))
  (values `(box)
          `(setf (unbox ',self) ,(unbox self))))

(defpattern box (x)
  (with-unique-names (b)
    `(trivia:guard1 ,b
                    (typep ,b 'box)
                    (unbox ,b)
                    ,x)))
