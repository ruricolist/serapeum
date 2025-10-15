(defpackage :serapeum/box
  (:documentation "Box data structure")
  #+sb-package-locks (:lock t)
  (:use
   :cl
   :alexandria
   :serapeum/types
   :trivia)
  (:import-from
   :serapeum/types
   :declaim-freeze-type)
  (:export
   :box
   :boxp
   :unbox
   :ensure-box))
(in-package :serapeum/box)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +atomic-accessors+
    (and (member :ecl *features*)
         (ignore-errors
          (eval `(defstruct (,(gensym) :atomic-accessors))))
         '(:atomic-accessors))))

(declaim (inline box))                  ;Allow dynamic-extent.
(defstruct (box (:constructor box (unbox))
                (:predicate boxp)
                (:conc-name nil)
                ;; Required for older ECLs only.
                . #.+atomic-accessors+)
  "A box is just a mutable cell.

You create a box using `box' and get and set its value using the
accessor `unbox'.

    (def a-box (box t))
    (unbox a-box) => t
    (setf (unbox a-box) nil)
    (unbox a-box) => nil

Serapeum attempts to provide the guarantee that, on Lisps that support
atomic operations (compare-and-swap), `unbox' on boxes should be
updateable atomically. (See
[atomics](https://github.com/Shinmera/atomics))."
  unbox)

(declaim-freeze-type box)

(setf (documentation 'box 'function)
      "Box a value.")

(setf (documentation 'boxp 'function)
      "Predicate for a boxed value.")

(setf (documentation 'unbox 'function)
      "The value in the box X."

      (documentation '(setf unbox) 'function)
      "Put VALUE in box X.")

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

(-> ensure-box (t) box)
(defun ensure-box (x)
  "Return X if boxed, otherwise a box containing X."
  (if (boxp x) x (box x)))
