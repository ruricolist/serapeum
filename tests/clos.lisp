(in-package :serapeum.tests)

(def-suite clos :in serapeum)
(in-suite clos)

(defclass clos-example ()
  ((slot1 :initarg :slot1
          :reader clos-example-slot)))

(defmethods clos-example (self slot1)
  (declare (symbol slot1))
  (:method clos-example-1 (self)
    slot1))

(defmethods clos-example (self (slot slot1))
  (declare (symbol slot))
  (:method clos-example-2 (self)
    slot))

(defmethods clos-example (self (slot #'clos-example-slot))
  (declare (symbol slot))
  (:method clos-example-3 (self)
    slot))

(test defmethods
  (let ((object (make 'clos-example :slot1 'x)))
    (is (eql 'x (clos-example-1 object)))
    (is (eql 'x (clos-example-2 object)))
    (is (eql 'x (clos-example-3 object)))))
