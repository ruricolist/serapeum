(in-package :serapeum.tests)

(def-suite clos :in serapeum)
(in-suite clos)

(defclass specific () ())
(defclass more-specific (specific) ())
(defclass even-more-specific (more-specific) ())

(defgeneric foo (x)
  (:method-combination standard/context)
  (:method ((x specific))
    '(specific))
  (:method :around ((x specific))
    (cons 'specific/around (call-next-method)))
  (:method :around ((x more-specific))
    (cons 'more-specific/around (call-next-method)))
  (:method :around ((x even-more-specific))
    (cons 'even-more-specific/around (call-next-method)))
  (:method :context ((x specific))
    (cons 'specific/context (call-next-method)))
  (:method :context ((x more-specific))
    (cons 'more-specific/context (call-next-method)))
  (:method :context ((x even-more-specific))
    (cons 'even-more-specific/context (call-next-method))))

(test standard/context
  (is (equal '(specific/context
               more-specific/context
               even-more-specific/context
               even-more-specific/around
               more-specific/around
               specific/around
               specific)
             (foo (make-instance 'even-more-specific)))))

(defclass my-topmost-object ()
  ())

(defclass my-metaclass (topmost-object-class)
  ()
  (:default-initargs
   :topmost-class 'my-topmost-object))

(defclass my-class ()
  ()
  (:metaclass my-metaclass))

(test topmost-object
  (is (typep (make-instance 'my-class) 'my-topmost-object)))

(defclass abstract-class ()
  ()
  (:metaclass abstract-standard-class))

(test abstract-class
  (signals error
    (make-instance 'abstract-class)))
