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

(defclass has-no-slots () ())

(defclass has-count-slot ()
  ((count)))

(defclass has-foo-slot ()
  ((foo :initarg :foo)))

(defclass has-foo-slot/default ()
  ((foo :initarg :foo)))

(defmethod slot-unbound
    ((class t)
     (instance has-foo-slot/default)
     (slot-name (eql 'foo)))
  (setf (slot-value instance 'foo) :foo))

(test slot-value-safe
  (is (equal '(nil nil nil)
             (multiple-value-list
              (slot-value-safe (make 'has-no-slots) 'foo))))
  (is (equal '(0 nil nil)
             (multiple-value-list
              (slot-value-safe (make 'has-no-slots) 'foo 0))))
  (is (equal '(nil nil t)
             (multiple-value-list
              (slot-value-safe (make 'has-foo-slot) 'foo))))
  (is (equal '(0 nil t)
             (multiple-value-list
              (slot-value-safe (make 'has-foo-slot) 'foo 0))))
  (is (equal '(nil t t)
             (multiple-value-list
              (slot-value-safe (make 'has-foo-slot :foo nil)
                               'foo))))
  (is (equal '(:foo t t)
             (multiple-value-list
              (slot-value-safe (make 'has-foo-slot :foo :foo)
                               'foo))))
  (is (equal '(:foo t t)
             (multiple-value-list
              (slot-value-safe (make 'has-foo-slot/default :foo :foo)
                               'foo))))
  (is (equal '(:foo nil t)
             (multiple-value-list
              (slot-value-safe (make 'has-foo-slot/default)
                               'foo)))))

(test setf-slot-value-safe
  (let ((c (make 'has-count-slot)))
    (incf (slot-value-safe c 'count 0))
    (is (eql (slot-value c 'count) 1))))

(defclass a-foo () ())

(defun make-foo () (make 'a-foo))

#+sbcl
(test reject-implausible-function
  (signals warning
    (compile nil '(lambda () (first (make-foo))))))

(defgeneric ctx-fn (x &key keyword)
  (:method-combination standard/context)
  (:method :context ((x t) &key (keyword nil keyword-supplied?))
    (list* keyword keyword-supplied? (call-next-method)))
  (:method ((x number) &key (keyword 'other))
    (list x keyword)))

(defgeneric ard-fn (x &key keyword)
  (:method :around ((x t) &key (keyword nil keyword-supplied?))
    (list* keyword keyword-supplied? (call-next-method)))
  (:method ((x number) &key (keyword 'other))
    (list x keyword)))

(test context-keyword-defaults
  (is (equal* (ctx-fn 1) (ard-fn 1) '(list other nil 1 other)))
  (is (equal* (ctx-fn 1 :keyword 'given)
              (ard-fn 1 :keyword 'given)
              '(list given t 1 given))))

(test find-class-safe
  (is (null (find-class-safe 'this-class-does-not-exist))))
