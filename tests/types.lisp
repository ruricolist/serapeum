(in-package :serapeum.tests)

(def-suite types :in serapeum)
(in-suite types)

(test tuple
  (is (typep '() '(tuple)))
  (is (not (typep '() '(tuple null))))
  (is (typep '(1) '(tuple integer)))
  (is (not (typep '(1) '(tuple symbol))))
  (is (typep '(1 :x #\c) '(tuple integer symbol character)))
  (is (typep '(1 2 3) '(tuple 1 2 3)))
  (is (not (typep '(1 2 3) '(tuple 3 2 1))))
  (is (typep '(#\c) '(tuple #\c)))
  (is (typep (list :name 1 #\a 'x)
             '(tuple :name 1 #\a symbol)))
  (is (typep (list :name 1 #\a 'x)
             '(tuple :name 1 #\a 'x))))

(test supertypep
  (is (supertypep 'rational 'integer))
  (is (supertypep 'integer 'integer)))

(test proper-subtype-p
  (is (proper-subtype-p 'integer 'rational))
  (is (not (proper-subtype-p 'integer 'integer))))

(test true
  (is (true t))
  (is (not (true nil)))
  ;; NB The read base doesn't matter; 0 (or 1) is always a digit char.
  (is (eql t (true (digit-char-p #\0)))))

(test assure-pattern
  (is
   (stringp
    (trivia:match ""
      ((assure string x)
       x))))
  (is
   (null
    (trivia:match ""
      ((assure list x)
       x)))))

(test assure-values
  (is (equal '(1 2)
             (multiple-value-list
              (assure (values integer integer)
                (values 1 2)))))
  (signals type-error
    (assure (values integer integer)
      (values 1 "2")))
  (is (equal '(1 2)
             (multiple-value-list
              (assure (values integer integer &optional integer)
                (values 1 2)))))
  (signals type-error
   (assure (values integer integer &optional integer)
     (values 1 2 "3")))
  (is (equal '(1 2 3 4 5)
             (multiple-value-list
              (assure (values integer &rest integer)
                (values 1 2 3 4 5)))))
  (signals type-error
    (assure (values integer &rest integer)
      (values "1" 2 3 4 5)))
  ;; Aspirational
  ;; (signals type-error
  ;;   (assure (values integer &rest integer)
  ;;     (values 1 2 3 4 "5")))
  (signals type-error
    (assure (values integer &rest integer)
      (values 1 2 3 4 nil))))

(test (with-item-key-function :compile-at :run-time)
  (finishes
    (locally (declare (optimize (space 3) (speed 0)))
      (let ((key #'reverse))
        (with-item-key-function (key)
          (key "xyz")))))
  (finishes
    (locally (declare (optimize (speed 3) (space 0)))
      (let ((key #'reverse))
        (with-item-key-function (key)
          (key "xyz"))))))

(defun test-with-boolean-values-actual (x y z)
  (let ((result 0))
    (with-boolean (x y z)
      (boolean-when x
        (incf result 100))
      (boolean-unless y
        (incf result 20))
      (boolean-if
       z
       (incf result 3)
       (incf result 4)))
    result))

(defun test-with-boolean-values-expected (x y z)
  (let ((result 0))
    (when x
      (incf result 100))
    (unless y
      (incf result 20))
    (if z
        (incf result 3)
        (incf result 4))
    result))

(test with-boolean-value
  (dolist (x '(nil t))
    (dolist (y '(nil t))
      (dolist (z '(nil t))
        (let ((actual (test-with-boolean-values-actual x y z))
              (expected (test-with-boolean-values-expected x y z)))
          (is-true (= actual expected)))))))

(defparameter *with-boolean-expansion-before*
  `(with-boolean (x y z)
     (boolean-when x
       (print "X is true"))
     (boolean-unless y
       (print "X is false"))
     (boolean-if
      z
      (print "Z is true!")
      (print "Z is false!"))))

(defparameter *with-boolean-expansion-after*
  `(macrolet ((:if (cond serapeum::then serapeum::else)
                   (list 'boolean-if cond serapeum::then serapeum::else))
              (:when (cond &body serapeum::then)
                (list* 'boolean-when cond serapeum::then))
              (:unless (cond &body serapeum::then)
                (list* 'boolean-unless cond serapeum::then)))
     (locally (declare
               #+sbcl (sb-ext:disable-package-locks serapeum::%in-branching%
                                                    serapeum::%all-branches%))
       (symbol-macrolet ((serapeum::%in-branching% t)
                         (serapeum::%all-branches% (x y z))
                         (serapeum::%true-branches% nil))
         (locally
             (declare
              #+sbcl (sb-ext:enable-package-locks serapeum::%in-branching%
                                                  serapeum::%all-branches%))
           (if x
               (symbol-macrolet ((serapeum::%true-branches% (x)))
                 (if y
                     (symbol-macrolet ((serapeum::%true-branches% (y x)))
                       (if z
                           (symbol-macrolet ((serapeum::%true-branches% (z y x)))
                             (progn (print "X is true") (progn)
                                    (print "Z is true!")))
                           (progn (print "X is true") (progn)
                                  (print "Z is false!"))))
                     (if z
                         (symbol-macrolet ((serapeum::%true-branches% (z x)))
                           (progn
                             (print "X is true")
                             (print "X is false")
                             (print "Z is true!")))
                         (progn
                           (print "X is true")
                           (print "X is false")
                           (print "Z is false!")))))
               (if y
                   (symbol-macrolet ((serapeum::%true-branches% (y)))
                     (if z
                         (symbol-macrolet ((serapeum::%true-branches% (z y)))
                           (progn (progn) (progn) (print "Z is true!")))
                         (progn (progn) (progn) (print "Z is false!"))))
                   (if z
                       (symbol-macrolet ((serapeum::%true-branches% (z)))
                         (progn (progn) (print "X is false")
                                (print "Z is true!")))
                       (progn (progn) (print "X is false")
                              (print "Z is false!"))))))))))

(test test-with-boolean-expansion
  (#+sbcl sb-ext:without-package-locks
   #-sbcl progn
   (let* ((form *with-boolean-expansion-before*)
          (expected *with-boolean-expansion-after*)
          (actual (trivial-macroexpand-all:macroexpand-all form)))
     (is (equal expected actual)))))

(test test-with-boolean-missing-lexical-environment
  (let ((x 42))
    (declare (ignorable x))
    (flet ((test-1 () (boolean-if x :foo :bar))
           (test-2 () (boolean-when x :foo))
           (test-3 () (boolean-unless x :foo))
           (test (fn)
             (multiple-value-bind (value error)
                 (ignore-errors (funcall fn))
               (is (null value))
               (is (typep error 'program-error)))))
      (test #'test-1)
      (test #'test-2)
      (test #'test-3))))

(test test-with-boolean-missing-branch
  (let ((x 42) (y 24))
    (declare (ignorable x y))
    (flet ((test ()
             (with-boolean (x)
               (boolean-if y 42))))
      (multiple-value-bind (value error)
          (ignore-errors (funcall #'test))
        (is (null value))
        (is (typep error 'program-error))))))

(test soft-list-of
  (is-true (typep () '(soft-list-of (not null))))
  (is-true (typep '(t) '(soft-list-of (not null))))
  (is-false (typep '(nil) '(soft-list-of (not null))))
  (is-false (typep '(t nil) '(soft-list-of (not null))))
  (is-true (typep '(t t) '(soft-list-of (not null))))
  (is-false (typep '(nil t) '(soft-list-of (not null))))
  (is-false (typep '(t t t nil) '(soft-list-of (not null))))

  (is-true (typep '(1 2 3) '(soft-list-of number)))
  ;; !!!
  (is-true (typep '(1 2 :x) '(soft-list-of number)))
  (is-false (typep '(1 2 nil) '(soft-list-of number)))
  (is-false (typep '(1 . 2) '(soft-list-of number)))
  ;; !!!
  (is-true (typep
            (append (make-list 20 :initial-element 1)
                    '(2 . 3))
            '(soft-list-of number))))

(test soft-alist-of
  (is (typep () '(soft-alist-of string number)))
  (is (typep '(("x" . 1)) '(soft-alist-of string number)))
  (is-false (typep '((:x . 1)) '(soft-alist-of string number)))
  (is-false (typep '(1 . 2) '(soft-alist-of number number)))
  (is-false (typep '(1) '(soft-alist-of number number)))
  (is (typep '((x . y) (1 . 2) (3 . 4)) '(soft-alist-of t t))))
