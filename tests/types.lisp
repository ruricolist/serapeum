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

(defmacro expansion-time-constant? (form &environment env)
  (nth-value 1 (eval-if-constant form env)))

(test with-boolean
  (let ((x t))
    (with-boolean (x)
      (is-true (expansion-time-constant? x)))))

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
