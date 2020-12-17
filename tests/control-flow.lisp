(in-package :serapeum.tests)

(def-suite control-flow :in serapeum)
(in-suite control-flow)

(test case-using
  (is (eql 'two
           (case-using #'= (+ 1.0 1.0)
             ((1) 'one)
             ((2) 'two)
             (t 'more))))

  (is (eql 2
           (case-using #'string= "bar"
             (("foo") 1)
             (("bar") 2))))

  (is-true
   (occurs 'string-case
           (macroexpand-1
            '(case-using #'string= "bar"
              (("foo") 1)
              (("bar") 2)))))
  (is-true
   (occurs 'string-case
           (macroexpand-1
            '(case-using 'string= "bar"
              (("foo") 1)
              (("bar") 2)))))

  (is-true
   (case-using #'eql 'x
     (x t)
     (t nil)))

  (is-true
   (occurs 'case
           (macroexpand-1
            '(case-using #'eql 'x
              (x t)
              (t nil)))))

  (is-true
   (case-using #'eql nil
     ((nil) t)
     (t nil))))

(test ecase-using
  (signals error
    (ecase-using #'char= #\y
      (#\x t)))
  (signals error
    (ecase-using #'char #\y))
  (is (eql t
           (ecase-using #'char= #\x
             (#\x t)))))

(test cond-every
  (is (null (cond-every)))
  (is (eql (cond-every (t 1) (otherwise 2)) 1))
  (is (eql (cond-every (otherwise 1)) 1))
  (is (eql (cond-every (t 1) (nil 2)) 1))
  (is (eql (let ((x 1)) (cond-every (x) (nil 2))) 1))
  ;; Tests are evaluated first.
  (is (eql 3
           (let ((x 1))
             (cond-every
               ((< x 2) (incf x))
               ((= x 1) (incf x)))
             x))))

(test bcond
  (is (= 2
         (bcond ((assoc 'b '((a 1) (b 2))) => #'cadr)
                (t nil))))
  (is (= 2
         (bcond ((assoc 'b '((a 1) (b 2))) => cons
                 (cadr cons))
                (t nil)))))

(test select
  (is-true
   (select Pi
     (pi t)
     (t nil)))
  (is-true
   (select 1
     (((- 2 1)) t)))
  (is-true
   (let ((x 1))
     (select 1
       (x t)))))

(test sort-values
  (is (null (sort-values #'>)))
  ;; Should this be an error?
  (is (eql t (sort-values #'> t)))
  (is (equal '(1 2)
             (multiple-value-list
              (sort-values #'< 2 1)))))

(test convert-string-case-to-case
  (is (= 4)
      (string-case "x"
        ("a" 1)
        ("b" 2)
        ("c" 3)
        ("x" 4))))

(test convert-string-case-to-case-with-default-clause
  (is (equal "another"
             (string-case "test" ("t" t) (t "another")))))

(test econd
  (declare (optimize (speed 0) (safety 3)))
  (let ((n (random 10)))
    (signals econd-failure
      (econd
        ((> n 10) (assert nil))))))

(test nix
  (let ((x 1) (y 2) (z 3))
    (is (equal '(1 2 3)
               (multiple-value-list
                (nix x y z))))
    (is (null x))
    (is (null y))
    (is (null z))))
