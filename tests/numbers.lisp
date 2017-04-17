(in-package :serapeum.tests)

(def-suite numbers :in serapeum)
(in-suite numbers)

(test parse-float
  ;; Clinger 1990.
  (is-true
   (= (parse-float "1.448997445238699" :type 'double-float)
      1.4489974452386990d0)))

(test unbits
  (is-true
   (let ((n (random most-positive-fixnum)))
     (= n (unbits (bits n))))))

(test random-in-range
  (is
   (floatp
    (random-in-range most-negative-double-float most-positive-double-float)))

  (for-all ((n (lambda () (random-in-range -20 -5))))
    (is (<= -20 n))
    (is (> -5 n)))

  (for-all ((n (lambda () (random-in-range -100 1))))
    (is (<= -100 n))
    (is (> 1 n)))

  (signals error
    (eval '(random-in-range 1 1)))

  (signals error
    (locally (declare (notinline random-in-range))
      (random-in-range 1 1))))
