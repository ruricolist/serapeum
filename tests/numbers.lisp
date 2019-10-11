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
      (random-in-range 1 1)))

  (multiple-value-bind (results1 results2)
      (let* ((rs1 (make-random-state nil))
             (rs2 (make-random-state rs1))
             (n 10)
             (results1 (let ((*random-state* rs1))
                         (loop repeat n
                               collect
                               (random-in-range
                                most-negative-single-float
                                most-positive-double-float))))
             (results2 (let ((*random-state* rs2))
                         (loop repeat n
                               collect
                               (random-in-range
                                (coerce most-negative-single-float 'double-float)
                                most-positive-double-float)))))
        (values results1 results2))
    (is (every #'= results1 results2))))

(test si-prefix
  (is (equal "yocto" (si-prefix least-positive-double-float)))
  (is (equal "yotta" (si-prefix most-positive-double-float)))
  (is-true (every (equals "") (mapcar #'si-prefix '(0 1 0s0 1s0 0d0 1d0))))
  (is (equal "deca" (si-prefix 10 :base 10)))
  (is (equal "deca" (si-prefix 12 :base 10)))
  (is (equal "kilo" (si-prefix 1001)))
  (is (equal "kibi" (si-prefix 2048 :base 2)))
  (is (equal "kibi" (si-prefix 1024 :base 2)))
  (is (equal "" (si-prefix 1000 :base 2)))
  (is (equal "" (si-prefix 1000 :base 1024)))
  (is (equal "kilo" (si-prefix 1024)))
  (is (equal "kilo" (si-prefix 1000)))
  (is (equal "pico" (si-prefix 1s-9)))
  (is (equal "nano" (si-prefix 1s-9 :base 1024)))
  (is (equal "kilo" (si-prefix -20000)))
  (is (equal "" (si-prefix -20)))
  (is (equal "kilo" (si-prefix (expt 2 10))))
  (is (equal "kibi" (si-prefix (expt 2 10) :base 2)))
  (is (equal "yocto" (si-prefix (expt 10 -23)))))
