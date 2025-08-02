(in-package :serapeum.tests)

(def-suite numbers :in serapeum)
(in-suite numbers)

(test fixnump
  (with-notinline (fixnump)
    (is (fixnump 0))
    (is (fixnump most-negative-fixnum))
    (is (fixnump most-positive-fixnum))
    (is (not (fixnump (1- most-negative-fixnum))))
    (is (not (fixnump (1+ most-positive-fixnum))))))

(test finc
  (let ((x 0))
    (is (eql (finc x) 0))
    (is (eql x 1))
    (is (eql (finc x 12) 1))
    (is (eql x 13))))

(test shift-incf
  (let ((x 0))
    (is (eql (shift-incf x) 0))
    (is (eql x 1))
    (is (eql (shift-incf x 12) 1))
    (is (eql x 13))))

(test fdec
  (let ((x 100))
    (is (eql (fdec x) 100))
    (is (eql x 99))
    (is (eql (fdec x 17) 99))
    (is (eql x 82))))

(test shift-decf
  (let ((x 100))
    (is (eql (shift-decf x) 100))
    (is (eql x 99))
    (is (eql (shift-decf x 17) 99))
    (is (eql x 82))))

(test parse-float
  (with-notinline (parse-float)
    ;; Clinger 1990.
    (is-true
     (= (parse-float "1.448997445238699" :type 'double-float)
        1.4489974452386990d0))
    (is-true (eql (parse-float "1" :type 'double-float) 1.0d0))
    (is-true (eql (nth-value 1 (parse-float "1" :type 'double-float)) 1))
    (is-true (eql (parse-float "21" :start 1 :type 'double-float) 1.0d0))
    (is-true (eql (nth-value 1 (parse-float "21" :start 1 :type 'double-float)) 2))
    (is-true (eql (parse-float "1.0d0") 1.0d0))
    (is-true (eql (nth-value 1 (parse-float "1.0d0")) 5))
    (is-true (eql (parse-float "1.0D0") 1.0D0))
    (is-true (eql (parse-float "1.0s0") 1.0s0))
    (is-true (eql (parse-float "1.0S0") 1.0S0))
    (is-true (eql (parse-float "1.0e0") 1.0e0))
    (is-true (eql (parse-float "1.0E0") 1.0E0))
    (is-true (eql (parse-float "1.0f0") 1.0f0))
    (is-true (eql (parse-float "1.0F0") 1.0F0))
    (is-true (eql (parse-float "1.0l0") 1.0l0))
    (is-true (eql (parse-float "1.0L0") 1.0L0))
    (is-true (eql (parse-float "1.0d+1") 1.0d1))
    (is-true (eql (parse-float "20.0d-1") 2.0d0))
    (is-true (eql (parse-float "2x" :type 'double-float :junk-allowed t)
                  2.0d0))
    (is-true (eql (nth-value 1 (parse-float "2x" :type 'double-float :junk-allowed t))
                  1))
    (signals error (parse-float "2x"))
    (is-true (eql (parse-float "2.0x" :type 'double-float :junk-allowed t)
                  2.0d0))
    (is-true (eql (nth-value 1 (parse-float "2.0x" :type 'double-float :junk-allowed t))
                  3))
    (signals error (parse-float "2.0x"))
    (is-true (eql (parse-float "1.0d0x" :junk-allowed t) 1.0d0))
    (is-true (eql (nth-value 1 (parse-float "1.0d0x" :junk-allowed t))
                  5))
    (signals error (parse-float "1.0d0x"))
    (is-true (eql (parse-float "20.0d-1x" :junk-allowed t) 2.0d0))
    (is-true (eql (nth-value 1 (parse-float "20.0d-1x" :junk-allowed t))
                  7))
    (is (null (parse-float "" :junk-allowed t :type 'double-float)))
    (is (null (parse-float "" :junk-allowed t :type 'single-float)))
    (is (null (parse-float "" :junk-allowed t :type *read-default-float-format*)))
    (signals error (parse-float ""))
    (signals error (parse-float "" :type 'double-float))
    (is-true (eql (parse-float "+1.0d0") 1.0d0))
    (is-true (eql (parse-float "-1.0d0") -1.0d0))
    (is-true (eql (parse-float "x" :junk-allowed t :type 'double-float)
                  0.0d0))
    (is-true (eql (nth-value 1 (parse-float "x" :junk-allowed t :type 'double-float))
                  0))
    (signals error (parse-float "x"))
    (is (eql 3d0 (parse-float "3d0")))
    (signals error (parse-float "3d0.0"))
    (is (eql 3f0 (parse-float "3f0")))
    (signals error (parse-float "3f0.0"))

    (is (eql (parse-float ".5") 0.5f0))
    (is (eql (parse-float "-.5") -0.5f0))))

(test round-to
  (with-notinline (round-to)
    (is (eql 20 (round-to 15 10)))))

(test unbits
  (let ((r (loop repeat 20 collect (random most-positive-fixnum))))
    (is-true
     (equal r (mapcar (lambda (i) (unbits (bits i))) r)))
    (is-true
     (equal r (mapcar (lambda (i) (unbits (bits i :big-endian t)
                                          :big-endian t))
                      r)))))

(test shrink
  (with-notinline (shrink)
    (is (eql (shrink 10 1/2) 5))))

(test grow
  (with-notinline (grow)
    (is (eql (grow 17 2) 51))))

(test shrinkf
  (let ((x 10))
    (is (eql (shrinkf x 1/2) 5))
    (is (eql x 5))))

(test growf
  (let ((x 17))
    (is (eql (growf x 2) 51))
    (is (eql x 51))))

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

  (for-all ((n (lambda () (random-in-range 1 -100))))
    (is (<= -100 n))
    (is (> 1 n)))

  (with-notinline (random-in-range)
    (for-all ((n (lambda () (random-in-range 1 -100))))
      (is (<= -100 n))
      (is (> 1 n))))

  (let ((hi 10))
    (for-all ((n (lambda () (random-in-range -1000 hi))))
      (is (<= -1000 n))
      (is (> hi n))))

  (let ((lo -10)
        (hi  200))
    (for-all ((n (lambda () (random-in-range lo hi))))
      (is (<= lo n))
      (is (> hi n))))

  (signals error
    (eval '(random-in-range 1 1)))
  (signals error
    (funcall (compile nil '(lambda () (random-in-range 1 1)))))

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

(test digit-length
  (for-all ((n (lambda () (random most-positive-fixnum))))
    (is (= (serapeum::digit-length n)
           (length (princ-to-string n))))))

(test null-if-zero
  (is (null (null-if-zero 0)))
  (is (null (null-if-zero 0.0)))
  (is (null (null-if-zero -0.0)))
  (is (null (null-if-zero -0.0)))
  (is (null (null-if-zero -0.0s0)))
  (is (null (null-if-zero 0.0s0)))
  (is (null (null-if-zero 0.0d0)))
  (is (null (null-if-zero -0.0d0)))
  (is (eql 1 (null-if-zero 1))))
