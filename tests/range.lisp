(in-package :serapeum.tests)

(def-suite range :in serapeum)
(in-suite range)

(defsubst range= (x y)
  "Wrap vector= for better reporting from FiveAM."
  (vector= x y :test #'=))

(defmacro test-range (actual expected)
  `(progn
     (locally (declare (inline range))
       (is (range= ,expected ,actual)))
     (locally (declare (notinline range))
       (is (range= ,expected ,actual)))))

(test empty-range
  (test-range (range 0 0) #())
  (test-range (range 0) #()))

(test bit-range
  (test-range (range 0 1) #(0))
  (test-range (range 1 0 -1) #(1))
  (signals error
    (eval '(range 1 0 1))))

(test integer-range
  (test-range (range 10)
              #(0 1 2 3 4 5 6 7 8 9))

  (test-range (range 1 11)
              #(1 2 3 4 5 6 7 8 9 10))

  (test-range (range 0 30 5)
              #(0 5 10 15 20 25))

  (test-range (range 0 10 3)
              #(0 3 6 9))

  (test-range (range 0 4 2)
              #(0 2))

  (test-range (range 0 5 2)
              #(0 2 4))

  (test-range (range 0 6 2)
              #(0 2 4))

  (test-range (range 0 6 2)
              #(0 2 4))

  (test-range (range 0 7 2)
              #(0 2 4 6)))

(test negative-integer-range
  (test-range (range -5 5)
              #(-5 -4 -3 -2 -1 0 1 2 3 4))

  (signals error
    (eval '(range -5 5 -1)))

  (test-range (range -100 100 10)
              #(-100 -90 -80 -70 -60 -50 -40 -30 -20 -10
                0 10 20 30 40 50 60 70 80 90))

  (signals error
    (eval '(range -100 100 -10)))

  (test-range (range 100 0 -10)
              #(100 90 80 70 60 50 40 30 20 10))

  (test-range (range 10 -10 -1)
              #(10 9 8 7 6 5 4 3 2 1 0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (test-range (range 0 -10 -1)
              #(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)))

(test float-range
  (test-range (range 0 8/10 1/10)
              #(0 1/10 1/5 3/10 2/5 1/2 3/5 7/10))

  (is (length= (range 0 8/10 1/10)
               (range 0 0.8d0 0.1d0)))

  (is (length= (range 0.4d0 0.6d0 0.2d0)
               (range 4/10 6/10 2/10)))

  (is (length= (range 0.4s0 0.8s0 0.2s0)
               (range 4/10 8/10 2/10))))

(test int-range-step
  (test-range (range -2 5 2)
              #(-2 0 2 4))
  (test-range (range 0 7 2)
              #(0 2 4 6))
  (test-range (range 5 -2 -2)
              #(5 3 1 -1))

  (test-range (range -2 6 3) #(-2 1 4))
  (test-range (range -2 8 3) #(-2 1 4 7)))

(test real-range
  (test-range (range -1/4 2/4 2/4) #(-1/4 1/4)))
