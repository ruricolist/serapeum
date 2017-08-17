(in-package :serapeum.tests)

(def-suite range :in serapeum)
(in-suite range)

(test range
  (is
   (vector= (range 10)
            #(0 1 2 3 4 5 6 7 8 9 10)))

  (is
   (vector= (range -5 5)
            #(-5 -4 -3 -2 -1 0 1 2 3 4)))

  (is
   (vector= (range -100 100 10)
            #(-100 -90 -80 -70 -60 -50 -40 -30 -20 -10
              0 10 20 30 40 50 60 70 80 90)))

  (is
   (vector= (range 0 4 2)
            #(0 2)))

  (is
   (vector= (range 0 5 2)
            #(0 2 4)))

  (is
   (vector= (range 0 6 2)
            #(0 2 4)))

  (is
   (vector= (range 0 6 2)
            #(0 2 4)))

  (is
   (vector= (range 0 7 2)
            #(0 2 4 6)))

  (is
   (vector= (range 100 0 -10)
            #(100 90 80 70 60 50 40 30 20 10)))

  (is
   (vector= (range 10 -10 -1)
            #(10 9 8 7 6 5 4 3 2 1 0 -1 -2 -3 -4 -5 -6 -7 -8 -9)))

  (is (vector= (range 0 0) #()))

  (is (vector= (range 0 1) #(0))))

(test )
