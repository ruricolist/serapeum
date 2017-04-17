(in-package :serapeum.tests)

(def-suite vectors :in serapeum)
(in-suite vectors)

(test vect
  (is (adjustable-array-p (vect)))
  (is (fill-pointer (vect)))
  (is (equalp (vect 1 2 3) #(1 2 3))))
