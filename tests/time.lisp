(in-package :serapeum.tests)

(def-suite time :in serapeum)
(in-suite time)

(test interval
  (is (= 31626000 (interval :years 1 :days 1 :hours 1))))
