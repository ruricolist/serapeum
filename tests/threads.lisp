(in-package :serapeum.tests)

(def-suite threads :in serapeum)
(in-suite threads)

(test count-cpus
  (is (> (count-cpus) 0))
  (is (<= (count-cpus :online t)
          (count-cpus))))
