(in-package :serapeum.tests)

(def-suite threads :in serapeum)
(in-suite threads)

(test count-cpus
      (is (> (count-cpus) 0)))

(test synchronized-no-body
  (is (null (synchronized ()))))
