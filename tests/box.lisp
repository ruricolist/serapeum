(in-package :serapeum.tests)

(def-suite box :in serapeum)
(in-suite box)

#+(or allegro ccl clasp ecl lispworks mezzano sbcl)
(test box-cas
  (let ((box (box nil)))
    (atomics:cas (unbox box) nil t)
    (is (eql t (unbox box)))))

#+(or allegro ccl clasp ecl lispworks mezzano sbcl)
(test box-atomic-update
  (let ((box (box nil)))
    (atomics:atomic-update (unbox box) (constantly t))
    (is (eql t (unbox box)))))
