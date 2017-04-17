(in-package :serapeum.tests)

(def-suite arrays :in serapeum)
(in-suite arrays)

(test array-index-row-major
  (is-true
   (let ((a (make-array '(7 4 9 5)))
         (subscripts '(3 2 8 1)))
     (equal subscripts
            (array-index-row-major
             a
             (apply #'array-row-major-index a subscripts))))))
