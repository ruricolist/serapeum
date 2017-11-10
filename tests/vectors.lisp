(in-package :serapeum.tests)

(def-suite vectors :in serapeum)
(in-suite vectors)

(test vect
  (is (adjustable-array-p (vect)))
  (is (fill-pointer (vect)))
  (is (equalp (vect 1 2 3) #(1 2 3))))

;;; Regression for #14.

(test dx-vect
  (is (equal
       (princ-to-string #((foo)))
       (princ-to-string ((lambda () (vect (list 'foo))))))))

(test vector=
  (is (vector= #() #()))
  (is (vector= "" ""))
  (is (vector= "" #()))
  (is (vector= #() ""))
  (is (vector= #* #()))
  (is (vector= #() #*))
  (is (vector= #(x y z) #(y z) :start1 1))
  (is (vector= #(x y) #(x y z) :end2 2)))
