(in-package :serapeum.tests)

(def-suite macro-tools :in serapeum)
(in-suite macro-tools)

(test read-only-var
  (let ((x 1))
    (serapeum::with-read-only-var (x)
      (is (eql x 1))))

  (signals error
    (eval*
     '(let ((x 1))
       (serapeum::with-read-only-var (x)
         (setf x 2))))))
