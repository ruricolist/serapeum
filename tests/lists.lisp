(in-package :serapeum.tests)

(def-suite lists :in serapeum)
(in-suite lists)

(test memq
  (let* ((list1 (list 'x 'y nil 'z))
         (list2 (delq nil list1)))
    (is (not (memq nil list2)))
    (is (eq list1 list2))))

(test mapply
  (is (equal
       ;; Sans compiler macro.
       (locally (declare (optimize (speed 0) (safety 3) (debug 3)))
         (mapply #'cons '((x 1) (y 2))))
       '((x . 1) (y . 2))))
  (is (equal (mapply #'list '((a 1) (b 2)) '((c 3) (d 4)))
             '((a 1 c 3) (b 2 d 4)))))
