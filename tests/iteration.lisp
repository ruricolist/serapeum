(in-package :serapeum.tests)

(def-suite iteration :in serapeum)
(in-suite iteration)

(test nlet-scope
  "Make sure variables in nlet can be closed over."
  (is (equal '(3 2 1)
             (mapcar 'funcall
                     (nlet rec ((i 3)
                                (acc nil))
                       (if (= i 0)
                           (nreverse acc)
                           (rec (1- i) (cons (lambda () i) acc))))))))

(test defloop-scope
  "Make sure variables in defloop can be closed over."
  (local
    (defloop rec (i &optional acc)
      (if (= i 0)
          (nreverse acc)
          (rec (1- i) (cons (lambda () i) acc))))

    (is (equal '(3 2 1)
               (mapcar 'funcall
                       (rec 3))))))

(test collecting
  (is (equal '(0 1 2 3 4)
             (collecting
               (dotimes (i 5)
                 (collect i))))))

(test with-collectors
  (is (equal '((1) (2) (3))
             (multiple-value-list
              (with-collectors (x y z)
                (x 1) (y 2) (z 3))))))

(test summing
  (is (= (summing) 0))
  (is (= (summing 0.0) 0.0))
  (is (= (summing 0.0 (sum 1)) 1.0))
  (is-true (equal 6 (summing (sum 1) (sum 2) (sum 3))))
  (summing (sum 1) (sum 2) (sum 3)
    (is (= (sum) 6)))
  (is-true (equal 16 (summing 10 (sum 1) (sum 5))))
  (summing 10 (sum 1) (sum 5)
    (is (= (sum) 16))))
