(in-package :serapeum.tests)

(def-suite op :in serapeum)
(in-suite op)

(test op
  ;; Constantly.
  (is (= (funcall (op 1)) 1))
  ;; Identity.
  (is (= (funcall (op _) 1) 1))
  ;; Positional.
  (is (= (funcall (op (+ 1 _)) 1) 2))
  ;; Backward reference.
  (is (= (funcall (op (+ _ _1)) 2) 4))
  ;; Rest.
  (is (equal (apply (op (list 1 _*)) '(2 3)) '(1 2 3)))
  ;; Positional and rest.
  (is (equal (apply (op (list _ _*)) 1 '(2 3)) '(1 2 3)))
  ;; Flip
  (is (eql 4 (find '(4) (range 10) :test (op (member _2 _1)))))
  ;; nth-arg
  (is (equal '(4 5 6) (mapcar (op _2) '(1 2 3) '(4 5 6))))
  ;; Sparse argument lists.
  (is (= 9 (apply (op (+ _1 _3 _5)) '(1 2 3 4 5))))
  ;; Backquotes.
  (is (equal '((:x 1) (:x 2) (:x 3)) (mapcar (op `(:x ,_)) '(1 2 3)))))

(test nested-op
  (signals warning
    (eval*
     '(op (list _1 (map nil (op (print _2)) _1))))))
