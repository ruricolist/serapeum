
(in-package :serapeum.tests)

(def-suite queue :in serapeum)
(in-suite queue)

(test qappend
  (let* ((list (list 1 2 3))
         (queue (qappend (queue) list))
         (qlist (qlist queue)))
    (is (not (eq qlist list)))))

(test qconc
  (let* ((list (list 1 2 3))
         (queue (qconc (queue) list))
         (qlist (qlist queue)))
    (is (eq qlist list))))

(test undeq
  (flet ((q= (q1 q2)
           (equal (qlist q1)
                  (qlist q2))))
    (let ((q1 (queue))
          (q2 (queue)))
      (enq 1 q1)
      (undeq 1 q2)
      (is (q= q1 q2)))
    (let ((q1 (queue 1))
          (q2 (queue 2)))
      (enq 2 q1)
      (undeq 1 q2)
      (is (q= q1 q2)))
    (for-all ((len (gen-integer :max 10 :min 1)))
      (let* ((nums (range len))
             (q (multiple-value-call #'queue
                  (values-vector nums))))
        (is (q= q
                (let ((item (deq q)))
                  (undeq item q)
                  q)))))))
