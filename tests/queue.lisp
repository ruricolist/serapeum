
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

(test setf-front
  (let ((q (queue)))
    (is (null (front q)))
    (setf (front q) 1)
    (is (equal 1 (front q)))
    (is (equal '(1) (qlist q))))
  (let ((q (queue 2 2)))
    (setf (front q) 1)
    (is (eql (front q) 1))
    (is (equal '(1 2) (qlist q)))))

(test qback
  (let ((q (queue)))
    (is (null (qback q)))
    (enq 1 q)
    (is (eql 1 (qback q))))
  (let ((q (queue 1)))
    (is (eql 1 (qback q)))
    (is (equal '(1) (qlist q)))))

(test setf-qback
  (let ((q (queue)))
    (is (null (qback q)))
    (setf (qback q) 1)
    (is (eql 1 (qback q)))
    (is (equal '(1) (qlist q))))
  (let ((q (queue 1)))
    (is (eql 1 (qback q)))
    (setf (qback q) 2)
    (is (eql 2 (qback q)))
    (is (equal '(2) (qlist q))))
  (let ((q (queue 1 2)))
    (is (eql 2 (qback q)))
    (setf (qback q) 3)
    (is (eql 3 (qback q)))
    (is (equal '(1 3) (qlist q)))))

(test copy-queue
  (let* ((q1 (queue 1 2 3))
         (q2 (copy-queue q1)))
    (is (not (eq (qlist q1) (qlist q2))))
    (enq 4 q1)
    (enq 2 q2)
    (is (equal (qlist q1) '(1 2 3 4)))
    (is (equal (qlist q2) '(1 2 3 2)))))

(test qprepend
  (let ((q (queue)))
    (qprepend '(1 2 3) q)
    (is (equal (qlist q) '(1 2 3))))
  (let ((q (queue 4 5 6)))
    (qprepend '(1 2 3) q)
    (is (equal (qlist q) '(1 2 3 4 5 6)))))

(test qpreconc
  (let ((q (queue)))
    (qpreconc (list 1 2 3) q)
    (is (equal (qlist q) '(1 2 3))))
  (let ((q (queue 4 5 6)))
    (qpreconc (list 1 2 3) q)
    (is (equal (qlist q) '(1 2 3 4 5 6)))))
