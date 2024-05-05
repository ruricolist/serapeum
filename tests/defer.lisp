(in-package :serapeum.tests)

(test defer-ordering
  (let ((output (queue)))
    (with-defer (:as 'outer)
      (with-defer ()
        (defer (enq 'world output) :to 'outer)
        (defer (enq 'hello output))))
    (is (equal (qlist output) '(hello world)))))
