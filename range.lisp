(in-package #:serapeum)

;;; For SBCL, at least, inlining the functions is enough to make the
;;; element type of the vector constant when the args are constant.

(defsubst range (start &optional (stop 0 stop?) (step 1))
  "Return a vector of integers.

With one argument, return all the integers in the interval [0,end).

With two arguments, return all the integers in the interval [start,end).

With three arguments, return the integers in the interval [start,end)
whose difference from START is evenly divisible by STEP.

Useful mostly for generating test data."
  (declare (optimize (debug 0) (safety 1)))
  (mvlet* ((start stop
            (if stop?
                (values start stop)
                (values 0 start)))
           (zero  (+ (* start 0) (* stop 0) (* step 0)))
           (start (+ start zero))
           (stop  (+ stop  zero))
           (step  (+ step  zero))
           (lower-limit upper-limit
            (if (<= start stop)
                (values start `(,stop))
                (values `(,stop) start)))
           (len
            (cond ((and (> stop start)
                        (plusp step))
                   (ceiling (abs (- stop start))
                            step))
                  ((minusp step)
                   (ceiling (abs (- stop start))
                            (abs step)))
                  (t 0)))
           (element-type
            (cond ((and (integerp start)
                        (integerp stop))
                   `(integer ,lower-limit ,upper-limit))
                  ((and (typep start 'single-float)
                        (typep stop 'single-float))
                   `(single-float ,lower-limit ,upper-limit))
                  ((and (typep start 'double-float)
                        (typep stop 'double-float))
                   `(double-float ,lower-limit ,upper-limit))
                  (t
                   `(real ,lower-limit ,upper-limit))))
           (array
            (make-array len :element-type element-type)))
    (loop for i from 0 below len
          for n = start then (+ n step)
          do (setf (vref array i) n)
          finally (return array))))
