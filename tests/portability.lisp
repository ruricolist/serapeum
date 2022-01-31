(in-package :serapeum.tests)

(def-suite portability :in serapeum)
(in-suite portability)

(test (static-load-time-value :compile-at :run-time)
  (let* ((fn (eval '(lambda ()
                     (static-load-time-value (random most-positive-fixnum))))))
    (finishes
      (handler-case
          (let ((try1 (list (funcall fn) (funcall fn) (funcall fn))))
            (is (every #'eql try1 (rest try1)))
            (setf fn (compile nil fn))
            (is (compiled-function-p fn))
            (let ((try2 (list (funcall fn) (funcall fn) (funcall fn))))
              (is (every #'eql try2 (rest try2)))))
        #-(or sbcl ccl ecl clasp)
        (static-load-time-value-error ())))))
