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

(test (static-load-time-value-ccl-regression :compile-at :run-time)
  ;; Cf. https://trac.clozure.com/ccl/ticket/1317
  (finishes
    (let* ((body
             '(let ((cache (static-load-time-value (cons (cons nil nil) nil))))
               (let ((fn #'(lambda () t)))
                 (setf (car cache) (cons fn t))
                 (values (funcall (caar cache))))))
           (thunk (compile nil `(lambda () ,body))))
      (funcall thunk))))

(defgeneric generic-with-no-methods (x))

(test no-applicable-method-error
  (is (proper-subtype-p 'no-applicable-method-error 'error))
  (is (eql 'caught
           (handler-case
               (generic-with-no-methods 1)
             (no-applicable-method-error ()
               'caught)))))
