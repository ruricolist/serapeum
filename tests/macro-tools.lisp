(in-package :serapeum.tests)

(def-suite macro-tools :in serapeum)
(in-suite macro-tools)

(test read-only-var
  (let ((x 1))
    (serapeum::with-read-only-vars (x)
      (is (eql x 1)))))

#+(or ccl sbcl cmucl allegro)
(progn
  (test read-only-var-warning
    (signals warning
      (compile nil
               '(lambda ()
                 (let ((x 1))
                   (serapeum::with-read-only-vars (x)
                     (setf x 2)))))))

  (test read-only-var-error
    (signals warning
      (eval*
       '(let ((x 1))
         (serapeum::with-read-only-vars (x)
           (setf x 2))))))

  (defvar *special*)

  #-ccl
  (test read-only-var/special
    (is-false
     (let ((x 1)
           (*special* t))
       (with-read-only-vars (*special*)
         (setf *special* nil))))))
