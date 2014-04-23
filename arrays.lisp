(in-package :serapeum)

(export '(array-index-row-major undisplace-array))

;; https://groups.google.com/forum/#!msg/comp.lang.lisp/CM3MQkyOTHk/Pl4KPUqfobwJ
(defun array-index-row-major (array row-major-index)
  "The inverse of ARRAY-ROW-MAJOR-INDEX.

Given an array and a row-major index, return a list of subscripts.

     (apply #'aref (array-index-row-major i))
     ≡ (array-row-major-aref i)"
  (declare (array-index row-major-index))
  (reduce (lambda (dim subscripts)
            (nconc (multiple-value-list (truncate (car subscripts) dim))
                   (cdr subscripts)))
          (cdr (array-dimensions array))
          :initial-value (list row-major-index)
          :from-end t))

(assert (let ((a (make-array '(7 4 9 5)))
              (subscripts '(3 2 8 1)))
          (equal subscripts
                 (array-index-row-major
                  a
                  (apply #'array-row-major-index a subscripts)))))

;;; https://groups.google.com/forum/#!original/comp.lang.lisp/JF3M5kA7_vo/g3oW1UuQJ_UJ
(defun undisplace-array (array)
  "Recursively get the fundamental array that ARRAY is displaced to.

Return the fundamental array, and the start and end positions into it.

Borrowed from Erik Naggum."
  (let ((length (length array))
        (start 0))
    (loop
      (multiple-value-bind (to offset) (array-displacement array)
        (if to
            (setq array to
                  start (+ start offset))
            (return (values array start (+ start length))))))))
