(in-package :serapeum)

;; https://groups.google.com/forum/#!msg/comp.lang.lisp/CM3MQkyOTHk/Pl4KPUqfobwJ
(defun array-index-row-major (array row-major-index)
  "The inverse of ARRAY-ROW-MAJOR-INDEX.

Given an array and a row-major index, return a list of subscripts.

     (apply #'aref (array-index-row-major i))
     ≡ (array-row-major-aref i)"
  (declare (array-index row-major-index)
           (optimize (speed 3) (safety 1)))
  (nlet rec ((subs (list row-major-index))
             (dims (reverse (rest (array-dimensions array)))))
    (if (null dims) subs
        (multiple-value-bind (q r)
            (truncate (the array-index (car subs))
                      (the (integer 0 #.array-dimension-limit)
                           (car dims)))
          (rec (cons q (rplaca subs r))
               (cdr dims))))))
