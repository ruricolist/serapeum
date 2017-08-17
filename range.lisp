(in-package #:serapeum)

;;; For SBCL, at least, inlining the functions is enough to make the
;;; element type of the vector constant when the args are constant.

(declaim (inline range/1 range/2 range/3))

(defun range/1 (end)
  (declare (type array-index end))
  (if (= end 0) #()
      (lret* ((type `(integer 0 (,end)))
              (vector (make-array end :element-type type
                                      :initial-element 0)))
        (nlet rec ((i 0))
          (declare (type array-index i))
          (if (= i end) vector
              (progn
                (setf (aref vector i) i)
                (rec (1+ i))))))))

(defun range/2 (start end)
  (declare (type array-index start end))
  (lret* ((type `(integer ,start (,end)))
          (len (- end start))
          (vector (make-array len :element-type type)))
    (nlet rec ((i 0)
               (n start))
      (declare (type array-index i n))
      (if (= i len) vector
          (progn
            (setf (aref vector i) n)
            (rec (1+ i)
                 (1+ n)))))))

(defun range/3 (start end step)
  (declare (type array-index start end step))
  (cond ((= start end) #())
        ((< start end)
         (lret* ((type `(integer ,start (,end)))
                 (len (ceiling (- end start) step))
                 (vector (make-array len :element-type type
                                         :initial-element start)))
           (nlet rec ((i 0)
                      (n start))
             (declare (type array-index i n))
             (if (= i len) vector
                 (progn
                   (setf (aref vector i) n)
                   (rec (1+ i)
                        (+ n step)))))))
        (t (error 'arithmetic-error
                  :operands (list start end step)
                  :operation 'range))))

(defun range (start &optional (end nil end-supplied?)
                              (step 1 step-supplied?))
  "Return a vector of integers.

With one argument, return all the integers in the interval [0,end).

With two arguments, return all the integers in the interval [start,end).

With three arguments, return the integers in the interval [start,end)
whose difference from START is evenly divisible by STEP.

Useful mostly for generating test data.
"
  (declare (type array-index start))
  (declare (type (or null array-index) start end))
  (cond ((and end-supplied? step-supplied?)
         (range/3 start end step))
        (end-supplied?
         (range/2 start end))
        (t
         (range/1 start))))

;; (define-compiler-macro range (start &optional
;;                                     (end nil end-supplied?)
;;                                     (by nil by-supplied?))
;;   (cond ((and end-supplied? by-supplied?)
;;          `(range/3 ,start ,end ,by))
;;         (end-supplied?
;;          `(range/2 ,start ,end))
;;         (t
;;          `(range/1 ,start))))
