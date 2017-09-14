(in-package #:serapeum)

;;; For SBCL, at least, inlining the functions is enough to make the
;;; element type of the vector constant when the args are constant.

(defsubst count-range/1 (end)
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

(defsubst count-range/2 (start end)
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

(defsubst count-range/3 (start end step)
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

(defsubst count-range (start &optional (end nil end-supplied?)
                                       (step 1 step-supplied?))
  "Return a vector of integers.

With one argument, return all the integers in the interval [0,end).

With two arguments, return all the integers in the interval [start,end).

With three arguments, return the integers in the interval [start,end)
whose difference from START is evenly divisible by STEP.

Useful mostly for generating test data."
  (declare (type array-index start))
  (declare (type (or null array-index) start end))
  (assert (not (zerop step)))
  (cond (step-supplied?
         (count-range/3 start end step))
        (end-supplied?
         (count-range/2 start end))
        (t
         (count-range/1 start))))

(define-compiler-macro count-range (start &optional
                                          (end nil end-supplied?)
                                          (by nil by-supplied?)
                                          &environment env)
  (let ((by (eval-constant by env)))
    (cond ((and by-supplied? (not (eql 1 by)))
           `(count-range/3 ,start ,end ,by))
          (end-supplied?
           `(count-range/2 ,start ,end))
          (t
           `(count-range/1 ,start)))))

(-> simple-range (real real real) (vector rational))
(defun simple-range (start end step)
  (lret ((vect (make-array 10
                           :adjustable t
                           :fill-pointer 0)))
    (if (< end start)
        (progn
          (assert (minusp step))
          (let ((step (abs step)))
            (loop for n downfrom start above end by step do
              (vector-push-extend n vect))))
        (loop for n from start below end by step do
          (vector-push-extend n vect)))))

(-> integer-range (integer integer integer) vector)
(defun integer-range (start stop step)
  (mvlet* ((low high
            (sort-values #'< start stop))
           (in-order? (eql low start))
           (element-type
            (if in-order?
                `(integer ,low (,high))
                `(integer (,low) ,high)))
           (len (abs (floor (abs (- high low))
                            (abs step))))
           (vect (make-array len :element-type element-type)))
    (if in-order?
        (loop for i from 0
              for n from low below high by step
              do (setf (aref vect i) n))
        (loop for i from 0
              for n downfrom high above low by (abs step)
              do (setf (aref vect i) n)))
    vect))

(progn
  (defmacro define-real-range (name &body (&key type))
    `(progn
       (-> ,name (,type ,type ,type) (vector ,type))
       (defun ,name (start stop step)
         (declare (type ,type start stop step))
         ;; Real ranges are filled by multiplying the step, rather by
         ;; adding it, because, for floating point numbers, if the
         ;; starting point is sufficiently large, and the step is
         ;; sufficiently small, then the result may be identical to
         ;; the starting point.
         (lret* ((len (max 1 (abs (floor (/ (- stop start) step)))))
                 (vect (make-array len :element-type ',type)))
           (loop for i from 0 below len
                 for n = (+ start (* step i))
                 do (setf (aref vect i) n))))))

  (define-real-range real-range
    :type real)

  (define-real-range short-float-range
    :type short-float)

  (define-real-range single-float-range
    :type single-float)

  (define-real-range double-float-range
    :type double-float)

  (define-real-range long-float-range
    :type long-float))

(defsubst frange (start stop step)
  "Perform floating-point contagion on START, STOP, and STEP, and call
`range' on the results."
  (apply #'range (float-precision-contagion start stop step)))

(defun range (start &optional (stop 0 stop?) (step 1))
  (declare (optimize (debug 0) (safety 1))
           #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note)
           (notinline count-range/1 count-range/2 count-range/3))
  (multiple-value-bind (start stop)
      (if stop?
          (values start stop)
          (values 0 start))
    (dispatch-case ((start real) (stop real) (step real))
      ((non-negative-integer non-negative-integer non-negative-integer)
       (if (<= start stop)
           (count-range/3 start stop step)
           (integer-range start stop step)))

      ((integer integer integer)
       (integer-range start stop step))

      ((ratio rational rational)
       (real-range start stop step))
      ((rational ratio rational)
       (real-range start stop step))
      ((rational rational ratio)
       (real-range start stop step))

      ((single-float single-float single-float)
       (single-float-range start stop step))
      ((double-float double-float double-float)
       (double-float-range start stop step))
      ((short-float short-float short-float)
       (short-float-range start stop step))
      ((long-float long-float long-float)
       (long-float-range start stop step))
      ;; Mixtures of different kinds of floats.
      ((float float float)
       ;; Do float contagion in advance.
       (frange start stop step))

      ;; Ensure correct contagion for mixtures of rationals and
      ;; floats. Coerce to double floats to start with, so we don't needlessly
      ;; lose precision.
      ((rational float float)
       (frange start stop step))
      ((rational float rational)
       (frange start stop step))
      ((rational rational float)
       (frange start stop step))
      ((float float rational)
       (frange start stop step))
      ((float rational float)
       (frange start stop step))
      ((float rational rational)
       (frange start stop step)))))

(define-compiler-macro range
    (&whole call
            start &optional (stop 0 stop?) (step 1 step?)
            &environment env)
  (let ((start (eval-constant start env))
        (stop  (eval-constant stop  env))
        (step  (eval-constant step  env)))
    (or
     ;; Expand directly into count-range when possible.
     (and (typep start 'array-index)
          (if (not stop?)
              `(count-range ,start)
              (and (typep stop 'array-index)
                   (<= start stop)
                   (if (not step?)
                       `(count-range ,start ,stop)
                       (and (typep step 'array-index)
                            `(count-range ,start ,stop ,step))))))
     call)))
