(in-package #:serapeum)

;;; For SBCL, at least, inlining the functions is enough to make the
;;; element type of the vector constant when the args are constant.

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
  (cond ((and end-supplied? step-supplied?)
         (count-range/3 start end step))
        (end-supplied?
         (count-range/2 start end))
        (t
         (count-range/1 start))))

(define-compiler-macro count-range (start &optional
                                          (end nil end-supplied?)
                                          (by nil by-supplied?))
  (cond ((and end-supplied? by-supplied?)
         `(count-range/3 ,start ,end ,by))
        (end-supplied?
         `(urange/2 ,start ,end))
        (t
         `(count-range/1 ,start))))

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

(progn
  (defmacro define-real-range (name &body (&key type))
    `(progn
       (-> ,name (,type ,type ,type) (vector ,type))
       (defun ,name (start stop step)
         (declare (type ,type start stop step))
         (lret ((vect (make-array 10
                                  :adjustable t
                                  :fill-pointer 0
                                  :element-type ',type)))
           (loop for n of-type ,type from start below stop by step
                 do (vector-push-extend n vect))))))

  (define-real-range real-range
    :type real)

  (define-real-range single-float-range
    :type single-float)

  (define-real-range double-float-range
    :type double-float))

(defun range (start &optional (stop 0 stop?) (step 1))
  (declare (optimize (debug 0) (safety 1)))
  (multiple-value-bind (start stop)
      (if stop?
          (values start stop)
          (values 0 start))
    (dispatch-case ((start real) (stop real) (step real))
      ((non-negative-integer non-negative-integer non-negative-integer)
       (locally (declare (notinline count-range/3))
         (count-range/3 start stop step)))
      ((ratio ratio ratio)
       (real-range start stop step))
      ((single-float single-float single-float)
       (single-float-range start stop step))
      ((double-float double-float double-float)
       (double-float-range start stop step))
      ((float float float)
       ;; Do float contagion in advance.
       (let ((zero
               (+ (* 0 start)
                  (* 0 stop)
                  (* 0 step))))
         (range (+ zero start)
                (+ stop stop)
                (+ zero step))))

      ;; Ensure correct contagion for mixtures of rationals and
      ;; floats.
      ((rational float float)
       (range (float start 0l0)
              stop
              step))
      ((rational float rational)
       (range (float start stop)
              stop
              (float step stop)))
      ((rational rational float)
       (range (float start step)
              (float stop step)
              step))
      ((float float rational)
       (range start
              stop
              (float step 0l0)))
      ((float rational float)
       (range start
              (float stop 0l0)
              step))
      ((float rational rational)
       (range start
              (float stop start)
              (float step start))))))
