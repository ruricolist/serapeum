(in-package #:serapeum)

(defconst no-bounds-checks
  '(declare (optimize
             (debug 0)
             (safety 0)
             (compilation-speed 0))))

;;; The spec says that rational and float are disjoint subtypes of
;;; real, but it does not say they are exhaustive.
(deftype real* ()
  '(or rational float))

(defun range-error (start stop step)
  (error 'arithmetic-error
         :operands (list start stop step)
         :operation 'range))

(declaim (inline check-range))
(defun check-range (start stop step)
  (let ((in-order? (<= start stop)))
    (when (or (zerop step)
              (eif in-order? (minusp step) (plusp step)))
      (RANGE-ERROR start stop step))))
(declaim (notinline check-range))

(defmacro check-range/inline (start stop step)
  `(locally (declare (inline check-range))
     (check-range ,start ,stop ,step)))

;;; For SBCL, at least, inlining the functions is enough to make the
;;; element type of the vector constant when the args are constant.

(defsubst count-range/1 (end)
  #.no-bounds-checks
  (declare (type array-index end))
  (check-range/inline 0 end 1)
  (if (= end 0) #()
      (lret* ((type `(integer 0 (,end)))
              (vector (make-array end :element-type type
                                      :initial-element 0)))
        (nlet lp ((i 0))
          (declare (type array-index i))
          (if (= i end) vector
              (progn
                (setf (aref vector i) i)
                (lp (1+ i))))))))

(defsubst count-range/2 (start end)
  #.no-bounds-checks
  (declare (type array-index start end))
  (check-range/inline start end 1)
  (lret* ((type `(integer ,start (,end)))
          (len (- end start))
          (vector (make-array len :element-type type)))
    (nlet lp ((i 0)
              (n start))
      (declare (type array-index i n))
      (if (= i len) vector
          (progn
            (setf (aref vector i) n)
            (lp (1+ i)
                (1+ n)))))))

(defsubst count-range/3 (start end step)
  #.no-bounds-checks
  (declare (type array-index start end step))
  (check-range/inline start end step)
  (cond ((= start end) #())
        ((< start end)
         (lret* ((type `(integer ,start (,end)))
                 (len (ceiling (- end start) step))
                 (vector (make-array len :element-type type
                                         :initial-element start)))
           (nlet lp ((i 0)
                     (n start))
             (declare (type array-index i n))
             (if (= i len) vector
                 (progn
                   (setf (aref vector i) n)
                   (lp (1+ i)
                       (+ n step)))))))
        (t (range-error start end step))))

(defsubst count-range (start &optional (end nil end-supplied?)
                                       (step 1 step-supplied?))
  "Return a vector of ascending positive integers.

With one argument, return all the integers in the interval [0,end).

With two arguments, return all the integers in the interval [start,end).

With three arguments, return the integers in the interval [start,end)
whose difference from START is evenly divisible by STEP."
  (declare (type array-index start))
  (declare (type (or null array-index) start end))
  (check-range/inline start end step)
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
  (let ((by (eval-if-constant by env)))
    (cond ((and by-supplied? (not (eql 1 by)))
           `(count-range/3 ,start ,end ,by))
          (end-supplied?
           `(count-range/2 ,start ,end))
          (t
           `(count-range/1 ,start)))))

(defun int-range-shape (start stop step)
  "Return the length and element type for a range vector from START to
STOP by STEP."
  (mvlet* ((low high
            (sort-values #'< start stop))
           (in-order? (eql low start))
           (element-type
            (if in-order?
                `(integer ,low (,high))
                `(integer (,low) ,high)))
           (len (ceiling (abs (- high low))
                         (abs step))))
    (values len element-type)))

(defmacro with-int-vector ((var vect) &body body)
  `(let ((,var ,vect))
     (with-type-dispatch
         ((simple-array bit                (*))
          (simple-array (unsigned-byte 2)  (*))
          (simple-array (unsigned-byte 4)  (*))
          (simple-array (unsigned-byte 7)  (*))
          (simple-array (signed-byte 8)    (*))
          (simple-array (unsigned-byte 8)  (*))
          (simple-array (signed-byte 16)   (*))
          (simple-array (unsigned-byte 16) (*))
          (simple-array (signed-byte 32)   (*))
          (simple-array (unsigned-byte 32) (*))
          (simple-array fixnum             (*))
          (simple-array integer            (*)))
         ,var
       ,@body)))

(-> fill-int-range! (vector integer integer integer) vector)
(defun fill-int-range! (vect start stop step)
  #.no-bounds-checks
  (let ((in-order? (<= start stop)))
    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
    (with-int-vector (vect vect)
      (with-boolean (in-order?)
        (nlet lp ((i 0)
                  (n start))
          (if (boolean-if in-order?
                          (>= n stop)
                          (<= n stop))
              vect
              (progn
                (setf (vref vect i) n)
                (lp (1+ i) (+ n step)))))))
    vect))

(defun prepare-int-range (start stop step)
  (multiple-value-bind (len element-type)
      (int-range-shape start stop step)
    (make-array len :element-type element-type)))

(-> integer-range (integer integer integer) vector)
(defun integer-range (start stop step)
  (check-range start stop step)
  (lret ((vect (prepare-int-range start stop step)))
    (fill-int-range! vect start stop step)))

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
         (lret* ((len (max 1 (abs (ceiling (/ (- stop start) step)))))
                 (vect (make-array len :element-type ',type)))
           (loop for i from 0 below len
                 for n = (+ start (* step i))
                 do (setf (aref vect i) n))))))

  (define-real-range real-range
    :type real*)

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
  "Return a (possibly specialized) vector of real numbers, starting from START.

With three arguments, return the integers in the interval [start,end)
whose difference from START is divisible by STEP.

START, STOP, and STEP can be any real number, except that if STOP is
greater than START, STEP must be positive, and if START is greater
than STOP, STEP must be negative.

The vector returned has the smallest element type that can represent
numbers in the given range. E.g. the range [0,256) will usually be
represented by a vector of octets, while the range \[-10.0,10.0) will
be represented by a vector of single floats. The exact representation,
however, depends on your Lisp implementation.

STEP defaults to 1.

With two arguments, return all the steps in the interval [start,end).

With one argument, return all the steps in the interval [0,end)."
  (declare (optimize (debug 0) (safety 1))
           #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note)
           (notinline count-range/1 count-range/2 count-range/3))
  (multiple-value-bind (start stop)
      (if stop?
          (values start stop)
          (values 0 start))
    (check-range start stop step)
    (dispatch-case ((start real*) (stop real*) (step real*))
      ((array-index array-index non-negative-integer)
       (if (<= start stop)
           (count-range/3 start stop step)
           (integer-range start stop step)))

      ((non-negative-integer non-negative-integer non-negative-integer)
       (integer-range start stop step))

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
      ;; floats.
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
  (let ((start (eval-if-constant start env))
        (stop  (eval-if-constant stop  env))
        (step  (eval-if-constant step  env)))
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
     ;; Expand directly into an integer range when possible.
     (and (typep start 'integer)
          (typep stop 'integer)
          (typep step 'integer)
          (progn
            (check-range start stop step)
            (with-unique-names (vect)
              (multiple-value-bind (len element-type)
                  (int-range-shape start stop step)
                `(lret ((,vect (make-array ,len :element-type ',element-type)))
                   (fill-int-range! ,vect ,start ,stop ,step))))))

     call)))
