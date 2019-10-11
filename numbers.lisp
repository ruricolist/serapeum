(in-package :serapeum)

(defsubst fixnump (n)
  "Same as `(typep N 'fixnum)'."
  (typep n 'fixnum))

(define-post-modify-macro finc (&optional (delta 1)) +
  "Like `incf', but returns the old value instead of the new.

An alternative to using -1 as the starting value of a counter, which
can prevent optimization.")

(define-post-modify-macro fdec (&optional (delta 1)) -
  "Like `decf', but returns the old value instead of the new.")

;;; Loose adaption of the parser in SBCL's reader.lisp.

(defun make-float (number divisor &optional (format *read-default-float-format*))
  (assert (subtypep format 'float))
  (coerce (/ number divisor) format))

(defun exponent-char? (char)
  (case char
    ((#\e #\s #\f #\l #\d) t)
    ((#\E #\S #\F #\L #\D) t)
    (t nil)))

(defun exponent-char-format (exponent-char)
  (ecase (char-downcase exponent-char)
    (#\e *read-default-float-format*)
    (#\s 'short-float)
    (#\f 'single-float)
    (#\d 'double-float)
    (#\l 'long-float)))

(defun read-float-aux (stream junk-allowed type)
  (declare (optimize (speed 3)))
  #.+merge-tail-calls+
  (labels ((junk ()
             (error "Junk in string"))
           (next ()
             (let ((char (read-char stream nil nil)))
               (values char (and char (digit-char-p char)))))
           (before-decimal (number)
             (multiple-value-bind (char digit) (next)
               (cond ((null char)
                      (coerce number type))
                     ((eql char #\.)
                      (after-decimal number 1))
                     (digit
                      (before-decimal (+ (* number 10) digit)))
                     (t (if junk-allowed
                            number
                            (junk))))))
           (after-decimal (number divisor)
             (multiple-value-bind (char digit) (next)
               (cond ((null char)
                      (make-float number divisor type))
                     ((exponent-char? char)
                      (exponent number divisor 0
                                (exponent-char-format char)))
                     (digit
                      (after-decimal (+ (* number 10) digit)
                                     (* divisor 10)))
                     (t (if junk-allowed
                            (make-float number divisor)
                            (junk))))))
           (exponent (n d e f &optional neg)
             (multiple-value-bind (char digit) (next)
               (cond ((null char)
                      (let ((e (if neg (- e) e)))
                        (make-float (* (expt 10 e) n) d f)))
                     ((eql char #\+)
                      (exponent n d e f nil))
                     ((eql char #\-)
                      (exponent n d e f t))
                     (digit
                      (exponent n d (+ (* e 10) digit) f neg))
                     (t (if junk-allowed
                            (let ((e (if neg (- e) e)))
                              (make-float (* (expt 10 e) n) d f))
                            (junk)))))))
    (before-decimal 0)))

(defun read-float (stream junk-allowed type)
  (let ((char (read-char stream nil nil)))
    (cond ((null char)
           (if junk-allowed
               (coerce 0 type)
               (error "Empty string cannot be parsed as float")))
          ((eql char #\+) (read-float-aux stream junk-allowed type))
          ((eql char #\-) (- (read-float-aux stream junk-allowed type)))
          ((digit-char-p char)
           (unread-char char stream)
           (read-float-aux stream junk-allowed type))
          (t (if junk-allowed
                 (coerce 0 type)
                 (error "Junk in string"))))))

(defun parse-float (string &key (start 0) (end (length string)) junk-allowed
                                (type *read-default-float-format* type-supplied-p))
  "Parse STRING as a float of TYPE.

The type of the float is determined by, in order:
- TYPE, if it is supplied;
- The type specified in the exponent of the string;
- or `*read-default-float-format*'.

     (parse-float \"1.0\") => 1.0s0
     (parse-float \"1.0d0\") => 1.0d0
     (parse-float \"1.0s0\" :type 'double-float) => 1.0d0

Of course you could just use `parse-number', but sometimes only a
float will do."
  (assert (subtypep type 'float))
  (with-input-from-string (stream string :start start :end end)
    (let ((float (read-float stream junk-allowed type)))
      (if type-supplied-p
          (coerce float type)
          float))))

;;; When parse-float is called with a constant `:type' argument, wrap
;;; it in a `the' form.

(define-compiler-macro parse-float (&whole decline string &rest args
                                           &key type
                                           &allow-other-keys)
  (if (and type (constantp type))
      (let ((type (eval type)))
        (assert (subtypep type 'float))
        `(locally (declare (notinline parse-float))
           (truly-the ,type
             (parse-float ,string ,@args))))
      decline))

(declaim (inline round-to))
(defun round-to (number &optional (divisor 1))
  "Like `round', but return the resulting number.

     (round 15 10) => 2
     (round-to 15 10) => 20"
  (* (round number divisor) divisor))

(declaim-constant-function round-to)

(defun bits (int &key big-endian)
  "Return a bit vector of the bits in INT.
Defaults to little-endian."
  (let ((bits (make-array (integer-length int)
                          :element-type 'bit)))
    (with-subtype-dispatch integer
      ((unsigned-byte 32) (unsigned-byte 64) fixnum)
      int
      (if big-endian
          (loop for i below (integer-length int)
                for j downfrom (1- (integer-length int)) to 0
                do (setf (sbit bits j)
                         (if (logbitp i int)
                             1
                             0)))
          (loop for i below (integer-length int)
                do (setf (sbit bits i)
                         (if (logbitp i int)
                             1
                             0)))))
    bits))

(defun unbits (bits &key big-endian)
  "Turn a sequence of BITS into an integer.
Defaults to little-endian."
  (declare (bit-vector bits))
  (with-type-dispatch (bit-vector simple-bit-vector) bits
    (if big-endian
        (reduce (lambda (x y)
                  (+ (ash x 1) y))
                bits)
        (loop with int = 0
              for bit across bits
              for i from 0
              do (setf int (logior int (ash bit i)))
              finally (return int)))))

(declaim (inline shrink grow))

(defun shrink (n by)
  "Decrease N by a factor."
  (- n (* n by)))

(defun grow (n by)
  "Increase N by a factor."
  (+ n (* n by)))

(define-modify-macro shrinkf (n) shrink
  "Shrink the value in a place by a factor.")

(define-modify-macro growf (n) grow
  "Grow the value in a place by a factor.")

(defun random-in-range (low high)
  "Random number in the range [low,high).

LOW and HIGH are automatically swapped if HIGH is less than LOW.

Note that the value of LOW+HIGH may be greater than the range that can
be represented as a number in CL. E.g., you can generate a random double float with

    (random-in-range most-negative-double-float most-positive-double-float)

even though (+ most-negative-double-float most-positive-double-float)
would cause a floating-point overflow.

From Zetalisp."
  (when (> low high)
    (rotatef low high))
  (when (= low high)
    (error 'arithmetic-error
           :operation 'random-in-range
           :operands (list low high)))
  (if (and (minusp low) (plusp high))
      ;; Arrange for float contagion if LOW and HIGH are of different
      ;; precisions. E.g. (random-in-range most-negative-single-float
      ;; most-positive-double-float) should return the same range of
      ;; results as (random-in-range (coerce
      ;; most-negative-single-float 'double-float)
      ;; most-positive-double-float).
      (let ((low (+ low (* 0 high)))
            (high (+ high (* 0 low))))
        ;; We do it this way lest low+high exceed the possible size of a
        ;; float. E.g. (random-in-range most-negative-double-float
        ;; most-positive-double-float) should work.
        (+ (- (random (abs low)))
           (random high)))
      (let ((range (- high low)))
        (+ low (random range)))))

(defun random-range-type (low high)
  (assert (< low high))
  (let ((types-worth-checking
          '#.(remove-duplicates
              '(integer single-float short-float double-float long-float)
              :test #'type=
              :from-end t)))
    ;; Low and high have the same type.
    (when-let (interval-type
               (find-if (lambda (type)
                          (and (typep low type)
                               (typep high type)))
                        types-worth-checking))
      (let ((type `(,interval-type ,low (,high))))
        (assert (subtypep type 'number))
        (assert (subtypep type interval-type))
        ;; Note (high) is exclusive.
        (assert (not (typep high type)))
        (assert (typep low type))
        type))))

(define-compiler-macro random-in-range (&whole call low high)
  "When LOW and HIGH are both constants, declare the type of the
result."
  (when (constantp low)
    (setf low (eval low)))
  (when (constantp high)
    (setf high (eval high)))
  (unless (and (numberp low) (numberp high))
    (return-from random-in-range
      call))
  (when (> low high)
    (rotatef low high))
  (when (= low high)
    (error 'arithmetic-error
           :operation 'random-in-range
           :operands (list low high)))
  (if-let (type (random-range-type low high))
    `(locally (declare (notinline random-in-range))
       (truly-the ,type
         (random-in-range ,low ,high)))
    call))

(defun float-precision-contagion (&rest ns)
  "Perform numeric contagion on the elements of NS.

That is, if any element of NS is a float, then every number in NS will
be returned as \"a float of the largest format among all the
floating-point arguments to the function\".

This does nothing but numeric contagion: the number of arguments
returned is the same as the number of arguments given."
  (declare (dynamic-extent ns))
  (let* ((zeros (mapcar (op (* _ 0)) ns))
         (zero (apply #'+ zeros)))
    (mapcar (op (+ _ zero))
            ns)))
