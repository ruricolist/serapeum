(in-package :serapeum)

(export '(parse-number
          parse-real-number
          parse-positive-real-number
          invalid-number
          invalid-number-reason
          invalid-number-value))

(export '(parse-float
          bits unbits
          round-to
          finc fdec
          shrink grow
          shrinkf growf
          random-in-range))

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
  (declare (optimize (speed 3) (debug 0)))
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
- `*read-default-float-format*'

     (parse-float \"1.0\") => 1.0s0
     (parse-float \"1.0d0\") => 1.0d0
     (parse-float \"1.0s0\" :type 'double-float) => 1.0d0

Of course you could just use `parse-number', but sometimes only a
float will do."
  (assert (subtypep type 'float))
  (let* ((stream (make-string-input-stream string start end))
         (float (read-float stream junk-allowed type)))
    (if type-supplied-p
        (coerce float type)
        float)))

;;; When parse-float is called with a constant `:type' argument, wrap
;;; it in a `the' form.

;; Prevent recursive macro-expansion.
(defun parse-float-wrapper (&rest args)
  (apply #'parse-float args))

(define-compiler-macro parse-float (&whole decline string &rest args
                                           &key type
                                           &allow-other-keys)
  (if (and type (constantp type))
      (let ((type (eval type)))
        (assert (subtypep type 'float))
        `(the ,type (parse-float-wrapper ,string ,@args)))
      decline))

(declaim (inline round-to))
(defun round-to (number &optional (divisor 1))
  "Like `round', but return the resulting number.

     (round 15 10) => 2
     (round-to 15 10) => 20"
  (* (round number divisor) divisor))

(defun bits (int &key big-endian)
  "Return a bit vector of the bits in INT.
Defaults to little-endian."
  (let ((bits (make-array (integer-length int)
                          :element-type 'bit)))
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
                           0))))
    bits))

(defun unbits (bits &key big-endian)
  "Turn a sequence of BITS into an integer.
Defaults to little-endian."
  (declare (bit-vector bits))
  (if big-endian
      (reduce (lambda (x y)
                (+ (ash x 1) y))
              bits)
      (loop with int = 0
            for bit across bits
            for i from 0
            do (setf int (logior int (ash bit i)))
            finally (return int))))

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

(declaim (inline random-in-range))
(defun random-in-range (low high)
  "Random number in the range [low,high).

From Zetalisp."
  (+ low (random (- high low))))
