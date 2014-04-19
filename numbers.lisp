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
          ln lb lg
          phi e
          random-in-range))

(defconst phi (/ (+ 1 (sqrt 5.0d0)) 2))

(defconst e (exp 1.0d0))

(defmacro finc (place &optional (delta 1))
  "Like `incf', but returns the old value instead of the new.

An alternative to using -1 as the starting value of a counter, which
can prevent optimization."
  (with-gensyms (old)
    `(let ((,old ,place))
       (setf ,place (+ ,old ,delta))
       ,old)))

(defmacro fdec (place &optional (delta 1))
  "Like `decf', but returns the old value instead of the new."
  (with-gensyms (old)
    `(let ((,old ,place))
       (setf ,place (- ,old ,delta))
       ,old)))

(defun parse-float (string &key (start 0) (end (length string)) junk-allowed)
  "Based on the venerable `parse-float' from the CMU Lisp repository.
Of course you could just use `parse-number', but sometimes only a
float will do."
  (declare (array-length start end))
  (let ((index (or (position-if-not #'whitespacep string
                                    :start start :end end)
                   (return-from parse-float (values 0.0 end))))
        (minusp nil) (decimalp nil) (found-digit nil)
        (before-decimal 0) (after-decimal 0) (decimal-counter 0)
        (exponent 0)
        (result 0)
        (radix 10))
    (declare (array-length index))
    (let ((char (char string index)))
      (cond ((char= char #\-)
             (setq minusp t)
             (incf index))
            ((char= char #\+)
             (incf index))))
    (loop
      until (= index end)
      for char = (char string index)
      as weight = (digit-char-p char radix)
      do (cond ((and weight (not decimalp))
                (setq before-decimal (+ weight (* before-decimal radix))
                      found-digit t))
               ((and weight decimalp)
                (setq after-decimal (+ weight (* after-decimal radix))
                      found-digit t)
                (incf decimal-counter))
               ((and (char= char #\.) (not decimalp))
                (setq decimalp t))
               ((and (or (char-equal char #\e)
                         (char-equal char #\d)
                         (char-equal char #\f)
                         (char-equal char #\s)
                         (char-equal char #\l))
                     (= radix 10))
                (multiple-value-bind (num idx)
                    (parse-integer string :start (1+ index) :end end
                                          :radix radix :junk-allowed junk-allowed)
                  (setq exponent (or num 0)
                        index idx)
                  (when (= index end) (return nil))))
               ((whitespacep char)
                (when (position-if-not #'whitespacep string
                                       :start (1+ index) :end end)
                  (if junk-allowed
                      (loop-finish)
                      (error "There's junk in this string: ~s." string)))
                (return nil))
               (t (if junk-allowed
                      (loop-finish)
                      (error "There's junk in this string: ~s." string))))
         (incf index))
    (setq result (coerce (* (+ before-decimal
                               (* after-decimal
                                  (coerce (expt radix (- decimal-counter))
                                          'float)))
                            (coerce (expt radix exponent)
                                    'float))
                         'float))
    (values
     (if found-digit
         (if minusp (- result) result)
         0.0)
     index)))

(defsubst round-to (number &optional (divisor 1))
  "Like `round', but return the resulting number.

     (round 15 10) => 2
     (round-to 15 10) => 20"
  (* (round number divisor) divisor))

(defun bits (int)
  "Return a bit vector of the bits in INT."
  (lret ((bits (make-array (integer-length int)
                           :element-type 'bit)))
    (loop for i below (integer-length int)
          for j downfrom (1- (integer-length int)) to 0
          do (setf (sbit bits j)
                   (if (logbitp i int)
                       1
                       0)))))

;; TODO Doesn't work on negative numbers.
(defun unbits (bits)
  "Turn a sequence of BITS into an integer."
  (reduce (lambda (x y)
            (+ (ash x 1) y))
          bits))

(assert (let ((n (random most-positive-fixnum)))
          (= n (unbits (bits n)))))

(defun shrink (n by)
  "Decrease N by a factor."
  (- n (* n by)))

(defun grow (n by)
  "Increase N by a factor."
  (+ n (* n by)))

(define-modify-macro shrinkf (n) shrink
  "Shrink the value in a place by a factor.")

(define-modify-macro growf (n) shrink
  "Grow the value in a place by a factor.")

(defsubst ln (n)
  "Natural logarithm."
  (log n))

(defsubst lb (n)
  "Binary logarithm."
  (log n 2))

(defsubst lg (n)
  "Decimal logarithm."
  (log n 10))

(defsubst random-in-range (low high)
  "Random number in the range [low,high).

From Zetalisp."
  (+ low (random (- high low))))
