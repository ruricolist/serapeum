(in-package :serapeum)

(export '(octet octet-vector octet-vector-p
          make-octet-vector octets unoctets))

(declaim (optimize speed))

(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-vector (&optional length)
  "An array of octets."
  `(simple-array octet (,length)))

(declaim (inline octet-vector-p))
(defun octet-vector-p (x)
  "Is X an octet vector?"
  (typep x 'octet-vector))

(-> make-octet-vector (array-length) octet-vector)
(defun make-octet-vector (size)
  "Make an octet vector of SIZE elements."
  (declare (array-index size))
  (make-array size :element-type 'octet))

(-> random-octet-vector (array-length) octet-vector)
(defun random-octet-vector (size)
  (map-into (make-octet-vector size)
            (lambda () (random 256))))

;;; Adapted from Ironclad.
(-> octets (integer &key (:big-endian t)) octet-vector)
(defun octets (n &key big-endian)
  "Return N, an integer, as an octet vector.
Defaults to little-endian order."
  (let* ((n-bits (integer-length n))
         (n-bytes (ceiling n-bits 8))
         (vec (make-octet-vector n-bytes)))
    (declare (octet-vector vec))
    (if big-endian
        (loop for i from (1- n-bytes) downto 0
              for j from 0
              do (setf (aref vec j)
                       (ldb (byte 8 (* i 8)) n)))
        (loop for i from 0 below n-bytes
              for byte from 0 by 8
              do (setf (aref vec i)
                       (ldb (byte 8 byte) n))))
    vec))

(-> unoctets (octet-vector &key (:big-endian t)) integer)
(defun unoctets (bytes &key big-endian)
  "Concatenate BYTES, an octet vecotor, into an integer.
Defaults to little-endian order."
  (declare (octet-vector bytes) (inline reduce))
  (if big-endian
      (reduce (lambda (sum octet)
                (+ octet (ash sum 8)))
              bytes
              :initial-value 0)
      (loop for i from (1- (length bytes)) downto 0
            sum (ash (aref bytes i) (* i 8)))))

(assert
 (= (unoctets (octets (random-in-range most-negative-fixnum
                                       most-positive-fixnum)))))
