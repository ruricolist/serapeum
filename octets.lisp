(in-package :serapeum)

(export '(octet octet-vector octet-vector-p
          make-octet-vector octets unoctets
          write-u32 read-u32 write-u64 read-u64))

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
(-> octets (integer) octet-vector)
(defun octets (n)
  "Return N, an integer, as a little-endian octet vector."
  (let* ((n-bits (integer-length n))
         (n-bytes (ceiling n-bits 8))
         (vec (make-octet-vector n-bytes)))
    (declare (octet-vector vec))
    (loop for i from 0 below n-bytes
          for byte from 0 by 8
          do (setf (aref vec i)
                   (ldb (byte 8 byte) n))
          finally (return vec))))

(-> unoctets (octet-vector) integer)
(defun unoctets (bytes)
  "Concatenate BYTES into an integer in little-endian order."
  (declare (octet-vector bytes))
  (loop for i from (1- (length bytes)) downto 0
        sum (ash (aref bytes i) (* i 8))))

(assert
 (= (unoctets (octets (random-in-range most-negative-fixnum
                                       most-positive-fixnum)))))
