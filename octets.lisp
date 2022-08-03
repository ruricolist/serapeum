(in-package :serapeum)

(declaim (inline octet-vector-p))
(defun octet-vector-p (x)
  "Is X an octet vector?"
  (typep x 'octet-vector))

(-> make-octet-vector (array-length) octet-vector)
(defun make-octet-vector (size)
  "Make an octet vector of SIZE elements."
  (declare (array-index size))
  (make-array size :element-type 'octet))

(defun octet-vector (&rest args)
  "Constructor an octet vector from ARGS."
  (make-array (length args)
              :element-type 'octet
              :initial-contents args))

(define-compiler-macro octet-vector (&rest args)
  `(make-array ,(length args)
               :element-type 'octet
               :initial-contents (list ,@args)))

;;; Adapted from Ironclad.
(-> octets (integer &key (:big-endian t)) octet-vector)
(defun octets (n &key big-endian)
  "Return N, an integer, as an octet vector.
Defaults to little-endian order."
  (declare (optimize speed))
  (with-subtype-dispatch integer
      ((unsigned-byte 32) (unsigned-byte 64) fixnum)
      n
    (let* ((n-bits (integer-length n))
           (n-bytes (ceiling n-bits 8))
           (vec (make-octet-vector n-bytes)))
      (declare (octet-vector vec))
      (if big-endian
          (loop for i from (1- n-bytes) downto 0
                for j from 0
                do (setf (aref vec j)
                         (locally (declare #+sbcl (optimize (speed 1)))
                           (ldb (byte 8 (* i 8)) n))))
          (loop for i from 0 below n-bytes
                for byte from 0 by 8
                do (setf (aref vec i)
                         (locally (declare #+sbcl (optimize (speed 1)))
                           (ldb (byte 8 byte) n)))))
      vec)))

(-> unoctets (octet-vector &key (:big-endian t)) integer)
(defun unoctets (bytes &key big-endian)
  "Concatenate BYTES, an octet vector, into an integer.
Defaults to little-endian order."
  (declare (octet-vector bytes)
           (inline reduce)
           (optimize #+sbcl (speed 1)
                     #-sbcl speed))
  (if big-endian
      (reduce (lambda (sum octet)
                (+ octet (ash sum 8)))
              bytes
              :initial-value 0)
      (loop for i from (1- (length bytes)) downto 0
            sum (ash (aref bytes i) (* i 8)))))

(declaim (inline octet-vector=/unsafe))
(defun octet-vector=/unsafe (v1 v2 start1 end1 start2 end2)
  (declare (optimize (speed 3)
                     (safety 0)
                     (debug 0)
                     (compilation-speed 0))
           (type octet-vector v1 v2)
           (type array-index start1 start2)
           (type array-length end1 end2))
  (and (= (- end1 start1)
          (- end2 start2))
       (loop for i from start1 below end1
             for j from start2 below end2
             always (eql (aref v1 i) (aref v2 j)))))

(-> octet-vector=
    (octet-vector octet-vector
                  &key (:start1 array-index)
                  (:start2 array-index)
                  (:end1 (or array-length null))
                  (:end2 (or array-length null)))
    boolean)
(defun octet-vector= (v1 v2 &key (start1 0) end1
                                 (start2 0) end2)
  "Like `string=' for octet vectors."
  (declare (octet-vector v1 v2)
           (array-index start1 start2)
           ((or array-length null) end1 end2)
           (optimize speed))
  (let* ((len1 (length v1))
         (len2 (length v2))
         (end1 (or end1 len1))
         (end2 (or end2 len2)))
    (assert (<= start1 end1 len1))
    (assert (<= start2 end2 len2))
    (octet-vector=/unsafe v1 v2 start1 end1 start2 end2)))

(defpattern octet-vector (&rest args)
  (with-unique-names (vec)
    `(trivia:guard1 ,vec
                    (octet-vector-p ,vec)
                    ,vec
                    (vector ,@args))))
