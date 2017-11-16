(defpackage :serapeum/vector=
  (:use :cl :alexandria :serapeum)
  (:import-from :serapeum :declaim-freeze-type :with-test-fn)
  #+sb-package-locks (:implement :serapeum :serapeum/vector=))
(in-package :serapeum/vector=)


;;; Model the relationships between comparison predicates.

;;; Use test/rat= when the predicate is safe for comparing rationals.
;;; Remember eql is safe for integers, but not floats.

(defclass test/rat= () ())
(defclass test/char= () ())
;;; Two chars are char= if they are char-equal.
(defclass test/char-equal (test/char=) ())

(defclass test/= (test/rat=) ())

(defclass test/eq () ())
(defclass test/eql (test/eq test/rat= test/char=) ())
(defclass test/equal (test/eql) ())
;;; #'= and #'eql differ for floating point numbers.
(defclass test/equalp (test/char-equal test/equal test/=) ())

(defgeneric test.fn (test)
  (:method ((test test/eq)) #'eq)
  (:method ((test test/eql)) #'eql)
  (:method ((test test/equal)) #'equal)
  (:method ((test test/char=)) #'char=)
  (:method ((test test/char-equal)) #'char-equal)
  (:method ((test test/=)) #'=)
  (:method ((test test/equalp)) #'equalp)
  (:method ((test function)) test))

(declaim-freeze-type
 test/rat= test/=
 test/char= test/char-equal
 test/eq test/eql test/equal test/equalp)

(-> test-fn (t) function)
(defsubst test-fn (test)
  (if (functionp test) test
      (ensure-function (test.fn test))))

(def test/=          (make 'test/=))
(def test/char=      (make 'test/char=))
(def test/char-equal (make 'test/char-equal))
(def test/equal      (make 'test/equal))
(def test/equalp     (make 'test/equalp))
(def test/eql        (make 'test/eql))
(def test/eq         (make 'test/eq))

;;; Unknown tests are left untouched (returned as functions).

(defun test-kind (test)
  (declare (function test))
  (assure test-kind
    (select test
      ((#'=) test/=)
      ((#'char=) test/char=)
      ((#'char-equal) test/char-equal)
      ((#'equalp) test/equalp)
      ((#'equal) test/equal)
      ((#'eql) test/eql)
      ((#'eq) test/eq)
      (t test))))


;;; Model relationships between vector types.

(defclass vector-wrapper ()
  ((vector :initarg :vector
           :type vector
           :reader unwrap)))

(defclass numeric-vector (vector-wrapper)
  ())

(defclass float-vector (numeric-vector)
  ())

(defclass rat-vector (numeric-vector)
  ())

(declaim-freeze-type
 vector-wrapper numeric-vector
 float-vector
 rat-vector)

(defun vector-kind (v)
  (let ((aet (array-element-type v)))
    (cond ((subtypep aet '(or rational (complex rational)))
           (make 'rat-vector :vector v))
          ((subtypep aet '(or float (complex float)))
           (make 'float-vector :vector v))
          (t v))))


;;; Model bounds.

(defconstructor bounds
  (start1 array-index)
  (end1 array-length)
  (start2 array-index)
  (end2 array-length))

(defmacro with-bounds ((start1 end1 start2 end2) bounds &body body)
  (with-unique-names (b)
    `(let ((,b (assure bounds ,bounds)))
       (let ((,start1 (bounds-start1 ,b))
             (,end1 (bounds-end1 ,b))
             (,start2 (bounds-start2 ,b))
             (,end2 (bounds-end2 ,b)))
         ,@body))))

(defun bounds-trivial? (vec start end)
  (declare (vector vec))
  (declare (array-index start))
  (declare ((or null array-length) end))
  (declare (optimize speed))
  (and (zerop start)
       (or (null end)
           (= end (length vec)))))

(defun reify-bounds (vec1 vec2
                     start1 end1
                     start2 end2)
  (declare (vector vec1 vec2))
  (let* ((len1 (length vec1))
         (len2 (length vec2))
         (end1 (or end1 len1))
         (end2 (or end2 len2)))
    (assert (<= 0 start1 end1 len1))
    (assert (<= 0 start2 end2 len2))
    (if (and (bounds-trivial? vec1 start1 end1)
             (bounds-trivial? vec2 start2 end2))
        nil
        (bounds start1 end1 start2 end2))))

(defgeneric bounds-plausible? (bounds v1 v2)
  (:method ((bounds null) v1 v2)
    (or (eq v1 v2)
        (= (length v1) (length v2))))
  (:method ((bounds bounds) v1 v2)
    (declare (ignore v1 v2))
    (with-bounds (start1 end1 start2 end2) bounds
      (= (- end1 start1)
         (- end2 start2)))))


;;; vector=.

(defun vector= (vec1 vec2 &key (test #'eql)
                               (start1 0)
                               (start2 0)
                               end1 end2)
  "Like `string=' for any vector."
  (check-type vec1 vector)
  (check-type vec2 vector)
  (check-type start1 array-index)
  (check-type start2 array-index)
  (check-type end1 (or array-length null))
  (check-type end2 (or array-length null))
  (let ((kind1 (vector-kind vec1))
        (kind2 (vector-kind vec2))
        (test-kind (test-kind (ensure-function test)))
        (bounds
          (reify-bounds vec1 vec2
                        start1 end1
                        start2 end2)))
    (and (bounds-plausible? bounds vec1 vec2)
         (vect= kind1 kind2 test-kind bounds))))

(defgeneric vect= (kind1 kind2 test-kind bounds))


;;; Handle strings.

(defmethod vect= ((s1 string) (s2 string) (test test/char=) (bounds null))
  (string= s1 s2))

(defmethod vect= ((s1 string) (s2 string) (test test/char-equal) (bounds null))
  (string-equal s1 s2))

(defmethod vect= ((s1 string) (s2 string) (test test/char=) (bounds bounds))
  (with-bounds (start1 end1 start2 end2) bounds
    (string= s1 s2
             :start1 start1
             :end1 end1
             :start2 start2
             :end2 end2)))

(defmethod vect= ((s1 string) (s2 string) (test test/char-equal) (bounds bounds))
  (with-bounds (start1 end1 start2 end2) bounds
    (string-equal s1 s2
                  :start1 start1
                  :end1 end1
                  :start2 start2
                  :end2 end2)))


;;; Handle bit vectors.

(defmethod vect= ((v1 bit-vector) (v2 bit-vector) (test test/rat=) (bounds null))
  (equal v1 v2))

(defmethod vect= ((v1 bit-vector) (v2 bit-vector) (test test/rat=) (bounds bounds))
  (with-bounds (start1 end1 start2 end2) bounds
    ;; Using `equal', even with displaced bit
    ;; vectors, is orders of magnitude faster
    ;; than looping bit by bit.
    (equal (nsubseq v1 start1 end1)
           (nsubseq v2 start2 end2))))


;;; Handle numeric vectors.

;;; The advantage with numeric vectors is that we can fall back to
;;; equalp (because we know we're not dealing with characters). The
;;; tricky part is vectors of floats: `=' and `eql' are not the same
;;; for floats.

(defmethod vect= ((w1 numeric-vector) (w2 numeric-vector) (test test/=) (bounds null))
  (equalp (unwrap w1) (unwrap w2)))

(defmethod vect= ((w1 rat-vector) (w2 rat-vector) (test test/rat=) (bounds null))
  (equalp (unwrap w1) (unwrap w2)))

;;; Unwrap vector wrappers.

(defmethod vect= ((w1 vector-wrapper) (w2 vector-wrapper) test bounds)
  (vect= (unwrap w1) (unwrap w2) test bounds))

(defmethod vect= ((w vector-wrapper) (v vector) test bounds)
  (vect= (unwrap w) v test bounds))

(defmethod vect= ((v vector) (w vector-wrapper) test bounds)
  (vect= v (unwrap w) test bounds))


;;; Not a string, not a bit vector, and not a pair of numeric vectors.

(defconst vector-comparison-specializations
  (if (featurep :allegro-cl-express)
      ;; Trying to compile vector= with specialized types exhausts the
      ;; heap on the Allegro CL Free Express Edition.
      '(vector)
      '((simple-array (unsigned-byte 8) (*))
        ;; Need to raise inline-expansion-limit?
        ;; (simple-array (signed-byte 8) (*))
        (simple-array (unsigned-byte 16) (*))
        ;; (simple-array (signed-byte 16) (*))
        ;; (simple-array (unsigned-byte 32) (*))
        ;; (simple-array (signed-byte 32) (*))
        ;; (simple-array (unsigned-byte 64) (*))
        ;; (simple-array (signed-byte 64) (*))
        (simple-array fixnum (*))
        (simple-array single-float (*))
        (simple-array double-float (*)))))

;;; Fall back to `equalp'.
(defmethod vect= ((v1 vector) (v2 vector) (test test/equalp) (bounds null))
  (equalp v1 v2))

(defmethod vect= ((v1 vector) (v2 vector) test (bounds null))
  (every (test-fn test) v1 v2))

(defmethod vect= ((v1 vector) (v2 vector) test (bounds bounds))
  (declare (optimize (debug 0) (safety 0) (compilation-speed 0)))
  (with-bounds (start1 end1 start2 end2) bounds
    (declare (type array-index start1 start2)
             (type array-length end1 end2))
    (fbind ((test (test-fn test)))
      (with-vector-dispatch #.vector-comparison-specializations v1
        (with-vector-dispatch #.vector-comparison-specializations v2
          (loop for i of-type array-index from start1 below end1
                for j of-type array-index from start2 below end2
                always (test (vref v1 i) (vref v2 j))))))))
