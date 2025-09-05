(defpackage :serapeum/vector=
  (:use :cl :alexandria :serapeum)
  (:import-from :serapeum :declaim-freeze-type :with-two-arg-test)
  #+sb-package-locks (:implement :serapeum :serapeum/vector=))
(in-package :serapeum/vector=)

;;; Here we define `vector=': like `string=', but for any vector. In
;;; terms of behavior, we could just use `mismatch'. In terms of
;;; performance, however, for some winning combinations of the type of
;;; the vectors, the test function, and whether we have bounds to
;;; consider, we can leave `mismatch' in the dust. Even on SBCL, by
;;; calling out to `string=' for strings, or `equal' for bit vectors,
;;; we improve performance by orders of magnitude.

;;; The problem we face is to keep track of those combinations. The
;;; solution we use is to model kinds of vectors and kinds of tests as
;;; classes and dispatch with generic functions. We lose on the
;;; overhead of generic function dispatch, but we more than make up
;;; for it when we can defer to a specialized comparison function.


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
  (select test
    ((#'=) test/=)
    ((#'char=) test/char=)
    ((#'char-equal) test/char-equal)
    ((#'equalp) test/equalp)
    ((#'equal) test/equal)
    ((#'eql) test/eql)
    ((#'eq) test/eq)
    (t test)))


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

;;; TODO Allow :type1 and :type2 arguments. These would be ignored by
;;; the function, but could be used by a compiler macro to produce
;;; optimized, inline code.

(defun vector= (vec1 vec2 &key (test #'eql)
                               (start1 0)
                               (start2 0)
                               end1 end2)
  "Like `string=' for any vector.
If no TEST is supplied, elements are tested with `eql'."
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
         (compare-elements kind1 kind2 test-kind bounds))))

(defgeneric compare-elements (kind1 kind2 test-kind bounds))


;;; Handle strings.

(defmethod compare-elements ((s1 string) (s2 string) (test test/char=) (bounds null))
  (string= s1 s2))

(defmethod compare-elements ((s1 string) (s2 string) (test test/char-equal) (bounds null))
  (string-equal s1 s2))

(defmethod compare-elements ((s1 string) (s2 string) (test test/char=) (bounds bounds))
  (with-bounds (start1 end1 start2 end2) bounds
    (string= s1 s2
             :start1 start1
             :end1 end1
             :start2 start2
             :end2 end2)))

(defmethod compare-elements ((s1 string) (s2 string) (test test/char-equal) (bounds bounds))
  (with-bounds (start1 end1 start2 end2) bounds
    (string-equal s1 s2
                  :start1 start1
                  :end1 end1
                  :start2 start2
                  :end2 end2)))


;;; Handle bit vectors.

(defmethod compare-elements ((v1 bit-vector) (v2 bit-vector) (test test/rat=) (bounds null))
  (equal v1 v2))

(defmethod compare-elements ((v1 bit-vector) (v2 bit-vector) (test test/rat=) (bounds bounds))
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

(defmethod compare-elements ((w1 numeric-vector) (w2 numeric-vector) (test test/=) (bounds null))
  (equalp (unwrap w1) (unwrap w2)))

(defmethod compare-elements ((w1 rat-vector) (w2 rat-vector) (test test/rat=) (bounds null))
  (let ((v1 (unwrap w1))
        (v2 (unwrap w2)))
    (if (and (octet-vector-p v1)
             (octet-vector-p v2))
        (octet-vector= v1 v2)
        (equalp (unwrap w1) (unwrap w2)))))

;;; Unwrap vector wrappers.

(defmethod compare-elements ((w1 vector-wrapper) (w2 vector-wrapper) test bounds)
  (compare-elements (unwrap w1) (unwrap w2) test bounds))

(defmethod compare-elements ((w vector-wrapper) (v vector) test bounds)
  (compare-elements (unwrap w) v test bounds))

(defmethod compare-elements ((v vector) (w vector-wrapper) test bounds)
  (compare-elements v (unwrap w) test bounds))


;;; Octet vectors.

(defconst octet-vector-class (class-of (make-octet-vector 0)))

(when (proper-subtype-p octet-vector-class 'vector)
  (defmethod compare-elements ((v1 #.octet-vector-class)
                               (v2 #.octet-vector-class)
                               (test test/rat=)
                               (bounds null))
    (octet-vector= v1 v2))

  (defmethod compare-elements ((v1 #.octet-vector-class)
                               (v2 #.octet-vector-class)
                               (test test/rat=)
                               (bounds bounds))
    (with-bounds (start1 end1 start2 end2) bounds
      (octet-vector= v1 v2
                     :start1 start1
                     :end1 end1
                     :start2 start2
                     :end2 end2))))


;;; Not a string, not a bit vector, and not a pair of numeric vectors.

;;; Fall back to `equalp'.
(defmethod compare-elements ((v1 vector) (v2 vector) (test test/equalp) (bounds null))
  (equalp v1 v2))

(defmethod compare-elements ((v1 vector) (v2 vector) test (bounds null))
  (every (test-fn test) v1 v2))

(defmethod compare-elements ((v1 vector) (v2 vector) test (bounds bounds))
  (with-bounds (start1 end1 start2 end2) bounds
    (not (mismatch v1 v2 :start1 start1
                         :end1 end1
                         :start2 start2
                         :end2 end2
                         :test (test-fn test)))))
