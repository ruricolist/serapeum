(in-package #:serapeum)

(-> vect (&rest t) (vector t *))
(defun vect (&rest initial-contents)
  "Succinct constructor for adjustable vectors with fill pointers.

    (vect 1 2 3)
    ≡ (make-array 3
            :adjustable t
            :fill-pointer 3
            :initial-contents (list 1 2 3))

The fill pointer is placed after the last element in INITIAL-CONTENTS."
  (declare (dynamic-extent initial-contents))
  (let ((len (length initial-contents)))
    (make-array len
                :element-type t
                :adjustable t
                :fill-pointer len
                :initial-contents initial-contents)))

(define-compiler-macro vect (&rest inits)
  (let ((len (length inits)))
    `(make-array ,len
                 :element-type t
                 :adjustable t
                 :fill-pointer ,len
                 :initial-contents
                 ;; NB We use to stack-allocate the list of inits, but
                 ;; that could result in junk in the vector; see issue
                 ;; #14. Note that SBCL does not actually allocate
                 ;; the list below; see array-tran.lisp.
                 (list ,@inits))))

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

(defun vector= (v1 v2 &key (test #'eql)
                           (start1 0)
                           (end1 nil)
                           (start2 0)
                           (end2 nil))
  "Like `string=' for any vector."
  (declare (vector v1 v2)
           ((or array-index null)
            end1 end2)
           (array-index start1 start2)
           (optimize (safety 0) (debug 0)))
  (setf test (ensure-function test))
  ;; The easy case.
  (when (eql v1 v2)
    (return-from vector= t))
  ;; Defer to string= when possible.
  (when (and (stringp v1)
             (stringp v2)
             (select test ((#'eql #'equal #'char=) t)))
    (return-from vector=
      (string= v1 v2 :start1 start2 :end1 end1 :start2 start2 :end2 end2)))
  ;; Handle bit vectors specially.
  (when (and (bit-vector-p v1)
             (bit-vector-p v2)
             (select test ((#'eq #'eql #'=) t)))
    (return-from vector=
      (let* ((end1 (or end1 (length v1)))
             (end2 (or end2 (length v2)))
             (size1 (- end1 start1))
             (size2 (- end2 start2)))
        (and (= size1 size2)
             (flet ((nsubseq (v start end)
                      (if (and (zerop start)
                               (= end (length v)))
                          v
                          ;; Using `equal', even with displaced bit
                          ;; vectors, is orders of magnitude faster
                          ;; than looping bit by bit.
                          (make-array (- end start)
                                      :element-type 'bit
                                      :displaced-to v
                                      :displaced-index-offset start))))
               (let* ((v1 (nsubseq v1 start1 end1))
                      (v2 (nsubseq v2 start2 end2)))
                 (equal v1 v2)))))))
  ;; Generate code for other kinds of vectors.
  (with-test-fn (test)
    (with-vector-dispatch #.vector-comparison-specializations v1
      (with-vector-dispatch #.vector-comparison-specializations v2
        (let* ((len1 (length v1))
               (len2 (length v2))
               (end1 (or end1 len1))
               (end2 (or end2 len2)))
          (assert (<= 0 start1 end1 len1))
          (assert (<= 0 start2 end2 len2))
          (and (= (- end1 start1)
                  (- end2 start2))
               (loop for i of-type array-index from start1 below end1
                     for j of-type array-index from start2 below end2
                     always (test (vref v1 i) (vref v2 j)))))))))
