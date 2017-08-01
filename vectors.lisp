(in-package #:serapeum)

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
  (if (null inits)
      `(make-array 0
                   :element-type t
                   :adjustable t
                   :fill-pointer 0)
      (let ((len (length inits)))
        (with-gensyms (tmp)
          `(let ((,tmp (list ,@inits)))
             (declare (dynamic-extent ,tmp))
             (make-array ,len
                         :element-type t
                         :adjustable t
                         :fill-pointer ,len
                         :initial-contents ,tmp))))))

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
  (with-test-fn (test)
    (with-vector-dispatch #1=((simple-array (unsigned-byte 8) (*))
                              (simple-array (unsigned-byte 16) (*))
                              (simple-array (unsigned-byte 32) (*))
                              (simple-array (unsigned-byte 64) (*))
                              (simple-array fixnum (*)))
      v1
      (with-vector-dispatch #1# v2
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

