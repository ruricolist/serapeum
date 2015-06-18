(in-package #:serapeum)

(defun vect (&rest initial-contents)
  "Succint constructor for adjustable vectors with fill pointers.

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
  (declare (vector v1 v2))
  (let ((end1 (or end1 (length v1)))
        (end2 (or end2 (length v2))))
    (and (= (- end1 start1)
            (- end2 start2))
         (loop for i from start1 below end1
               for j from start2 below end2
               always (funcall test (aref v1 i) (aref v2 j))))))
