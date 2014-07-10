(in-package #:serapeum)

(export '(vect))

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

(assert (adjustable-array-p (vect)))
(assert (fill-pointer (vect)))
(assert (equalp (vect 1 2 3) #(1 2 3)))
