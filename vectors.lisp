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

(defpattern vect (&rest inits)
  `(vector ,@inits))
