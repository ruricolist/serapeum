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

(-> pad-start (vector array-length &optional t)
    vector)
(defun pad-start (vec length &optional (pad #\Space))
  "Pad VEC, a vector, to LENGTH, using PAD.
If VEC is already the same length, or longer, than LENGTH, return VEC
unchanged.

    (pad-start \"abc\" 3)
    => \"abc\"

If PAD is a sequence, then it is repeated before VEC to make up LENGTH.

    (pad-start \"abc\" 9 \"def\")
    => \"defdefabc\"

If PAD is not a sequence, it is used to fill the remainder of VEC.

    (pad-start \"abc\" 6 #\x)
    => \"xxxabc\"

PAD defaults to the space character.

This function is most useful for strings, but it can be used with any
vector."
  (declare (vector vec)
           (array-length length))
  (cond ((>= (length vec) length) vec)
        ((typep pad 'sequence)
         (let ((pad-len (length pad)))
           (cond ((= pad-len 0) vec)
                 ((= pad-len 1)
                  (pad-start vec length (elt pad 0)))
                 (t
                  (lret* ((offset (- length (length vec)))
                          (element-type (array-element-type vec))
                          (out (make-array length :element-type element-type)))
                    (replace out vec :start1 offset)
                    (loop for i from 0 below offset by pad-len do
                      (replace out pad :start1 i :end1 offset)))))))
        (t
         (lret* ((offset (- length (length vec)))
                 (element-type (array-element-type vec))
                 (out (make-array length :element-type element-type)))
           (replace out vec :start1 offset)
           (fill out pad :end offset)
           out))))

(define-compiler-macro pad-start (&whole call vec len
                                         &optional (pad #\Space)
                                         &environment env)
  (expand-pad-x call 'pad-start env vec len pad))

(-> pad-end (vector array-length &optional t)
    vector)
(defun pad-end (vec length &optional (pad #\Space))
  "Pad VEC, a vector, to LENGTH, using PAD.
If VEC is already the same length, or longer, than LENGTH, return VEC
unchanged.

    (pad-end \"abc\" 3)
    => \"abc\"

If PAD is a sequence, then it is repeated after VEC to make up LENGTH.

    (pad-end \"abc\" 9 \"def\")
    => \"abcdefdef\"

If PAD is not a sequence, it is used to fill the remainder of VEC.

    (pad-end \"abc\" 6 #\x)
    => \"abcxxx\"

PAD defaults to the space character.

This function is most useful for strings, but it can be used with any
vector."
  (declare (vector vec)
           (array-length length))
  (cond ((>= (length vec) length) vec)
        ((typep pad 'sequence)
         (let ((pad-len (length pad)))
           (cond ((= pad-len 0) vec)
                 ((= pad-len 1)
                  (pad-end vec length (elt pad 0)))
                 (t
                  (lret* ((element-type (array-element-type vec))
                          (out (make-array length :element-type element-type)))
                    (replace out vec)
                    (loop for i from (length vec) below length by pad-len do
                      (replace out pad :start1 i)))))))
        (t
         (lret* ((element-type (array-element-type vec))
                 (out (make-array length :element-type element-type)))
           (replace out vec)
           (fill out pad :start (length vec))
           out))))

(define-compiler-macro pad-end (&whole call vec len
                                       &optional (pad #\Space)
                                       &environment env)
  (expand-pad-x call 'pad-end env vec len pad))

(defun expand-pad-x (call fn env vec len pad)
  (if (not (typep pad 'sequence)) call
      (case (length pad)
        (0
         (cond ((constantp len env)
                vec)
               ((constantp vec env)
                ;; We don't have to worry about evaluation order.
                `(progn ,len ,vec))
               (t
                ;; Evaluate VEC, then LEN, then return STRING.
                (with-unique-names (temp)
                  ;; Ensure LEN
                  `(let ((,temp ,vec))
                     ,len
                     ,temp)))))
        (1 `(,fn ,vec ,len ',(aref pad 0)))
        (t call))))
