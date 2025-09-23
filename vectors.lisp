(in-package #:serapeum)

(defsubst ensure-vector (x)
  "If X is a vector, return it.
Otherwise, return a vector with X as its sole element."
  (if (vectorp x) x
      (vector x)))

(defun vectp (x)
  "Is X a vect?"
  (and (arrayp x)
       (adjustable-array-p x)
       (handler-case
           (fill-pointer x)
         (type-error ()
           nil))))

(deftype vect ()
  "The type of a vector constructed by `vect'."
  '(and (vector t) (satisfies vectp)))

(-> vect (&rest t) (vector t *))
(defun vect (&rest initial-contents)
  "Succinct constructor for adjustable vectors with fill pointers.

    (vect 1 2 3)
    ≡ (make-array 3
            :adjustable t
            :fill-pointer 3
            :initial-contents (list 1 2 3))

The fill pointer is placed after the last element in INITIAL-CONTENTS.

As a constructor this also has a matching definition as a Trivia
pattern for destructing."
  (declare (dynamic-extent initial-contents))
  (let ((len (length initial-contents)))
    (make-array len
                :element-type t
                :adjustable t
                :fill-pointer len
                :initial-contents initial-contents)))

(defmacro generate-values-vector-case (vec)
  ;; TODO This should use `tree-case', but it would need to be a
  ;; different file.
  `(case (length ,vec)
     ,@(loop for i from 0 below 20
             collect `(,i
                       (values ,@(loop for j from 0 below i
                                       collect `(aref ,vec ,j)))))
     (t (values-list (coerce ,vec 'list)))))

(defun values-vector (vec)
  "Return the elements of VEC, a vector, as multiple values.
This is to vectors what `values-list' is to lists."
  (declare (type vector vec))
  (generate-values-vector-case vec))

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

;;; This differs from the default Trivia `vector' pattern in that it
;;; works for adjustable vectors with fill pointers.

(defpattern vect (&rest elts)
  (with-unique-names (it)
    `(trivia:guard (and ,it (trivia:vector* ,@elts))
                   (= (length ,it) ,(length elts)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-pad-x (call fn env vec len pad)
    "Auxiliary function for `pad-X' compiler macros.
     Optimizes some cases where PAD is a constant sequence."
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
          (t call)))))

(define-compiler-macro pad-start (&whole call vec len
                                         &optional (pad #\Space)
                                         &environment env)
  (expand-pad-x call 'pad-start env vec len pad))

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
vector. Note that the vector returned has the same element type as
VEC, so PAD must satisfy that element type.

Loosely inspired by ECMA."
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

(define-compiler-macro pad-end (&whole call vec len
                                       &optional (pad #\Space)
                                       &environment env)
  (expand-pad-x call 'pad-end env vec len pad))

(-> pad-end (vector array-length &optional t)
    vector)
(defun pad-end (vec length &optional (pad #\Space))
  "Pad VEC, a vector, to LENGTH, using PAD.
Like `pad-start', but padding is addded to the end, rather than the
beginning."
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

(defun vector-conc-extend (vector new-elements &optional (extension 0))
  "Add NEW-ELEMENTS to the end of VECTOR, an adjustable array with a fill-pointer.
This is the practical equivalent to calling `vector-push-extend' on
each element on NEW-ELEMENTS, but should be faster.

Returns VECTOR."
  (declare (type array-length extension))
  (cond ((emptyp new-elements))
        ((single new-elements)
         (vector-push-extend (elt new-elements 0) vector))
        (t (let* ((size (array-dimension vector 0))
                  (len1 (length vector))
                  (len2 (length new-elements))
                  (diff (- size len1 len2)))
             (when (minusp diff)
               (adjust-array vector (max extension (- size diff))))
             (incf (fill-pointer vector) len2)
             (replace vector new-elements :start1 len1))))
  vector)
