(defpackage :serapeum/generalized-arrays
  (:use :cl :alexandria :serapeum)
  (:export
   :tally
   :valence
   :each
   :each-left
   :each-right
   :tell
   :array=)
  (:documentation "Implementation of generalized arrays.")
  #+sb-package-locks (:implement :serapeum :serapeum/dispatch-case))
(in-package :serapeum/generalized-arrays)

(defsubst tally (array)
  "Return the total size of ARRAY, a generalized array.
For a true array this is equivalent to `array-total-size'."
  ;; (reduce #'* (shape array))
  (typecase array
    (sequence (length array))
    (array (array-total-size array))
    (t 0)))

(defsubst shape (array)
  "Return the shape of ARRAY, a generalized array.
For a true array this is equivalent to `array-dimensions'."
  (typecase array
    (sequence (list (length array)))
    (array (array-dimensions array))
    ;; An array with no axes.
    (otherwise nil)))

(defsubst valence (array)
  "Return the number of dimensions of ARRAY, a generalized array.
For a true array this is equivalent to `array-rank'."
  ;; (tally (shape array))
  (typecase array
    (sequence 1)
    (array (array-rank array))
    (t 0)))

(defsubst shape= (array1 array2)
  "Return true if ARRAY1 and ARRAY2 have the same shape."
  ;; (equal (shape array1) (shape array2))
  (typecase array1
    (sequence
     (typecase array2
       (sequence
        (length= array1 array2))
       (otherwise nil)))
    (array
     (typecase array2
       (vector nil)
       (array
        (equal (shape array1) (shape array2)))
       (otherwise nil)))
    (otherwise
     (typecase array2
       (sequence nil)
       (array nil)
       (otherwise t)))))

(defsubst ensure-shape (x)
  (etypecase x
    (array-length (list x))
    (list x)))

(defsubst replace* (out in)
  "Like `replace' with no keyword arguments, but if IN is shorter than
OUT, extend it cyclically.

In the base case, if IN is empty, leave OUT alone."
  (if (emptyp in) out
      (loop for start from 0 below (length out) by (length in)
            do (replace out in :start1 start)
            finally (return out))))

(defsubst %flatten (array)
  (make-array (array-total-size array)
              :displaced-to array
              :displaced-index-offset 0
              :element-type (array-element-type array)))

(defsubst shrink-wrap (object shape)
  "Make an array of shape SHAPE containing OBJECT as its initial element.
The array will have the smallest element type sufficient to contain
OBJECT."
  (make-array shape
              :initial-element object
              :element-type (upgraded-array-element-type `(eql ,object))))

(defsubst void (x)
  (shrink-wrap x 0))

(defsubst displace (array shape
                    &optional (offset 0))
  "Shorthand function for displacing an array."
  (make-array (ensure-shape shape)
              :displaced-to array
              :displaced-index-offset offset
              :element-type (array-element-type array)))

(defun reshape (shape array &key (element-type t) (displace t))
  "Return an array that has the same items as ARRAY, a generalized
array, but whose shape is SHAPE.

If the resulting array is smaller than ARRAY, then discard the excess
items.

If the resulting array is larger than ARRAY, fill it with the items of
ARRAY cyclically.

ELEMENT-TYPE specifies an element type to use for the resulting array
if one cannot be inferred from the array itself."
  (setf shape (ensure-shape shape))
  ;; (when (arrayp array)
  ;;   (setf array (undisplace-array array)))
  (cond
    ((equal shape (shape array))
     array)
    ((null shape)
     (assure (or null (vector * 0))
       (typecase array
         (array
          (make-array 0 :element-type (array-element-type array)))
         (number (void array))
         (t nil))))
    ((null (cdr shape))
     (assure sequence
       (let ((len (car shape)))
         (typecase array
           (sequence
            (let ((array-len (length array)))
              (if (<= len array-len)
                  (if displace
                      (nsubseq array 0 len)
                      (subseq array 0 len))
                  (lret ((out (serapeum::make-sequence-like array len)))
                    (replace* out array)))))
           (array
            (let ((element-type (array-element-type array)))
              (or (and (<= len (array-total-size array))
                       (if displace
                           (displace array len)
                           (and (= len (array-total-size array))
                                (make-array len
                                            :element-type element-type
                                            :initial-contents (%flatten array)))))

                  (lret ((out (make-array
                               len
                               :element-type (array-element-type array))))
                    (replace* out (%flatten array))))))
           (t (shrink-wrap array shape))))))
    (t
     (assure (and array (not vector))
       (let ((size (apply #'* shape)))
         (typecase array
           (vector
            (or (and (<= size (length array))
                     (and displace
                          (displace array shape)))
                (lret ((out (make-array
                             shape
                             :element-type (array-element-type array))))
                  (replace* (%flatten out) array))))
           (sequence
            (lret ((out (make-array shape :element-type element-type)))
              (replace* (%flatten out) array)))
           (array
            (let ((element-type (array-element-type array)))
              (or (and (<= size (array-total-size array))
                       (and displace
                            (displace array shape)))
                  (lret ((out (make-array shape
                                          :element-type element-type)))
                    (replace* (%flatten out)
                              (%flatten array))))))
           (t (shrink-wrap array shape))))))))

(defun ravel (array &key (displace t))
  "Return the items of ARRAY as a sequence.

Array theory calls this operation `list', but the MOA operation is
identical and has a more distinctive name."
  ;; (reshape (tally array) array)
  (typecase array
    (sequence (copy-seq array))
    (array (reshape (tally array) array :displace displace))
    (t (list array))))

(defun tell (shape)
  (etypecase shape
    (array-index (range shape))
    (sequence
     (lret* ((shape (ensure-shape shape))
             (array (make-array shape)))
       (loop for i from 0 below (array-total-size array)
             do (setf (row-major-aref array i)
                      (array-index-row-major array i)))))))

(defun array= (x y)
  #.+merge-tail-calls+
  (and (shape= x y)
       (typecase x
         (sequence
          (typecase y
            (sequence
             (every #'array= x y))
            (otherwise nil)))
         (array
          (typecase y
            (array
             (loop with size = (array-total-size x)
                   for i below size
                   always (array= (row-major-aref x i)
                                  (row-major-aref y i))))
            (otherwise nil)))
         (otherwise (equal x y)))))

(defun each (fn array &key (element-type t))
  (let ((fn (ensure-function fn)))
    (typecase array
      (list (mapcar fn array))
      (vector (map-into
               (make-array (length array) :element-type element-type)
               fn
               array))
      (sequence (map-into
                 (serapeum::make-sequence-like array (length array))
                 fn array))
      (array
       (lret ((out (make-array
                    (array-dimensions array)
                    :element-type element-type)))
         (map-into (%flatten out)
                   fn
                   (%flatten array))))
      (otherwise (funcall fn array)))))

(defun each-left (array fn fixed &key (element-type t))
  "The left refers to the position of the array."
  (fbind (fn)
    (each (op (fn _ fixed))
          array
          :element-type element-type)))

(defun each-right (fixed fn array &key (element-type t))
  (fbind (fn)
    (each (op (fn fixed _))
          array
          :element-type element-type)))

(defun mutual-element-type (arrays)
  (upgraded-array-element-type
   (cons 'or
         (map 'list
              (lambda (array)
                (if (arrayp array)
                    (array-element-type array)
                    t))
              arrays))))

(defun link (arrays)
  "Return a list of all of the items in ARRAYS."
  (cond
    ((nor (arrayp arrays)
          (typep arrays 'sequence))
     (list arrays))
    ((notevery #'arrayp arrays)
     (collecting
       (do-each (a arrays)
         (typecase a
           (sequence
            (do-each (x a)
              (collect x)))
           (array
            (loop for i from 0 below (array-total-size a) do
              (collect (row-major-aref a i))))
           (otherwise (collect a)))
         arrays)))
    (t
     (let* ((size (reduce #'+ arrays :key #'array-total-size))
            (element-type (mutual-element-type arrays))
            (offset 0)
            (array-out (make-array size :element-type element-type)))
       (do-each (a arrays array-out)
         (replace array-out a :start1 offset))))))

;;; TODO Experiment with value.
(defconst seq-cutoff 128
  "Max length above which to operate pairwise.")

(defun reduce-between (fn xs start end)
  (fbind fn
    (let ((first-time? t)
          (result nil))
      (loop for i from start below end
            do (if first-time?
                   (setf first-time? nil
                         result (aref xs i))
                   (setf result (fn result (aref xs i))))
            finally (return result)))))

(defun reduce-vector-pairwise (fun xs)
  (fbindrec (fun
             (pairwise
              (lambda (start end)
                (let ((len (- end start)))
                  (if (<= len seq-cutoff)
                      (reduce-between fun xs start end)
                      (let ((split (+ start (ceiling len 2))))
                        (fun (pairwise start split)
                             (pairwise split end))))))))
    (pairwise 0 (length xs))))

(defun pairwise (fn xs)
  (reduce-vector-pairwise fn (coerce xs 'vector)))

(defun sum (array)
  "Return the sum of all of the elements of ARRAY, a generalized array.
Operates pairwise for numerical stability."
  (etypecase array
    (bit-vector
     (with-type-dispatch (simple-bit-vector bit-vector) array
       (count 1 array)))
    (sequence (or (pairwise #'+ array) 0))
    (array (sum (%flatten array)))
    (number array)))

(defun prod (array)
  "Return the product of all of the elements of ARRAY, a generalized array.
Operates pairwise for numerical stability."
  (etypecase array
    (bit-vector
     (with-type-dispatch (simple-bit-vector bit-vector) array
       (if (find 0 array) 0 1)))
    (sequence (or (pairwise #'* array) 1))
    (array (prod (%flatten array)))
    (number array)))
