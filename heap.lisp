(in-package :serapeum)

;;; Heap implementation adapted from Zach Beane's timers package for
;;; SBCL.

;;; Would this be worthwhile to document and export?

(defstruct
    (heap
     (:constructor make-heap
         (&key (size 100)
               (element-type t)
               (key #'identity)
               (test #'>=)
          &aux (vector
                (make-array size
                            :adjustable t
                            :fill-pointer 0
                            :element-type element-type)))))
  (vector #() :type vector :read-only t)
  (key #'identity :type function)
  (test #'>= :type function))

(declaim (inline heap-parent heap-left heap-right))

(defun heap-parent (i)
  (declare (array-index i))
  (ash (1- i) -1))

(defun heap-left (i)
  (declare (array-index i))
  (1+ (ash i 1)))

(defun heap-right (i)
  (declare (array-index i))
  (+ 2 (ash i 1)))

(defun heapify (vec start key test)
  (declare (function key test)
           (array-index start)
           (vector vec))
  (fbind ((ge test))
    (declare (ftype (-> (t t) t) ge))
    (with-vector-dispatch () vec
      (let ((l (heap-left start))
            (r (heap-right start))
            (size (length vec))
            largest)
        (with-key-fn (key)
          (setf largest (if (and (< l size)
                                 (not (ge (key (aref vec start))
                                          (key (aref vec l)))))
                            l
                            start))
          (when (and (< r size)
                     (not (ge (key (aref vec largest))
                              (key (aref vec r)))))
            (setf largest r)))
        (when (/= largest start)
          (rotatef (aref vec largest) (aref vec start))
          (heapify vec largest key test)))
      vec)))

(defun heap-insert (heap new-item)
  (let ((vec (heap-vector heap)))
    (fbind ((ge (heap-test heap)))
      (vector-push-extend nil vec)
      (with-key-fn (key (heap-key heap))
        (loop for i = (1- (length vec)) then parent-i
              for parent-i = (heap-parent i)
              while (and (> i 0)
                         (not (ge (key (aref vec parent-i))
                                  (key new-item))))
              do (setf (aref vec i) (aref vec parent-i))
              finally (setf (aref vec i) new-item)
                      (return-from heap-insert i))))))

(defun heap-maximum (heap)
  (let ((vec (heap-vector heap)))
    (unless (zerop (length vec))
      (aref vec 0))))

(defun heap-extract (heap i)
  (declare (heap heap) (array-index i))
  (let ((vec (heap-vector heap)))
    (unless (> (length vec) i)
      (error "Heap underflow"))
    (with-accessors ((key heap-key) (test heap-test)) heap
      (prog1 (aref vec i)
        (setf (aref vec i) (aref vec (1- (length vec))))
        (decf (fill-pointer vec))
        (heapify vec i key test)))))

(defun heap-extract-maximum (heap)
  (heap-extract heap 0))

(defun heap-extract-all (heap)
  (declare (heap heap))
  (loop while (> (length (heap-vector heap)) 0)
        collect (heap-extract-maximum heap)))
