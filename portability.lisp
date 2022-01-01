(in-package :serapeum)

;;;#

;; Cut and paste from babel-encodings. TODO: Think about if and how
;; this could be usefully exposed.
(progn

  (defmacro with-simple-vector (((v vector) (s start) (e end)) &body body)
    "If VECTOR is a displaced or adjustable array, binds V to the
underlying simple vector, adds an adequate offset to START and
END and binds those offset values to S and E.  Otherwise, if
VECTOR is already a simple array, it's simply bound to V with no
further changes.

START and END are unchecked and assumed to be within bounds.

Note that in some Lisps, a slow copying implementation is
necessary to obtain a simple vector thus V will be bound to a
copy of VECTOR coerced to a simple-vector.  Therefore, you
shouldn't attempt to modify V."
    #+sbcl
    `(sb-kernel:with-array-data ((,v ,vector) (,s ,start) (,e ,end))
       ,@body)
    #+(or cmu scl)
    `(lisp::with-array-data ((,v ,vector) (,s ,start) (,e ,end))
       ,@body)
    #+openmcl
    (with-unique-names (offset)
      `(multiple-value-bind (,v ,offset)
           (ccl::array-data-and-offset ,vector)
         (let ((,s (+ ,start ,offset))
               (,e (+ ,end ,offset)))
           ,@body)))
    #+allegro
    (with-unique-names (offset)
      `(excl::with-underlying-simple-vector (,vector ,v ,offset)
         (let ((,e (+ ,end ,offset))
               (,s (+ ,start ,offset)))
           ,@body)))
    ;; slow, copying implementation
    #-(or sbcl cmu scl openmcl allegro)
    (once-only (vector)
      `(funcall (if (adjustable-array-p ,vector)
                    #'call-with-array-data/copy
                    #'call-with-array-data/fast)
                ,vector ,start ,end
                (lambda (,v ,s ,e) ,@body))))

  (defun call-with-array-data/fast (vector start end fn)
    (multiple-value-bind (data offset)
        (undisplace-array vector)
      (funcall fn data (+ offset start) (+ offset end))))

  (defun call-with-array-data/copy (vector start end fn)
    (funcall fn (replace (make-array (- end start) :element-type
                                     (array-element-type vector))
                         vector :start2 start :end2 end)
             0 (- end start))))
