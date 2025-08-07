(defpackage :serapeum/portability
  (:documentation "Subtrivial portability.")
  #+sb-package-locks (:lock t)
  (:use :cl :alexandria)
  (:export
   :no-applicable-method-error
   :static-load-time-value
   :static-load-time-value-error
   :undisplace-array))

(in-package :serapeum/portability)

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

;;; https://groups.google.com/forum/#!original/comp.lang.lisp/JF3M5kA7_vo/g3oW1UuQJ_UJ
(defun undisplace-array (array)
  "Recursively get the fundamental array that ARRAY is displaced to.

Return the fundamental array, and the start and end positions into it.

Borrowed from Erik Naggum."
  (let ((length (length array))
        (start 0))
    (loop
      (multiple-value-bind (to offset) (array-displacement array)
        (if to
            (setq array to
                  start (+ start offset))
            (return (values array start (+ start length))))))))

(define-condition static-load-time-value-error (error)
  ((form :initarg :form)
   (read-only-p :initarg :read-only-p))
  (:report (lambda (c s)
             (with-slots (form) c
               (format s "Cannot use ~s with ~s"
                       'static-load-time-value
                       form)))))

(defun test-load-time-value (fn form read-only-p)
  (unless (eql (funcall fn) (funcall fn))
    (error 'static-load-time-value-error
           :form form
           :read-only-p read-only-p)))

;;; Use a compiler macro to eliminate the overhead for compiled code.
(define-compiler-macro test-load-time-value (fn form read-only-p)
  (declare (ignore fn form read-only-p))
  nil)

(defmacro static-load-time-value
    (form &optional (read-only-p nil read-only-p-supplied?))
  "Like `load-time-value', but signals an error if it cannot preserve identity.

On close reading of the standard, in a function that is evaluated but
not compiled, it is permissible for implementations to repeatedly
execute a `load-time-value' form, and in fact some implementations do
this \(including, at the time of writing, ABCL, CLISP, Allegro and
LispWorks).

When `static-load-time-value' is compiled, it behaves exactly like
`load-time-value'. Otherwise it conducts a run-time check to ensure
that `load-time-value' preserves identity."
  ;; Thanks to Jean-Philippe Paradis and Michał Herda for helping to
  ;; diagnose and treat the problem here.
  (if *compile-file-truename*
      `(load-time-value ,form
                        ,@(and read-only-p-supplied? (list read-only-p)))
      `(progn
         (flet ((fn () (load-time-value (random most-positive-fixnum))))
           (declare (dynamic-extent #'fn) (ignorable #'fn)
                    #+LispWorks (notinline fn))
           ;; Do the actual test out of line.
           (test-load-time-value #'fn ',form ',read-only-p))
         (load-time-value ,form
                          ,@(and read-only-p-supplied? (list read-only-p))))))

(deftype no-applicable-method-error ()
  "The type of the error signaled by `no-applicable-method' on this
Lisp. Note this may not be unique."
  (load-time-value
   (handler-case
       (no-applicable-method #'no-applicable-method (list 1))
     (error (e)
       (type-of e)))))
