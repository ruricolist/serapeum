(in-package :serapeum)

(export '(-> assure assuref wholenum))

(deftype wholenum ()
  "A whole number. Equivalent to `(integer 0 *)'."
  '(integer 0 *))

(deftype -> (args values)
  "The type of a function from ARGS to VALUES."
  `(function ,args ,values))

(defmacro -> (function args values)
  "Declaim the ftype of a function from ARGS to VALUES.

     (-> mod-fixnum+ (fixnum fixnum) fixnum)
     (defun mod-fixnum+ (x y) ...)"
  `(declaim (ftype (-> ,args ,values) ,function)))

(defun read-new-value ()
  "Read and evaluate a value."
  (format *query-io* "~&New value: ")
  (list (eval (read *query-io*))))

(defmacro wrong-type (datum type restart &body (report))
  `(restart-case
       (error 'type-error
              :datum ,datum
              :expected-type ,type)
     (,restart (new)
       :report ,report
       :interactive read-new-value
       new)))

(defun require-type (datum spec)
  (declare (optimize (debug 0)))
  (if (typep datum spec)
      datum
      (let ((new (wrong-type datum spec use-value
                   "Supply a value to use instead")))
        (require-type new spec))))

(defun require-type-for (datum spec place)
  (declare (optimize (debug 0)))
  (if (typep datum spec)
      datum
      (let ((new (wrong-type datum spec store-value
                   (lambda (s) (format s "Supply a new value for ~s" place)))))
        (require-type-for new spec place))))

(defmacro assure (type-spec &body (form))
  "Macro for inline type checking.

`assure' is to `the' as `check-type' is to `declare'.

     (the string 1) => undefined
     (assure string 1) => error

The value returned from the `assure' form is guaranteed to satisfy
TYPE-SPEC. If FORM does not return a value of that type, then a
correctable error is signaled. You can supply a value of the correct
type with the `use-value' restart.

Note that the supplied value is *not* saved into the place designated
by FORM. (But see `assuref'.)

From ISLISP."
  `(the ,type-spec (values (require-type ,form ',type-spec))))

(defmacro assuref (place type-spec)
  "Like `(progn (check-type PLACE TYPE-SPEC) PLACE)`, but evaluates
PLACE only once."
  (with-gensyms (temp)
    (let ((ts type-spec))
      `(the ,ts
            (values
             (let ((,temp ,place))
               (if (typep ,temp ',ts)
                   ,temp
                   (setf ,place (require-type-for ,temp ',ts ',place)))))))))
