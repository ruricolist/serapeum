(in-package :serapeum)

(export '(-> check-the))

(deftype -> (args values)
  "The type of a function from ARGS to VALUES."
  `(function ,args ,values))

(defmacro -> (function args values)
  "Declaim the ftype of a function from ARGS to VALUES.

     (-> mod-fixnum+ (fixnum fixnum) fixnum)
     (defun mod-fixnum+ (x y) ...)"
  `(declaim (ftype (-> ,args ,values) ,function)))

(defmacro check-the (type-spec &body (form))
  "Cross between CHECK-TYPE and THE for inline type checking.
The syntax is the same as THE; the semantics are the same as
CHECK-TYPE."
  (with-gensyms (temp)
    `(let ((,temp ,form))
       (check-type ,temp ,type-spec)
       ,temp)))
