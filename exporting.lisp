(in-package :serapeum)

(defpackage :serapeum.exporting
  (:use)
  (:export :defclass))

(defmacro serapeum.exporting:defclass (name supers &body (slots . options))
  "Like `defclass', but implicitly export the name of the class and
the names of all accessors (including readers and writers)."
  (let (accessors)
    (dolist (slot slots)
      (destructuring-bind (&key accessor reader writer &allow-other-keys)
          (rest (ensure-list slot))
        (when accessor (push accessor accessors))
        (when reader (push reader accessors))
        (when writer (push writer accessors))))
    (setf accessors (remove-duplicates accessors))
    `(progn
       (export-always ',(cons name accessors))
       (defclass ,name ,supers
         ,slots
         ,@options))))
