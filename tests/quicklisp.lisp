(in-package :serapeum.tests)

(def-suite quicklisp :in serapeum)
(in-suite quicklisp)

(test cl-strftime
  (finishes
    (ql:quickload :cl-strftime :silent t)))

(test media-types
  (finishes
    (ql:quickload :media-types :silent t)))
