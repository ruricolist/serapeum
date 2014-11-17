;;;; package.lisp

(defpackage #:serapeum
  (:use :cl :alexandria :optima :split-sequence :parse-number
        :named-readtables :tcr.parse-declarations-1.0)
  (:documentation "Utilities beyond Alexandria.")
  #+sb-package-locks (:lock t))
