;;;; package.lisp

(defpackage #:serapeum
  (:use :cl :alexandria :optima :split-sequence :parse-number)
  (:documentation "Utilities beyond Alexandria.")
  #+sb-package-locks (:lock t))
