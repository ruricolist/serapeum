(defpackage #:serapeum/sum
  (:use)
  ;; Create this in a dedicated, unlocked package to prevent SBCL
  ;; package locking from keeping SUM being defined in a FLET
  (:export #:sum))
