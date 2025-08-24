(defpackage #:serapeum/sum
  (:use)
  ;; Export this from a dedicated, unlocked package to prevent SBCL
  ;; package locking from keeping SUM being defined in a FLET
  (:export #:sum))
