(in-package :serapeum.tests)

;;; Auxiliary file for defining-types, to make sure unit types retain
;;; their identity across files.

(defunit zero)

(defun get-zero () zero)

(defunion tree
  leaf
  (node (value integer)
        (left tree)
        (right tree)))

(defun get-leaf () leaf)
