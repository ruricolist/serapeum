
(in-package :serapeum)

;;;# `find-keyword'

(declaim (ftype (function (string-designator)
                          (or keyword null))
                find-keyword))
(defun find-keyword (string)
  "If STRING has been interned as a keyword, return it.

Like `make-keyword', but preferable in most cases, because it doesn't
intern a keyword -- which is usually both unnecessary and unwise."
  ;; TODO Should the case of the readtable be respected?
  (if (keywordp string) string
      (find-symbol (string string) :keyword)))

;;;# `bound-value'

;;; `bound-value' is a contraction for the idiom of checking `boundp'
;;; before calling `symbol-value'.

(declaim (ftype (function (symbol &optional t)
                          (values t boolean))
                bound-value))
(defsubst bound-value (s &optional default)
  "If S is bound, return (values s t). Otherwise, return DEFAULT and nil."
  (if (boundp s)
      (values (symbol-value s) t)
      (values default nil)))

(defun (setf bound-value) (val sym)
  "Like `(setf (symbol-value SYM) VAL)', but raises an error if SYM is
  not already bound."
  (unless (boundp sym)
    (error "Attempt to set bound value of ~s, an unbound symbol.~%Value: ~a"
           sym val))
  (setf (symbol-value sym) val))
