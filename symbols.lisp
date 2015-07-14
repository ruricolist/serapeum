(in-package :serapeum)

;;;# `find-keyword'

(defun find-keyword (string)
  "If STRING has been interned as a keyword, return it.

Like `make-keyword', but preferable in most cases, because it doesn't
intern a keyword -- which is usually both unnecessary and unwise."
  ;; TODO Should the case of the readtable be respected?
  (find-symbol (string string) :keyword))

;;;# `bound-value'

;;; `bound-value' is a contraction for the idiom of checking `boundp'
;;; before calling `symbol-value'.

(defsubst bound-value (s &optional default)
  "If S is bound, return (values s t). Otherwise, return DEFAULT."
  (if (boundp s)
      (values (symbol-value s) t)
      (values default nil)))

