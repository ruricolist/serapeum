(in-package #:serapeum)

(defconst standard-input-syntax-vars
  '(*package*
    *read-base*
    *read-default-float-format*
    *read-eval*
    *read-suppress*
    *readtable*)
  "Reader control variables bound by `with-standard-io-syntax'.")

(def standard-input-syntax-values
  (with-standard-io-syntax
    (mapcar #'symbol-value standard-input-syntax-vars))
  "Values of the standard reader control variables.")

(defun call/standard-input-syntax (fn)
  (progv standard-input-syntax-vars
      standard-input-syntax-values
    (funcall fn)))

(defmacro with-standard-input-syntax (&body body)
  "Like `with-standard-io-syntax', but only bind the variables that
control the reader, not the printer.

This may be preferable to using `with-standard-io-syntax' when loading
data, as it will not effect how errors are printed, thus preserving
debugging information."
  (with-thunk ((body :name with-standard-input-syntax))
    `(call/standard-input-syntax ,body)))
