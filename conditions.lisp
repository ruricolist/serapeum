(defpackage :serapeum/conditions
  (:documentation "Condition handling utilities.")
  (:use :cl :alexandria)
  #+sb-package-locks (:lock t)
  (:export
   ;; NB Not exporting ignoring as it's deprecated.
   :maybe-invoke-restart))

(in-package #:serapeum/conditions)

(defmacro ignoring (type &body body)
  "DEPRECATED: use `alexandria:ignore-some-conditions' instead."
  (simple-style-warning "~s is deprecated, use ~s instead"
                        'ignoring 'ignore-some-conditions)
  `(ignore-some-conditions (,type)
     ,@body))

(defun maybe-invoke-restart (restart &rest values)
  "When RESTART is active, invoke it with VALUES."
  (when (find-restart restart)
    (apply #'invoke-restart restart values)))
