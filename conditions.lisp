(in-package #:serapeum)

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
