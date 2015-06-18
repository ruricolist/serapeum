(in-package #:serapeum)

(defmacro with-timing ((&key quiet gc repeat)
                       &body body)
  "A convenience wrapper around TIME.

QUIET suppresses both the return value and any output to
`*standard-output*'.

REPEAT specifies a number of repetitions.

If GC is non-nil, perform a garbage collection before running BODY.
This can be useful with repeated trials, when you want to make sure
the running time of the *nth* run is not distorted by cleaning up
after the runs before it."
  (let* ((body `(progn ,@body))
         (form body))
    (when repeat
      (setf form `(loop repeat ,repeat do ,form)))
    (when quiet
      (setf form `(prog1 nil
                    (let ((*standard-output* (make-broadcast-stream)))
                      ,form))))
    (if gc
        `(progn (tg:gc) (time ,form))
        `(time ,form))))
