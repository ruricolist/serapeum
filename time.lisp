(in-package :serapeum)

(export '(universal-to-unix
          unix-to-universal
          get-unix-time
          date-leap-year-p time-since time-until interval
          with-timing))

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0)
  "The Unix epoch as a universal time.")

(defconstant +seconds-in-hour+ (* 60 60))

(defconstant +seconds-in-day+ (* +seconds-in-hour+ 24))

;; Names chosen for conformity with local-time.
(defun universal-to-unix (time)
  "Convert a universal time to a Unix time."
  (- time +unix-epoch+))

(defun unix-to-universal (time)
  "Convert a Unix time to a universal time."
  (+ time +unix-epoch+))

(defun get-unix-time ()
  "The current time as a count of seconds from the Unix epoch."
  (universal-to-unix (get-universal-time)))

(defun date-leap-year-p (year)
  "Is YEAR a leap year in the Gregorian calendar?"
  (cond ((zerop (rem year 400)))
        ((zerop (rem year 100)) nil)
        ((zerop (rem year 4)))
        (t nil)))

(defun time-since (time)
  "Return seconds since TIME."
  (- (get-universal-time) time))

(defun time-until (time)
  "Return seconds until TIME."
  (- time (get-universal-time)))

(defun interval (&key (seconds 0) (minutes 0) (hours 0) (days 0)
                      (weeks 0) (months 0) (years 0)
                      (month-days 28) (year-days 365))
  "A verbose but readable way of specifying intervals in seconds.

Intended as a more readable alternative to idioms
like (let ((day-in-seconds #.(* 24 60 60))) ...)

Has a compiler macro."
  (round (+ seconds
            (* minutes 60)
            (* hours +seconds-in-hour+)
            (* days +seconds-in-day+)
            (* weeks +seconds-in-day+ 7)
            (* months +seconds-in-day+ month-days)
            (* years +seconds-in-day+ year-days))))

(defun interval-aux (&rest args)
  "Dummy to prevent recursive expansion."
  (apply #'interval args))

(define-compiler-macro interval (&whole decline
                                        &rest args
                                        &environment env)
  (if (loop for arg in args
            always (constantp arg env))
      `(load-time-value (interval-aux ,@args))
      decline))

(assert (= 31626000 (interval :years 1 :days 1 :hours 1)))

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
