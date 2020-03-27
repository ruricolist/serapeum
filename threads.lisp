(in-package #:serapeum)

(defun count-cpus-string (&key online)
  (labels ((try (cmd)
             (multiple-value-bind (s e r)
                 (uiop:run-program cmd
                                   :output :string
                                   :ignore-error-status t)
               (declare (ignore e))
               (when (zerop r)
                 (return-from count-cpus-string
                   s))))
           (try-for (vars fn)
             (dolist (var vars)
               (try (funcall fn var)))))
    (handler-case
        (cond
          ((uiop:os-unix-p)
           ;; Linux
           (when (resolve-executable "nproc")
             (try `("nproc" ,@(and (not online) '("--all")))))
           ;; BSD, Darwin
           (when (resolve-executable "sysctl")
             (try-for
              (if online
                  '("hw.availcpu" "hw.activecpu" "hw.ncpuonline")
                  '("hw.physicalcpu" "hw.ncpu" "hw.ncpufound"))
              (lambda (v)
                `("sysctl" "-n" ,v))))
           ;; Unix
           (when-let (exe
                      (or (resolve-executable "getconf")
                          ;; Has a built-in getconf!
                          (resolve-executable "ksh93")))
             (try-for
              (if online
                  '("_NPROCESSORS_ONLN"
                    "NPROCESSORS_ONLN"
                    "SC_NPROCESSORS_ONLN"
                    "_SC_NPROCESSORS_ONLN")
                  '("_NPROCESSORS_CONF"
                    "NPROCESSORS_CONF"
                    "SC_NPROCESSORS_CONF"
                    "_SC_NPROCESSORS_CONF"))
              (if (equal (pathname-name exe) "getconf")
                  (lambda (v)
                    `("getconf" ,v))
                  (lambda (v)
                    `("ksh93" "-c"
                              ,(format nil "getconf ~a" v))))))
           ;; Solaris?
           (when (resolve-executable "psrinfo")
             ;; TODO online?
             (try '("psrinfo" "-p"))))
          ((uiop:os-windows-p)
           (when (resolve-executable "wmic")
             (when-let* ((string
                          (uiop:run-program
                           `("wmic" "cpu" "get"
                                    ,(if online
                                         "NumberOfEnabledCore" ;sic
                                         "NumberOfCores")
                                    "/value")
                           :output :string
                           :ignore-error-status t))
                         (num (some (lambda (part)
                                      (every #'digit-char-p part))
                                    (split-sequence #\= string))))
               (return-from count-cpus-string
                 num)))
           (uiop:getenv "NUMBER_OF_PROCESSORS"))
          (t nil))
      (serious-condition ()
        nil))))

(defun count-cpus (&key (default 2) online)
  "Try very hard to return a meaningful count of CPUs.
If ONLINE is non-nil, try to return only the active CPUs.

The second value is T if the number of processors could be queried,
`nil' otherwise."
  (let ((string (count-cpus-string :online online)))
    (if string
        (let ((int (parse-integer string :junk-allowed t)))
          (if int
              (values int t)
              (values default nil)))
        (values default nil))))

;;; NB We used to use non-recursive locks here, but it turns out all
;;; languages providing a `synchronized' keyword (Java, Objective-C,
;;; C#, D) use recursive locks, so that is what we use now.

(eval-when (:compile-toplevel :load-toplevel)
  (defconstant +lock-class+ (class-of (bt:make-recursive-lock))))

;;; We need more space for locks than you might expect. In, say, Java,
;;; only a handful of locks exist at a time. But in Lisp I often use
;;; `synchronized' on symbols, which of course are rarely \(if ever)
;;; garbage-collected. Thus our use of a hash table instead of a list.

(defvar *monitors*
  (tg:make-weak-hash-table
   :weakness :key
   ;; This should be plenty big enough to never need resizing.
   :size 512))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lock-form (object objectp env string)
    (cond ((not objectp)
           (let ((string (or string "Anonymous critical section")))
             `(load-time-value (bt:make-recursive-lock ,string))))
          ((constantp object env)
           `(load-time-value
             (ensure-monitor ,object ,string)))
          (t `(ensure-monitor ,object ,string)))))

;;; TODO A natural extension to the `synchronized' syntax would be to
;;; allow multiple locks to be taken at once. This might be useful if
;;; they could be implicitly re-ordered, using some kind of global
;;; order, to avoid deadlocks.

(defmacro synchronized ((&optional (object nil objectp)) &body body &environment env)
  "Run BODY holding a unique lock associated with OBJECT.
If no OBJECT is provided, run BODY as an anonymous critical section.

If BODY begins with a literal string, attach the string to the lock
object created (as the argument to `bt:make-recursive-lock')."
  (multiple-value-bind (string? body)
      (if (stringp (first body))
          (values (first body) (rest body))
          (values nil body))
    (let* ((form (lock-form object objectp env string?)))
      (with-gensyms (lock)
        `(let ((,lock ,form))
           (bt:with-recursive-lock-held (,lock)
             ,@body))))))

(defgeneric monitor (object)
  (:documentation "Return a unique lock associated with OBJECT."))

(defmethod monitor ((object #.+lock-class+))
  object)

(defmethod monitor ((object t))
  nil)

(defun ensure-monitor (object string)
  (or (monitor object)
      (let ((string (or string "Monitor")))
        (flet ((ensure-monitor (object string)
                 (ensure-gethash object *monitors*
                                 (bt:make-recursive-lock string))))
          ;; Clozure has lock-free hash tables.
          #+ccl (ensure-monitor object string)
          #-ccl (synchronized ()
                  (ensure-monitor object string))))))

(defclass synchronized ()
  ((monitor :initform (bt:make-recursive-lock)
            :reader monitor))
  (:documentation "Mixin for a class with its own monitor."))
