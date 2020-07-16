(in-package #:serapeum)

(defun count-cpus-string ()
  (labels ((run (cmd)
             "Run CMD, return its output as a string if it succeeds."
             (multiple-value-bind (s e r)
                 (uiop:run-program cmd
                                   :output :string
                                   :ignore-error-status t)
               (declare (ignore e))
               (and (zerop r) s)))
           (try (&rest results)
             "If there is a string in RESULTS, return it."
             (loop for result in results do
               (when (stringp result)
                 (return-from count-cpus-string
                   result)))))
    (handler-case
        (cond
          ((uiop:os-unix-p)
           ;; Linux
           (when (resolve-executable "nproc")
             (try (run '("nproc" "--all"))))
           ;; BSD, Darwin
           (when (resolve-executable "sysctl")
             (let ((vs '("hw.physicalcpu" "hw.ncpu" "hw.ncpufound")))
               (dolist (v vs)
                 (try (run `("sysctl" "-n" ,v))))))
           ;; Unix
           (let ((vs
                   '("_NPROCESSORS_CONF"
                     "NPROCESSORS_CONF"
                     "SC_NPROCESSORS_CONF"
                     "_SC_NPROCESSORS_CONF")))
             (cond ((resolve-executable "getconf")
                    (dolist (v vs)
                      (try (run `("getconf" ,v)))))
                   ;; Has a built-in getconf!
                   ((resolve-executable "ksh93")
                    (dolist (v vs)
                      (try (run
                            `("ksh93" "-c"
                                      ,(format nil "getconf ~a" v))))))))
           ;; Solaris?
           (when (resolve-executable "psrinfo")
             (try (run '("psrinfo" "-p")))))
          ((uiop:os-windows-p)
           (when (resolve-executable "wmic")
             (when-let* ((string
                          (run `("wmic" "cpu" "get"
                                        "NumberOfCores"
                                        "/value")))
                         (num (some (lambda (part)
                                      (every #'digit-char-p part))
                                    (split-sequence #\= string))))
               (return-from count-cpus-string
                 num)))
           (try (uiop:getenvp "NUMBER_OF_PROCESSORS")))
          (t nil))
      (serious-condition ()
        nil))))

(defun count-cpus (&key (default 2))
  "Try very hard to return a meaningful count of CPUs.

The second value is T if the number of processors could be queried,
`nil' otherwise."
  (let ((string (count-cpus-string)))
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
             ;; On some Lisps this would return T if empty.
             ,@(or body '(nil))))))))

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
