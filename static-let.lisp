(defpackage #:serapeum/static-let
  (:use #:cl)
  ;; Imports for actual use.
  (:import-from #:alexandria
                #:required-argument
                #:once-only
                #:ensure-car
                #:deletef
                #:parse-body
                #:when-let
                #:mappend)
  (:import-from #:serapeum
                #:partition-declarations
                #:keep
                #:static-load-time-value)
  ;; Imports for implementing.
  (:import-from #:serapeum
                #:recklessly-continue
                #:static-binding-flush-error
                #:static-binding-flush-error-group
                #:static-binding-flush-error-all-groups-p
                #:static-binding-active-error
                #:flush-static-binding-group
                #:flush-all-static-binding-groups
                #:static-let
                #:static-let*)
  #+sbcl (:implement #:serapeum))

(in-package #:serapeum/static-let)

;;; Static binding structure

(defstruct (static-binding (:constructor %make-static-binding)
                           (:copier nil)
                           (:predicate nil)
                           (:conc-name #:||))
  (name 'unknown :type symbol)
  (value nil :type t)
  (initializedp nil :type boolean)
  (read-only nil :type boolean)
  (lock nil :read-only t :type (or null bt:lock)))

(declaim (inline value-ref (setf value-ref)))

(defun value-ref (static-binding)
  (value static-binding))

(defun (setf value-ref) (value static-binding)
  (if (read-only static-binding)
      (error "Cannot mutate a ~s binding: ~s"
             'static-let
             (name static-binding))
      (let ((lock (lock static-binding)))
        (if lock
            (bt:with-lock-held (lock)
              (setf (value static-binding) value))
            (setf (value static-binding) value)))))

(defun make-static-binding (&key once name read-only)
  (%make-static-binding
   :lock (if once (bt:make-lock "Static binding lock") nil)
   :name name
   :read-only read-only))

;;; Condition helpers

(defun report-group-substring (&key group all-groups-p)
  (format nil "~:[static bindings from group ~S~;ALL static bindings~]"
          all-groups-p group))

(defun recklessly-continue (&optional condition)
  "Invokes the last bound RECKLESSLY-CONTINUE restart. Returns NIL if
no such restart was bound or if the restart failed to transfer control."
  (when-let ((restart (find-restart 'recklessly-continue condition)))
    (invoke-restart restart)))

;;; Flush error

(defun report-static-binding-flush-error (condition stream)
  (let* ((group (static-binding-flush-error-all-groups-p condition))
         (all-groups-p (static-binding-flush-error-group condition))
         (substring (report-group-substring :group group
                                            :all-groups-p all-groups-p)))
    (format stream
            "Requested to flush all values for ~A and ~
             to restore them to their uninitialized state.~@
             This operation is unsafe to perform while any other ~
             threads are trying to access these bindings.~:[~@
             Remove this error message with :ARE-YOU-SURE-P T.~;~]"
            substring all-groups-p)))

(define-condition static-binding-flush-error (error)
  ((group :initarg :group
          :reader static-binding-flush-error-group)
   (all-groups-p :initarg :all-groups-p
                 :reader static-binding-flush-error-all-groups-p))
  (:default-initargs :group nil :all-groups-p nil)
  (:report report-static-binding-flush-error)
  (:documentation
   "A subtype of error related to attempts to flush static bindings
in a potentially unsafe way."))

(defun static-binding-flush-error (&optional (group nil groupp))
  (let* ((condition (make-condition 'static-binding-flush-error
                                    :group group :all-groups-p (not groupp)))
         (continue-string
           (format nil "Flush values for ~A."
                   (report-group-substring :group group
                                           :all-groups-p (null groupp)))))
    (cerror continue-string condition)))


;;; Active binding error

(defun report-static-binding-active-error (condition stream)
  (let* ((group (static-binding-flush-error-all-groups-p condition))
         (all-groups-p (static-binding-flush-error-group condition))
         (substring (report-group-substring :group group
                                            :all-groups-p all-groups-p)))
    (format stream
            "Requested to flush all values for ~A and ~
             to restore them to their uninitialized state, ~
             but a binding from group ~S is currently active.~@
             This can cause undefined behavior if any of the ~
             bindings is accessed again before it is reinitialized."
            substring group)))

(define-condition static-binding-active-error (static-binding-flush-error
                                               program-error) ()
  (:default-initargs :group (required-argument :group))
  (:report report-static-binding-active-error)
  (:documentation
   "A subtype of error related to attempts to flush static bindings
which are currently active."))

(defun static-binding-active-error (group &optional all-groups-p)
  (let ((condition (make-condition 'static-binding-active-error
                                   :group group :all-groups-p all-groups-p))
        (continue-string
          (format nil "Flush values for ~A."
                  (report-group-substring :group group
                                          :all-groups-p all-groups-p))))
    (with-simple-restart (recklessly-continue continue-string)
      (error condition))
    nil))

;;; Variables

(defvar *flushing-lock* (bt:make-lock "Static binding flushing lock"))

(defvar *flushable-bindings* (make-hash-table))

(defvar *active-groups* '())

;;; Flushing mechanisms

(defun %flush (group)
  (let ((count 0))
    (dolist (pointer (gethash group *flushable-bindings*))
      :start
      (let ((binding (tg:weak-pointer-value pointer)))
        (when (null binding)
          (go :start))
        (if (lock binding)
            (bt:with-lock-held ((lock binding))
              (setf (initializedp binding) nil
                    (value binding) nil))
            (setf (initializedp binding) nil
                  (value binding) nil))
        (incf count)))
    (setf (gethash group *flushable-bindings*) '())
    count))

(defun flush-static-binding-group (group &key are-you-sure-p)
  "Flushes all static binding values in binding group `group' and
restores them to their uninitialized state, forcing any initforms
for these static bindings to be reevaluated whenever control
next reaches the respective `static-let'/`static-let*'. Returns the
number of live bindings flushed that way.

This operation is unsafe to perform while any other threads are
trying to access these bindings; proper synchronization is left
to the user. Therefore, a continuable error is signaled unless
Lisp is running single-threaded or `are-you-sure-p' is true.

Note that a static binding that was created as `:flushablep nil'
will not be affected by this operation."
  (declare (ignorable are-you-sure-p))
  (when (member group *active-groups*)
    (static-binding-active-error group))
  (bt:with-lock-held (*flushing-lock*)
    (if (gethash group *flushable-bindings*)
        (%flush group)
        0)))

(defun flush-all-static-binding-groups ()
  "Flush all static binding values in ALL binding groups and
restore them to their uninitialized state, forcing any initforms
for these static bindings to be reevaluated whenever control
next reaches the respective `static-let'/`static-let*'.  Returns the
number of live bindings flushed that way.

This operation is unsafe to perform while any other threads are
trying to access these bindings; proper synchronization is left
to the user. In addition, this operation will clear ALL values,
including these which were not bound by the programmer. This can
lead to unintended behavior, hence, a continuable error is signaled
unless Lisp is running single-threaded.

This function is useful e.g. when deploying Lisp binaries in order
to not include static binding values in the resulting Lisp image.

Note that a static binding that was created as `:flushablep nil'
will not be affected by this operation."
  (dolist (group (remove-duplicates *active-groups*))
    (static-binding-active-error group t))
  (bt:with-lock-held (*flushing-lock*)
    (if (> (hash-table-count *flushable-bindings*) 0)
        (let ((result 0))
          (flet ((flush (key value)
                   (declare (ignore value))
                   (incf result (%flush key))))
            (maphash #'flush *flushable-bindings*)
            result))
        0)))

(pushnew 'flush-all-static-binding-groups uiop:*image-dump-hook*)

;;; Binding canonicalizer

(deftype variable-name () '(and symbol (not (satisfies constantp))))

(defun canonicalize-binding (binding)
  ;; Returns a list of seven elements:
  ;; * user-provided name
  ;; * initform
  ;; * type
  ;; * gensym for holding a reference to the actual load-time-value binding
  ;; * boolean stating if the evaluation of initform should be synchronized
  ;; * boolean stating if the binding should be flushable
  ;; * object naming the flushing group
  ;; * boolean static if the binding is read-only
  (let (name
        (value nil)
        (type 't)
        (gensym (gensym "STATIC-BINDING"))
        (once nil)
        (flush t)
        (in *package*)
        (read-only nil))
    (etypecase binding
      ;; VAR
      (variable-name
       (setf name binding))
      ;; (VAR)
      ((cons variable-name null)
       (setf name (first binding)))
      ;; (VAR VALUE)
      ((cons variable-name (cons t null))
       (setf name (first binding)
             value (second binding)))
      ;; (VAR VALUE &KEY TYPE ONCE FLUSH IN)
      ((cons variable-name (cons t cons))
       (destructuring-bind (new-name new-value
                            &key
                              ((:type new-type) nil new-type-p)
                              ((:once new-once) nil new-once-p)
                              ((:flush new-flush) nil new-flush-p)
                              ((:in new-in) nil new-in-p)
                              ((:read-only new-read-only) nil new-read-only-p))
           binding
         (setf name new-name
               value new-value)
         (when new-type-p (setf type new-type))
         (when new-once-p (setf once new-once))
         (when new-flush-p (setf flush new-flush))
         (when new-in-p (setf in new-in))
         (when new-read-only-p (setf read-only new-read-only)))))
    (list name value type gensym once flush in read-only)))

(defmacro with-canonicalized-binding-accessors (() &body body)
  `(flet ((name (x) (elt x 0))
          (value (x) (elt x 1))
          (type (x) (elt x 2))
          (sym (x) (elt x 3))
          (once (x) (elt x 4))
          (flush (x) (elt x 5))
          (in (x) (elt x 6))
          (read-only (x) (elt x 7)))
     (declare (ignorable #'name #'value #'type #'sym #'once #'flush #'in #'read-only))
     ,@body))

;;; Macro element generators

(defun make-let-binding (x)
  (with-canonicalized-binding-accessors ()
    (let ((once (once x)))
      `(,(sym x)
        (static-load-time-value (make-static-binding
                                 :once ,once
                                 :name ',(name x)
                                 :read-only ,(read-only x)))))))

(defun make-flusher (x)
  (with-canonicalized-binding-accessors ()
    (if (flush x)
        (let ((in (in x)))
          (once-only (in)
            `(bt:with-lock-held (*flushing-lock*)
               ;; Remove all dead weak pointers that might
               ;; have already accumulated.
               (deletef
                (gethash ,in *flushable-bindings*)
                nil :key #'tg:weak-pointer-value)
               ;; Push the new weak pointer into the list.
               (push (tg:make-weak-pointer ,(sym x))
                     (gethash ,in *flushable-bindings*)))))
        '())))

(defun make-initform (x)
  (with-canonicalized-binding-accessors ()
    (let* ((sym (sym x))
           (body `(setf (value ,sym) ,(value x)
                        (initializedp ,sym) t))
           (flusher (make-flusher x)))
      (if (once x)
          `(unless (initializedp ,sym)
             (bt:with-lock-held ((lock ,sym))
               (unless (initializedp ,sym)
                 ,body
                 ,flusher)))
          `(unless (initializedp ,sym)
             ,body
             ,flusher)))))

(defun make-macrolet-binding (x)
  (with-canonicalized-binding-accessors ()
    `(,(name x) (value-ref ,(sym x)))))

(defun make-type-declaration (x)
  (with-canonicalized-binding-accessors ()
    `(type ,(type x) ,(name x))))

(defun make-active-groups-binding (bindings)
  (with-canonicalized-binding-accessors ()
    (let ((groups (mapcar #'in bindings)))
      `(list* ,@groups *active-groups*))))

(defun check-no-dynamic-extent (names declarations)
  "Check that none of NAMES are declared `dynamic-extent' in DECLARATIONS."
  (declare (notinline keep))
  (let* ((relevant-declarations (partition-declarations names declarations))
         (dynamics
           (mappend #'cdr
                    (keep 'dynamic-extent
                          (mappend #'cdr relevant-declarations)
                          :key #'car))))
    (when-let (intersection (intersection names dynamics))
      (error "~s bindings cannot be declared dynamic-extent: ~a"
             'static-let
             intersection))))

(defun parse-static-let (bindings body)
  (let* ((bindings (mapcar #'canonicalize-binding bindings))
         (let-bindings (mapcar #'make-let-binding bindings))
         (initforms (mapcar #'make-initform bindings))
         (macrolet-bindings (mapcar #'make-macrolet-binding bindings))
         (type-declarations (mapcar #'make-type-declaration bindings))
         (active-groups-binding (make-active-groups-binding bindings)))
    (multiple-value-bind (real-body declarations) (parse-body body)
      (check-no-dynamic-extent (mapcar #'car macrolet-bindings) declarations)
      `(let (,@let-bindings)
         ,@initforms
         (symbol-macrolet (,@macrolet-bindings)
           (declare ,@type-declarations)
           ,@declarations
           (let ((*active-groups* ,active-groups-binding))
             (declare (dynamic-extent *active-groups*))
             ,@real-body))))))

(defun parse-static-let* (bindings body env)
  (case (length bindings)
    (0 `(locally ,@body))
    (1 `(static-let ,bindings ,@body))
    (t (multiple-value-bind (body declarations) (parse-body body)
         (check-no-dynamic-extent (mapcar #'car bindings) declarations)
         (destructuring-bind (binding . other-bindings) bindings
           (let ((binding-name (ensure-car binding)))
             (multiple-value-bind (declarations other-declarations)
                 (partition-declarations (list binding-name) declarations env)
               `(static-let (,binding)
                  ,@declarations
                  (static-let* (,@other-bindings)
                    ,@other-declarations
                    ,@body)))))))))

(defmacro static-let ((&rest bindings) &body body)
  "Like `let', except the variables are only initialized once and
retain their values between different invocations of `body'.

Every static binding is similar to a `let' binding, except it can have
additional keyword arguments:

- `type' Denotes the type of the variable.
- `once' If true, then binding initialization and mutation will be
         thread-safe.
- `flush' If true, this binding will be flushable. Defaults to true.
- `in' Denotes the static binding group in which the binding will be
       placed for flushing. Defaults to the value of `*package'.
- `read-only' If true, then the binding cannot be mutated with `setf'.

Static bindings can be flushed via `flush-static-binding-group' and
`flush-all-static-binding-groups'; the latter is automatically pushed
into `uiop:*dump-image-hooks*' by Serapeum.

An unflushable static binding will carry its value over into dumped
Lisp binaries."
  (parse-static-let bindings body))

(defmacro static-let* ((&rest bindings) &body body &environment env)
  "Like `let*', except the variables are only initialized once and
retain their values between different invocations of `body'.

Every static binding is similar to a `let' binding, except it can have
additional keyword arguments:

- `type' Denotes the type of the variable.
- `once' If true, then binding initialization and mutation will be
         thread-safe.
- `flush' If true, this binding will be flushable. Defaults to true.
- `in' Denotes the static binding group in which the binding will be
       placed for flushing. Defaults to the value of `*package'.
- `read-only' If true, then the binding cannot be mutated with `setf'.

Static bindings can be flushed via `flush-static-binding-group' and
`flush-all-static-binding-groups'; the latter is automatically pushed
into `uiop:*dump-image-hooks*' by Serapeum.

An unflushable static binding will carry its value over into dumped
Lisp binaries."
  (parse-static-let* bindings body env))
