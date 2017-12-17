(in-package #:serapeum)

(defvar *hook* nil
  "The hook currently being run.")
(declaim (type symbol *hook*))

(declaim (ftype (function (symbol (or function symbol) &key (:append t))
                          t)
                add-hook))
(defun add-hook (name fn &key append)
  "Add FN to the value of NAME, a hook."
  (declare (type (or function symbol) fn))
  (if (not append)
      (pushnew fn (symbol-value name))
      (unless (member fn (symbol-value name))
        (appendf (symbol-value name) (list fn)))))

(declaim (ftype (function (symbol (or function symbol))
                          t)
                remove-hook))
(defun remove-hook (name fn)
  "Remove fn from the symbol value of NAME."
  (removef (symbol-value name) fn))

(defmacro with-hook-restart (&body body)
  `(with-simple-restart (continue "Call next function in hook ~s" *hook*)
     ,@body))

(declaim (ftype (function (&rest symbol)
                          null)
                run-hooks))
(defun run-hooks (&rest hookvars)
  "Run all the hooks in all the HOOKVARS.
The variable `*hook*' is bound to the name of each hook as it is being
run."
  (dolist (*hook* hookvars)
    (dolist (fn (symbol-value *hook*))
      (with-hook-restart
        (funcall fn)))))

(declaim (ftype (function (symbol &rest t)
                          null)
                run-hook-with-args))
(defun run-hook-with-args (*hook* &rest args)
  "Apply each function in the symbol value of HOOK to ARGS."
  (dolist (fn (symbol-value *hook*))
    (with-hook-restart
      (apply fn args))))

(declaim (ftype (function (symbol &rest t)
                          boolean)
                run-hook-with-args-until-failure))
(defun run-hook-with-args-until-failure (*hook* &rest args)
  "Like `run-hook-with-args', but quit once a function returns nil."
  (loop for fn in (symbol-value *hook*)
        always (apply fn args)))

(declaim (ftype (function (symbol &rest t)
                          t)
                run-hook-with-args-until-success))
(defun run-hook-with-args-until-success (*hook* &rest args)
  "Like `run-hook-with-args', but quit once a function returns
non-nil."
  (loop for fn in (symbol-value *hook*)
          thereis (apply fn args)))
