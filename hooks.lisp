(in-package #:serapeum)

(export '(*hook*
          add-hook remove-hook
          run-hooks run-hook-with-args
          run-hook-with-args-until-failure
          run-hook-with-args-until-success))

(defvar *hook* nil
  "The hook currently being run.")
(declaim (type symbol *hook*))

(defun add-hook (name fn)
  "Add FN to the value of NAME, a hook."
  (check-type fn (or function symbol))
  (pushnew fn (symbol-value name)))

(defun remove-hook (name fn)
  "Remove fn from the symbol value of NAME."
  (removef (symbol-value name) fn))

(defun run-hooks (&rest hookvars)
  "Run all the hooks in all the HOOKVARS.
The variable `*hook*' is bound to each hook as it is being run."
  (dolist (hook hookvars)
    (let ((*hook* hook))
      (dolist (fn (symbol-value hook))
        (funcall fn)))))

(defun run-hook-with-args (hook &rest args)
  "Apply each function in the symbol value of HOOK to ARGS."
  (check-type hook symbol)
  (let ((*hook* hook))
    (dolist (fn (symbol-value hook))
      (apply fn args))))

(defun run-hook-with-args-until-failure (hook &rest args)
  "Like `run-hook-with-args', but quit once a function returns nil."
  (let ((*hook* hook))
    (loop for fn in (symbol-value hook)
          always (apply fn args))))

(defun run-hook-with-args-until-success (hook &rest args)
  "Like `run-hook-with-args', but quit once a function returns
non-nil."
  (let ((*hook* hook))
    (loop for fn in (symbol-value hook)
          thereis (apply fn args))))
