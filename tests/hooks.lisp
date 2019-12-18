(in-package :serapeum.tests)

(def-suite hooks :in serapeum)
(in-suite hooks)

(defun call/temp-hook (fn)
  (let ((hook (gensym)))
    (setf (symbol-value hook) nil)
    (unwind-protect
         (funcall fn hook)
      (makunbound hook))))

(defmacro with-temp-hook ((hook &key) &body body)
  `(call/temp-hook
    (lambda (,hook)
      ,@body)))

(test run-hook
  (with-temp-hook (hook)
    (let ((list '()))
      (flet ((add-1 ()
               (push 1 list))
             (add-2 ()
               (push 2 list))
             (add-x (x)
               (push x list)))
        (add-hook hook #'add-2)
        (add-hook hook #'add-1 :append t)
        (run-hooks hook)
        (is (equal list '(1 2)))))))

(test run-hook-with-args ()
  (with-temp-hook (hook)
    (let ((list '()))
      (flet ((add (n) (push n list)))
        (add-hook hook #'add)
        (run-hook hook '2)
        (run-hook hook '1)
        (is (equal list '(1 2)))))))

(test run-hook-until-failure
  (with-temp-hook (hook)
    (add-hook hook (constantly nil))
    (add-hook hook
              (lambda () (fail "This function should not run"))
              :append t)
    (run-hook-until-failure hook)))

(test run-hook-until-success
  (with-temp-hook (hook)
    (add-hook hook (constantly t))
    (add-hook hook
              (lambda () (fail "This function should not run"))
              :append t)
    (run-hook-until-success hook)))
