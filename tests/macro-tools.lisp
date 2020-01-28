(in-package :serapeum.tests)

(def-suite macro-tools :in serapeum)
(in-suite macro-tools)

(test read-only-var
  (let ((x 1))
    (with-read-only-vars (x)
      (is (eql x 1)))))

(test read-only-var-warning
  (if (can-introspect-environment?)
      (signals warning
        (compile nil
                 '(lambda ()
                   (let ((x 1))
                     (with-read-only-vars (x)
                       (setf x 2))))))
      (pass)))

(test read-only-var-error
  (if (can-introspect-environment?)
      (signals warning
        (eval*
         '(let ((x 1))
           (with-read-only-vars (x)
             (setf x 2)))))
      (pass)))

(test read-only-var/special
  (is-false
   (let ((*special* t))
     (with-read-only-vars (*special*)
       (setf *special* nil)))))

(defconstant +one+ 1)
(test eval-if-constant
  (is (eql 1 (eval-if-constant '+one+))))

(defun permafoo () 'foo)
(define-compiler-macro permafoo () 'foo)

(test eval-if-constant/compiler-macro
  (is-false (constantp '(foo)))
  (is (eql 'foo (eval-if-constant '(permafoo)))))

(test eval-if-constant-in-env
  (macrolet ((constant-value/env (x &environment env)
               (eval-if-constant x env)))
    (symbol-macrolet ((x 1))
      (is (eql 1 (constant-value/env x))))))

(test require-form-for-eval
  (finishes
    (require-form-for-eval (list '(lambda ()))))
  (finishes
    (require-form-for-eval nil))
  (finishes
    (require-form-for-eval (list nil)))
  (signals error
    (require-form-for-eval (list '(+ 1 2)))))

(test sane-body-for-splice
  (signals error
    (sane-body-for-splice '(progn)))
  (signals error
    (sane-body-for-splice '(locally)))
  (signals error
    (sane-body-for-splice 'x))
  (finishes
    (sane-body-for-splice nil)))
