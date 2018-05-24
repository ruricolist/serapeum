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

(test eval-if-constant-in-env
  (macrolet ((constant-value/env (x &environment env)
               (eval-if-constant x env)))
    (symbol-macrolet ((x 1))
      (is (eql 1 (constant-value/env x))))))
