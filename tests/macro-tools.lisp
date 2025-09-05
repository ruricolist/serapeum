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

(test expect-single-form
  (finishes
    (expect-single-form (list '(lambda ()))))
  (finishes
    (expect-single-form nil))
  (finishes
    (expect-single-form (list nil)))
  (signals error
    (expect-single-form (list '(+ 1 2)))))

(test expect-form-list
  (signals error
    (expect-form-list '(progn)))
  (signals error
    (expect-form-list '(locally)))
  (signals error
    (expect-form-list 'x))
  (finishes
    (expect-form-list nil)))

(defun my-plus (x y)
  (+ x y))

(define-modify-macro my-incf (&optional (delta 1)) my-plus)

(define-post-modify-macro my-incf* (&optional (delta 1)) my-plus)

(defclass foo ()
  ((x :accessor x
      :initform 42)))

(let ((foo (make-instance 'foo)))
  (my-incf* (x foo))
  (x foo))

(test post-modify-macro
  (is (= 43
         (let ((foo (make-instance 'foo)))
           (my-incf (x foo))
           (x foo))))
  (is (= 43
         (let ((foo (make-instance 'foo)))
           (my-incf* (x foo))
           (x foo))))
  (is (= 42
         (let ((x 42))
           (my-incf* x)))))

(define-case-macro faux-case (expr &body clauses)
    (:default default)
  `(cond ,@(loop for (key . body) in clauses
                 collect `((eql ,expr ,key) ,@body))
         (t ,@default)))

(test faux-case
  (dolist (target '(tagbody flet))
    (let ((*case-macro-target* target))
      (is (eql 'bit
               (funcall
                (compile
                 nil
                 (eval
                  `(lambda (x)
                     (faux-case x
                       ((0 1)
                        ;; Multiple forms.
                        (let ((*standard-output* (make-broadcast-stream)))
                          (print "hello"))
                        'bit)
                       (t 'non-bit)))))
                1))))))
