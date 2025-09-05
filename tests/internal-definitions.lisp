(in-package :serapeum.tests)

(def-suite internal-definitions :in serapeum)
(in-suite internal-definitions)

(test internal-definitions
  (is (eql 2
           (local
             (def x 1)
             (def y (1+ x))
             y)))

  (is (eql 3
           (local
             (defun adder (y)
               (+ x y))
             (def x 2)
             (adder 1))))

  (is (eql 3
           (funcall
            (local
              (defun adder (y)
                (+ x y))
              (def x 2)
              #'adder)
            1)))

  (is (eql 'plus
           (local
             (plus 2 2)
             (defun plus (x y)
               (+ x y)))))

  (is (not (eql 1
                (let ((y 2))
                  (local
                    ;; Check that we don't hoist the constant binding of X.
                    (def ret nil)
                    (setq ret x)
                    (def x y)
                    (def x 1)
                    ret)))))

  ;; Closures in vars.
  (is (eql 4
           (funcall
            (local
              (def adder (lambda (x) (+ x y)))
              (def y 2)
              adder)
            2)))

  (is (eql 1
           (local
             (let ((x 1))
               (defun fn ()
                 x))
             (fn))))

  (is (eql 1
           (local
             (let* ((x 1))
               (defun fn ()
                 (values x)))
             (fn))))

  (is (eql 1
           (local
             (multiple-value-bind (x) (values 1)
               (defun fn ()
                 x))
             (fn))))

  (is (eql 4
           (local
             (defconst x (+ 2 2))
             x)))

  (is (eql 2
           (local
             (let* ((x 1)
                    (y 1))
               (+ x y)))))

  (is (equal '(1 2 3)
             (local
               (destructuring-bind (&key x y z) '(:x 1 :y 2 :z 3)
                 (list x y z))))))

(test let-over-def
  (is (eql 3
           (local
             (def x 1)
             (let ((x 2))
               (def x 3))
             x)))
  (is (eql 2
           (let ((y 2))
             (local
               (let ((x 1))
                 (def x y))
               x)))))

(test let-over-def-vs-hoisting
  (is (equal '(1 3)
             (let (a b)
               (local
                 (def x 1)
                 (setf a x)
                 (let ((x 2))
                   (def x 3))
                 (setf b x))
               (list a b)))))

;; Test that the binding from the or doesn't end up outside the
;; symbol macrolet form.
(test symbol-macrolet-scope
  (finishes
    (eval*
     '(let ((xy (cons 'x 'y)))
       (local
         (symbol-macrolet ((x (car xy)))
           (or x 1)))))))

(test symbol-macro-before-macro
  (is (eql 1
           (local
             (define-symbol-macro x 1)
             (defmacro foo () 'foo)
             x))))

(test expr-env
  (is (eql 2
           (let ((a 1))
             (local
               (+ 2 2)
               (define-symbol-macro x a)
               (def y (+ x 1))
               y)))))

(test flet-over-defalias
  (is (eql 3
           (local
             (defun x ()
               1)
             (flet ((x () 2))
               (defalias x (constantly 3)))
             (x)))))

(test redefining-functions
  (is (eql 3
           (local
             (defalias x (constantly 1))
             (defalias x (constantly 2))
             (defun x () 3)
             (x)))))

(test internal-definitions+macros
  (is (equal '(x)
             (local
               (declaim (ignorable x))
               (defmacro q (x)
                 `(quote ,x))

               (def x 1)

               (list (q x)))))

  ;; Ensure that forms are partially expanded in the right env.

  (is (eql 'defined
           (local
             (defmacro define-function (name args &body body)
               `(defun ,name ,args
                  ,@body))
             (define-function fn () 'defined)
             (fn))))

  (is (equal 1
             (local
               (defmacro always-1 () 1)
               (defun fn () (always-1))
               (fn))))

  ;; Defmacro inside progn.
  (is (eql 2
           (local
             (progn
               (defmacro m () 2)
               (m)))))

  (signals error
    (eval*
     '(let (x)
       (flet ((m () 1))
         (local
           (setq x (m))
           (defmacro m () 2)
           x)))))

  (local
    (define-do-macro do-seq ((var seq &optional return) &body body)
      `(map nil (lambda (,var) ,@body) ,seq))
    (is (equal '(1 2 3)
               (collecting
                 (do-seq (x #(1 2 3))
                   (collect x)))))))

(test internal-definitions+progn
  (is (equal '((1) (2))
             (multiple-value-list
              (local (with-collectors (xs ys) (xs 1) (ys 2)))))))

(test internal-definitions+symbol-macros
  (is (equal '(1 1)
             (let (a b)
               (local
                 (define-symbol-macro x (setq a 1))
                 x
                 (define-symbol-macro redefine-x (def x (setq b 1)))
                 redefine-x
                 x)
               (list a b)))))

(test internal-definitions+symbol-macrolet
  (is (equal (local
               (define-symbol-macro x 1)
               (symbol-macrolet ((x 2))
                 (def x 3))
               x)
             (local
               (define-symbol-macro x 1)
               (let ((x 2))
                 (def x 3))
               x)))

  (is (equal (local
               (define-symbol-macro x 1)
               (let ((x 2))
                 (def x 3)
                 x))
             (local
               (define-symbol-macro x 1)
               (symbol-macrolet ((x 2))
                 (def x 3)
                 x))))

  (is (eql 3
           (local
             (def x 1)
             (let ((x 2))
               (define-symbol-macro x 3))
             x))))

(test exprs-before-macros
  (is (eql 1
           (let (a (b 1))
             (local
               (setq a b)
               (define-symbol-macro b 2))
             a)))

  (signals error
    (eval*
     '(flet ((b () 1))
       (let (a)
         (local
           (setq a (b))
           (defmacro b () 2)))))))

(test expanding-bindings
  (is (eql 2
           (local
             (let (x)
               (setq x 2)
               (def x 1)
               x))))

  (is (null
       (local
         (let (y)
           (def x y))
         x))))

(test symbol-macrolet-over-let
  (is (eql 1
           (local
             (symbol-macrolet ((x 1))
               (let ((x x))
                 x))))))

(test symbol-macrolet-over-flet
  (is (eql 2
           (local
             (symbol-macrolet ((x 1))
               (flet ((x (y)
                        "Add x and y."
                        (+ x y)))
                 (x 1)))))))

(test symbol-macrolet-over-labels
  (is (eql 2
           (local
             (symbol-macrolet ((x 1))
               (labels ((x (y)
                          (declare (number x))
                          "Add x and y."
                          (+ x y)))
                 (x 1)))))))

(test blocks-introduce-lexical-contours
  (is-true (occurs "foo"
                   (macroexpand
                    '(local (block block
                              (progn "foo")
                              (progn
                                (with-simple-restart (abort "Abort"))
                                "bar"))))
                   :test #'equal)))

(test no-splice-in-prog1
  (is (eql 'goodbye
           (block nil
             (local
               (prog1 (progn 'hello 'world)
                 ;; Make sure this branch is not eliminated.
                 (return 'goodbye)))))))

(define-symbol-macro global-symbol-macro 1)

(test symbol-macro-setf-values-scope
  (is (eql 4
           (local
             ;; This expands into a `(setf (values ..))` form. If we
             ;; macroexpand the setf, we get a (nonexistent) backing
             ;; variable for the global symbol macro.
             (def (values global-symbol-macro x) 4 'x)
             global-symbol-macro))))
