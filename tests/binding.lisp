(in-package :serapeum.tests)

(def-suite binding :in serapeum)
(in-suite binding)

(test let1-match
  (is (equal '(1 2 3)
             (match (list 2 3)
               ((let1 x 1 (list y z))
                (list x y z))))))

(test lret
  (is (equal 1 (lret () 1)))
  (is (equal 2
             (lret ((x 1)
                    (y 2))
               x))))

(test lret-atom
  (is (null (lret (x y)
              (declare (ignore x))
              1))))

(test lret*
  (is (equal 1 (lret* () 1)))
  (is (equal 2
             (lret* ((y 1)
                     (y 2))
               (declare (ignorable y))
               3))))

(test letrec
  "Check that `letrec' and `letrec*' behave differently."
  ;; The exact error depends on the implementation.
  (signals error
    (letrec ((f (constantly t))
             (a (funcall f)))
      a))

  (finishes
    (letrec* ((f (constantly t))
              (a (funcall f)))
      (is-true a))))

(test letrec-atom
  "Test that letrec handles atom in the binding list."
  (is (null (letrec (x) x)))
  (is (null (letrec* (x) x))))

(test letrec-constant
  "Test that letrec handles variables bound to constants."
  (letrec ((x 1)
           (y (lambda () y)))
    (is (functionp y))
    (is (functionp (funcall y)))
    (is (numberp x))))

(test receive
  (is (equal '(1 2 3)
             (receive list (values 1 2 3) list)))
  (is (equal '(1 2 3)
             (receive (one . more) (values 1 2 3)
               (cons one more))))
  (is (equal '(1 2 3)
             (receive (one two three) (values 1 2 3)
               (list one two three))))

  (is (null (receive () (values) nil)))
  (is (null (receive x (values) x)))

;;; See https://gitlab.com/embeddable-common-lisp/ecl/-/issues/672
  #-ecl
  (progn
    (signals error
      (eval* `(receive (one two &optional three) (values 1 2 3)
                (list one two three))))
    (signals error
      (eval* `(receive (one two) (values 1 2 3)
                (list one two))))
    (signals error
      (eval* `(receive (one two three four) (values 1 2 3)
                (list one two three four))))
    (signals error
      (eval* `(receive () (values 1))))
    (signals error
      (eval* `(receive (x) (values) x)))))

(test mvlet*
  (is (= 2 (let ((x 1)) x
             (mvlet* ((x 2)
                      (y x))
               x y))))
  (is (= 13 (let ((x 1)) x
              (mvlet* ((x y (floor 20 6))
                       (z a (values x 5)))
                (+ x y z a))))))

(test mvlet
  (is (= 1 (let ((x 1))
             (mvlet ((x 2)
                     (y x))
               x y))))
  (is (= 11 (let ((x 1))
              (mvlet ((x y (floor 20 6))
                      (z a (values x 5)))
                (+ x y z a))))))

(test mvlet-bare-symbol
  (is (null (mvlet (x) x)))
  (is (null (mvlet* (x) x)))
  (is (equal '(nil nil)
             (mvlet (x y) (list x y))))
  (is (equal '(nil nil)
             (mvlet* (x y) (list x y)))))

(test mvlet-no-init
  (is (null (mvlet ((x)) x)))
  (is (null (mvlet* ((x)) x)))
  (is (equal '(nil nil)
             (mvlet ((x) (y)) (list x y))))
  (is (equal '(nil nil)
             (mvlet* ((x) (y)) (list x y)))))

;; Since it exists, we incorporate the unit test harness from
;; <http://pobox.com/~oleg/ftp/Scheme/vland.scm>.
(test and-let*
  (macrolet ((expect (x y)
               `(is (equal (eval ,x) ,y)))
             (must-be-a-syntax-error (x)
               `(signals error (eval ,x))))
    ;; No claws
    (expect  '(and-let* () 1) 1)
    (expect  '(and-let* () 1 2) 2)
    #+(or) (expect  '(and-let* () ) t)
    ;; One claw, no body
    (expect '(let ((x nil)) (and-let* (x))) nil)
    (expect '(let ((x 1)) (and-let* (x))) 1)
    (expect '(let ((x 1)) (and-let* ( (x) ))) 1)
    (expect '(let ((x 1)) (and-let* ( ((+ x 1)) ))) 2)
    (expect '(and-let* ((x nil)) ) nil)
    (expect '(and-let* ((x 1)) ) 1)
    ;; two claws, no body
    (expect '(and-let* ( (nil) (x 1)) ) nil)
    (must-be-a-syntax-error '(and-let* (2 (x 1))))
    (expect '(and-let* ( (2) (x 1)) ) 1)
    (expect '(and-let* ( (x 1) (2)) ) 2)
    (expect '(and-let* ( (x 1) x) ) 1)
    (expect '(and-let* ( (x 1) (x)) ) 1)
    ;; two claws, body
    (expect '(let ((x nil)) (and-let* (x) x)) nil)
    (expect '(let ((x "")) (and-let* (x) x)) "")
    (expect '(let ((x "")) (and-let* (x)  )) "")
    (expect '(let ((x 1)) (and-let* (x) (+ x 1))) 2)
    (expect '(let ((x nil)) (and-let* (x) (+ x 1))) nil)
    (expect '(let ((x 1)) (and-let* (((plusp x))) (+ x 1))) 2)
    (expect '(let ((x 1)) (and-let* (((plusp x))) )) t)
    (expect '(let ((x 0)) (and-let* (((plusp x))) (+ x 1))) nil)
    (expect '(let ((x 1)) (and-let* (((plusp x)) (x (+ x 1))) (+ x 1)))  3)
    (expect
     '(let ((x 1)) (and-let* (((plusp x)) (x (+ x 1)) (x (+ x 1))) (+ x 1)))
     4)
    (expect '(let ((x 1)) (and-let* (x ((plusp x))) (+ x 1))) 2)
    (expect '(let ((x 1)) (and-let* ( ((progn x)) ((plusp x))) (+ x 1))) 2)
    (expect '(let ((x 0)) (and-let* (x ((plusp x))) (+ x 1))) nil)
    (expect '(let ((x nil)) (and-let* (x ((plusp x))) (+ x 1))) nil)
    (expect '(let ((x nil)) (and-let* ( ((progn x)) ((plusp x))) (+ x 1))) nil)
    (expect  '(let ((x 1)) (and-let* (x (y (- x 1)) ((plusp y))) (/ x y))) nil)
    (expect  '(let ((x 0)) (and-let* (x (y (- x 1)) ((plusp y))) (/ x y))) nil)
    (expect  '(let ((x nil)) (and-let* (x (y (- x 1)) ((plusp y))) (/ x y))) nil)
    (expect  '(let ((x 3)) (and-let* (x (y (- x 1)) ((plusp y))) (/ x y))) (/ 3 2))))

#+(or sbcl ccl)
(test and-let-unused ()
  (signals style-warning
    (eval `(and-let* ((x 1)
                      (y 2))
             x))))

(test if-not
  (is (= 2 (if-not t 1 2)))
  (is (= 2 (if-not "test" 1 2)))
  (is (= 1 (if-not nil 1 2)))
  (is (null (if-not t 1))))

;; tests adapted from Alexandria/tests
(declaim (notinline opaque))
(defun opaque (x)
  x)

(test if-not-let
  (is (eql (if-not-let (x (opaque :ok))
	     :bad
	     x)
	   :ok))
  (is (eql :ok
	   (if-not-let (x (opaque nil))
	     (and (not x) :ok)
	     :bad)))
  (is (= 3
	 (let ((x 1))
	   (if-not-let ((x 2)
			(y x))
	     :oops
	     (+ x y)))))
  (is (= 1
	 (if-not-let ((x 1)
		      (y nil))
           (and (not y) x)
	   :oops)))
  (is (if-not-let (x)
	(not x)
	:oops))
  (is (eql :type-error
	   (handler-case
	       (eval '(if-not-let x
		       :oops
		       :oops))
	     (type-error ()
	       :type-error)))))
