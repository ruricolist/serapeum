(in-package :serapeum.tests)

(def-suite control-flow :in serapeum)
(in-suite control-flow)

(test no
  (locally (declare (notinline no))
    (is-true (no nil))
    (is-false (no t))))

(test nor
  (is-true (nor))
  (is-true (nor nil))
  (is-false (not t))
  (is-true (nor nil nil))
  (is-false (nor nil t))
  (is-false (nor t nil))
  (is-false (nor t t)))

(test nand
  (is-false (nand))
  (is-false (nand t))
  (is-true (nand nil))
  (is-false (nand t t))
  (is-true (nand nil t))
  (is-true (nand t nil))
  (is-true (nand nil nil)))

(test typecase-of
  (is (= 1
         (eval '(typecase-of integer 0 ((integer 0) 1) ((integer * (0)) 2) (otherwise 3)))))
  (signals error
    (eval '(typecase-of integer 0 (integer 1))))
  (let (warned)
    (handler-bind ((warning #'(lambda (c) (setf warned t) (muffle-warning c))))
      (eval '(typecase-of integer 0 ((integer 0) 1) ((integer * -2) 2) (otherwise 3))))
    (is-true warned))
  ;;; types that cannot be tested for exhaustiveness
  (is (equal :b
             (eval '(typecase-of t 1
                     ((not (satisfies eval)) :a)
                     (otherwise :b)))))
  ;;; clause types must be subtypes of given type
  (let ((warn nil))
    (flet ((h (c) (setf warn t) (muffle-warning c)))
      (handler-bind ((warning #'h))
        (let ((val (eval '(typecase-of integer 1 (real :a) (otherwise :b)))))
          (is (equal :a val))))
      (is-true warn)
      (setf warn nil)
      (let ((val
              (handler-bind ((warning #'h))
                (eval '(typecase-of (satisfies eval) 1
                        (real :a)
                        (otherwise :b))))))
        (is (equal :a val))
        (is-true warn)))))

(deftype explodable-type ()
  '(or x (or y z) (member :x :y)))

(deftype explodable-member-type ()
  '(member :x :y :z))

(test explode-type
  (is (type= nil '(member)))
  ;; A member type with no arguments is impossible.
  (is (equal '(nil) (serapeum/control-flow::explode-type '(member) nil)))
  (is (set-equal
       (serapeum/control-flow::explode-type '(member :x :y :z) nil)
       '((eql :x) (eql :y) (eql :z))
       :test #'equal))
  (is (set-equal
       (serapeum/control-flow::explode-type 'explodable-member-type nil)
       '((eql :x) (eql :y) (eql :z))
       :test #'equal))
  (is (set-equal
       (serapeum/control-flow::explode-type '(or x (or y z) (member :x :y)) nil)
       '(x y z (eql :x) (eql :y))
       :test #'equal))
  (is (set-equal
       (serapeum/control-flow::explode-type 'explodable-type nil)
       '(x y z (eql :x) (eql :y))
       :test #'equal)))

(test etypecase-of
  (is (= 2
         (eval '(etypecase-of integer 0 ((integer 1) 1) ((integer -1 0) 2)))))
  (signals error
    (eval '(etypecase-of integer 0 ((integer 1) 1) ((integer * -1) 2)))))

(test case-of
  (is (eql :b
           (eval '(case-of (integer 0 6) 1
                   (0 :a) (1 :b) ((2) :c) ((3 5) :d) ((4 6) :e)
                   (otherwise nil)))))
  (signals error
    (eval '(case-of (integer 0 2) 1
            (1 :a) (2 :b) (0 :c)))))


(test case-using
  (is (eql 'two
           (case-using #'= (+ 1.0 1.0)
             ((1) 'one)
             ((2) 'two)
             (t 'more))))

  (is (eql 2
           (case-using #'string= "bar"
             (("foo") 1)
             (("bar") 2))))

  (is-true
   (occurs 'string-case
           (macroexpand-1
            '(case-using #'string= "bar"
              (("foo") 1)
              (("bar") 2)))))
  (is-true
   (occurs 'string-case
           (macroexpand-1
            '(case-using 'string= "bar"
              (("foo") 1)
              (("bar") 2)))))

  (is-true
   (case-using #'eql 'x
     (x t)
     (t nil)))

  (is-true
   (occurs 'case
           (macroexpand-1
            '(case-using #'eql 'x
              (x t)
              (t nil)))))

  (is-true
   (case-using #'eql nil
     ((nil) t)
     (t nil))))

(test ecase-using
  (signals error
    (ecase-using #'char= #\y
      (#\x t)))
  (signals error
    (ecase-using #'char #\y))
  (is (eql t
           (ecase-using #'char= #\x
             (#\x t)))))

(test cond-every
  (is (null (cond-every)))
  (is (eql (cond-every (t 1) (otherwise 2)) 1))
  (is (eql (cond-every (otherwise 1)) 1))
  (is (eql (cond-every (t 1) (nil 2)) 1))
  (is (eql (let ((x 1)) (cond-every (x) (nil 2))) 1))
  ;; Tests are evaluated first.
  (is (eql 3
           (let ((x 1))
             (cond-every
               ((< x 2) (incf x))
               ((= x 1) (incf x)))
             x))))

(test bcond
  (is (= 2
         (bcond ((assoc 'b '((a 1) (b 2))) => #'cadr)
           (t nil))))
  (is (= 2
         (bcond ((assoc 'b '((a 1) (b 2))) => cons
                 (cadr cons))
           (t nil))))
  (is (= 1
         (bcond
           ((assoc 'c '((a 1))) "=>" #'not)
           (t 1))))
  (is-false (bcond))
  (signals error
    (eval '(bcond (1 =>)))))

(test case-let
  (is (eql 2
           (case-let (x 1)
             (0 3) (1 (1+ x)) (t 5))))
  (is (eql 5
           (case-let (x 16)
             (0 3) (1 (1+ x)) (t 5)))))

(test ccase-let
  (is (eql 2
           (ccase-let (x 1)
             (0 3) (1 (1+ x)) (t 5))))
  (signals type-error
    (ccase-let (x 17)
      (0 x) (1 (1+ x)))))

(test ecase-let
  (is (eql 2
           (ecase-let (x 1)
             (0 3) (1 (1+ x)))))
  (signals error
    (ecase-let (x 17)
      (0 x) (1 (1+ x)))))

(test typecase-let
  (is (eql 2
	   (typecase-let (x 2)
	     (string 20)
	     (integer x))))
  (is (string= "test-here"
	       (typecase-let (y "test")
		 (integer "not")
		 (string (concatenate 'string y "-here"))))))

(test ctypecase-let
  (is (eql 'asdf
	   (ctypecase-let (x 'asdf)
	     (string 20)
	     (integer :sdf)
	     (symbol x))))
  (signals type-error
    (ctypecase-let (y 'test-symbol)
      (integer "not")
      (string (concatenate 'string y "-here")))))

(test etypecase-let
  (is (eql 2
	   (etypecase-let (x 'asdf)
	     (string 20)
	     (integer x)
	     (symbol 2))))
  (signals type-error
    (etypecase-let (y 'test-symbol)
      (integer "not")
      (string (concatenate 'string y "-here")))))

(test comment
  (is-false (comment "This is a comment")))

(test example
  (is-false (example "This is a comment")))

(defvar *e*)

(test ensure
  (let ((x 1))
    (is (= 1 (ensure x 0))))
  (setf *e* 1)
  (is (= 1 *e*))
  (is (= 1 (ensure *e* 0)))
  (makunbound '*e*)
  (is (= 2 (ensure *e* 2)))
  (makunbound '*e*)
  (is (= 1 (incf (ensure *e* 0))))
  (is (= 1 *e*)))

(test ensure2
  (let ((table (make-hash-table)))
    (is (eql 1 (ensure2 (gethash :a table) 1)))
    (is (eql 1 (gethash :a table)))
    (is (eql 5 (incf (ensure2 (gethash :b table) 4))))
    (is (eql 5 (gethash :b table)))))

(test nest
  (nest (let ((x 1)))
        (let ((y (+ x 3))))
        (is (equal '(1 4) (list x y)))))

(test nest-pattern
  (is-true
   (match '(1 2 3 4)
     ((nest (cons 1)
            (cons 2)
            (cons 3)
            (list 4))
      t))))

(test arrow-pattern
  (is-true
   (match '(1 2 3 4)
     ((~>> '(4)
           (cons 3)
           (cons 2)
           (cons 1))
      t)))
  (is-true
   (match '(t)
     ((~> x list)
      x))))

(test select
  (is-true
   (select Pi
     (pi t)
     (t nil)))
  (is-true
   (select 1
     (((- 2 1)) t)))
  (is-true
   (let ((x 1))
     (select 1
       (x t)))))

(test sort-values
  (is (null (sort-values #'>)))
  ;; Should this be an error?
  (is (eql t (sort-values #'> t)))
  (is (equal '(1 2)
             (multiple-value-list
              (sort-values #'< 2 1))))
  (is (equal '(1 2 3 4 5 6 7 8 9)
             (multiple-value-list
              (sort-values #'< 5 8 1 4 2 9 7 3 6)))))

(test convert-string-case-to-case
  (is (= 4)
      (string-case "x"
        ("a" 1)
        ("b" 2)
        ("c" 3)
        ("x" 4)))
  ;; Distinguish case of "all strings are length 1"
  ;; and otherwise.
  (is (= 2)
      (string-case "bb"
        ("a" 1)
        ("bb" 2)
        ("c" 3)
        ("x" 4)))
  ;; Case with more than one string
  (is (= 2)
      (string-case "a"
        (("b" "c") 1)
        ("a" 2)
        ("d" 3)))
  ;; Non-string as clause constant
  (signals error
    (eval '(string-case "a" (#\x 0) (t 1))))
  ;; Default is present
  (is (= 2)
      (string-case "b" ("a" 1) ("b" 2) ("c" 3) (t 4)))
  (is (= 4)
      (string-case "z" ("a" 1) ("b" 2) ("c" 3) (t 4))))

(test string-ecase-error
 (signals error
    (string-ecase "x" ("a" 1) ("b" 2))))

(test convert-string-case-to-case-with-default-clause
  (is (equal "another"
             (string-case "test" ("t" t) (t "another")))))

(test eif
  (let ((*error-output* (make-broadcast-stream)))
    (is (not (null (nth-value 1 (compile nil '(lambda (x y) (eif x y)))))))))

(test eif-let
  (is (eql :a (eif-let ((x :a)) x :b)))
  (is (eql :b (eif-let ((x nil)) x :b)))
  (let ((*error-output* (make-broadcast-stream)))
    (is (not (null (nth-value 1 (compile nil '(lambda (a b) (eif-let ((x a)) b)))))))))

(test econd
  (declare (optimize (speed 0) (safety 3)))
  (let ((n (random 10)))
    (signals econd-failure
      (econd
       ((> n 10) (assert nil))))))

(test cond-let
  (is (eql (cond-let x (10 x) (t 0)) 10))
  (is (eql (cond-let x (nil x) (:a x)) :a))
  (is (eql (cond-let x (nil x) (t x)) nil))
  (is (eql (cond-let x ((1+ 0))) 1)))

(test cond-let/declare
  (flet ((aux (x)
           (cond-let y
             ((and (minusp x) (- x 1))
              (declare (integer y))
              y)
             ((and (plusp x) (+ x 1s0))
              (declare (single-float y))
              y))))
    (declare (notinline aux))
    (is (= -2 (aux -1)))
    (is (= 2s0 (aux 1)))))

(test econd-let
  (is (eql (econd-let x (10 x) (t 0)) 10))
  (is (eql (econd-let x (nil x) (:a x)) :a))
  (is (eql (econd-let x (nil x) (t x)) nil))
  (is (eql (econd-let x ((1+ 0))) 1))
  (signals error
    (econd-let x (nil :a))))

(test nix
  (let ((x 1) (y 2) (z 3))
    (is (equal '(1 2 3)
               (multiple-value-list
                (nix x y z))))
    (is (null x))
    (is (null y))
    (is (null z))))

(test without-recursion
  (labels ((f ()
             (without-recursion () (1+ (g))))
           (g () 2))
    (is (= 3 (f))))
  (labels ((f (x)
             (without-recursion () (1+ (g (1- x)))))
           (g (y) (if (< y 0) 0 (1+ (f (1- y))))))
    (signals recursion-forbidden
      (f 10))))
