(in-package #:serapeum.tests)

(defun run-tests ()
  (5am:run! 'serapeum))

(defun run-tests/quiet ()
  (handler-bind ((warning #'muffle-warning))
    (run-tests)))

(defun debug-test (test)
  (let ((5am:*debug-on-error* t)
        (5am:*debug-on-failure* t))
    (run! test)))

(defun a-fixnum ()
  (lambda ()
    (random-in-range most-negative-fixnum most-positive-fixnum)))

(defun an-iota (n)
  (lambda ()
    (iota n)))

(defun a-list-of (len fn)
  (lambda ()
    (map-into (make-list len) fn)))

(defun eval* (form)
  "Variant of eval forcing macroexpansion."
  (funcall (compile nil (eval `(lambda () ,form)))))

(def-suite serapeum)
(in-suite serapeum)

(defmacro suite (name &body tests)
  `(progn
     (def-suite ,name :in serapeum)
     (in-suite ,name)
     ,@tests))

(suite macro-tools)

(suite types

  (test tuple
    (is (typep '() '(tuple)))
    (is (not (typep '() '(tuple null))))
    (is (typep '(1) '(tuple integer)))
    (is (not (typep '(1) '(tuple symbol))))
    (is (typep '(1 :x #\c) '(tuple integer symbol character)))))

(suite definitions
  
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

    (is (null
         (let ((y 2))
           (local
             ;; Check that we don't hoist the constant binding of X.
             (def ret nil)
             (setq ret x)
             (def x y)
             (def x 1)
             ret))))

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

  (test defstruct-read-only
    (is (equal '(defstruct (foo (:copier nil))
                 (bar (required-argument 'bar) :read-only t))
               (macroexpand-1
                '(defstruct-read-only foo
                  bar))))

    (is (equal '(defstruct (foo (:copier nil))
                 "A struct."
                 (bar (required-argument 'bar) :read-only t))
               (macroexpand-1
                '(defstruct-read-only foo
                  "A struct."
                  bar))))

    (is (equal '(defstruct (foo (:copier nil))
                 (bar nil :read-only t))
               (handler-bind ((warning #'muffle-warning))
                 (macroexpand-1
                  '(defstruct-read-only foo
                    (bar nil :read-only nil))))))

    (is (equal '(defstruct (foo (:copier nil))
                 "A struct."
                 (bar nil :read-only t))
               (handler-bind ((warning #'muffle-warning))
                 (macroexpand-1
                  '(defstruct-read-only foo
                    "A struct."
                    (bar nil :read-only nil))))))

    (signals error
      (macroexpand-1 '(defstruct-read-only (foo (:include bar)))))

    (signals error
      (macroexpand-1 '(defstruct-read-only (foo (:copier copy-foo)))))))

(suite binding

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
      #+ () (expect  '(and-let* () ) t)
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
      (expect  '(let ((x 3)) (and-let* (x (y (- x 1)) ((plusp y))) (/ x y))) (/ 3 2)))))

(suite control-flow

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
     (case-using #'eql 'x
       (x t)
       (t nil))))

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
                 (t nil)))))

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
    (for-all ((list (a-list-of (random 100)
                               (lambda () (random 100)))))
      (is
       (equal (sort (copy-list list) #'>)
              (multiple-value-list
               (eval `(sort-values #'> ,@list)))))))

  (test convert-string-case-to-case
    (is (= 4)
        (string-case "x"
          ("a" 1)
          ("b" 2)
          ("c" 3)
          ("x" 4)))))

(suite threads)

(suite iteration

  (test nlet-scope
    "Make sure variables in nlet can be closed over."
    (is (equal '(3 2 1)
               (mapcar 'funcall
                       (nlet rec ((i 3)
                                  (acc nil))
                         (if (= i 0)
                             (nreverse acc)
                             (rec (1- i) (cons (lambda () i) acc))))))))
  (test collecting
    (is (equal '(0 1 2 3 4)
               (collecting
                 (dotimes (i 5)
                   (collect i))))))

  (test with-collectors
    (is (equal '((1) (2) (3))
               (multiple-value-list
                (with-collectors (x y z)
                  (x 1) (y 2) (z 3)))))))

(suite conditions)

(suite op

  (test op
    ;; Constantly.
    (is (= (funcall (op 1)) 1))
    ;; Identity.
    (is (= (funcall (op _) 1) 1))
    ;; Positional.
    (is (= (funcall (op (+ 1 _)) 1) 2))
    ;; Backward reference.
    (is (= (funcall (op (+ _ _1)) 2) 4))
    ;; Rest.
    (is (equal (apply (op (list 1 _*)) '(2 3)) '(1 2 3)))
    ;; Positional and rest.
    (is (equal (apply (op (list _ _*)) 1 '(2 3)) '(1 2 3)))
    ;; Flip
    (is (eql 4 (find '(4) (iota 10) :test (op (member _2 _1)))))
    ;; nth-arg
    (is (equal '(4 5 6) (mapcar (op _2) '(1 2 3) '(4 5 6))))
    ;; Sparse argument lists.
    (is (= 9 (apply (op (+ _1 _3 _5)) '(1 2 3 4 5))))
    ;; Backquotes.
    (is (equal '((:x 1) (:x 2) (:x 3)) (mapcar (op `(:x ,_)) '(1 2 3))))))

(suite functions

  (test juxt
    (is (equal (funcall (juxt #'remove-if-not #'remove-if)
                        #'evenp
                        '(1 2 4 3 5 6))
               '((2 4 6) (1 3 5))))

    (is (equal (funcall (juxt #'+ #'max #'min) 2 3 5 1 6 4)
               '(21 6 1))))

  (test dynamic-closure
    (let ((fn (lambda ()
                (write-string "Hello")
                (get-output-stream-string *standard-output*))))
      (is (equal "Hello"
                 (funcall (let ((*standard-output* (make-string-output-stream)))
                            (dynamic-closure '(*standard-output*) fn)))))
      (is (equal "Hello"
                 (funcall (let ((*standard-output* (make-string-output-stream))
                                (symbols '(*standard-output*)))
                            (dynamic-closure symbols fn)))))))

  (test dynamic-closure/local-specials
    (funcall
     (let ((x 1))
       (declare (special x))
       (dynamic-closure '(x) (lambda () (symbol-value 'x)))))))

(suite trees

  (test leaf-map
    (is (equal (leaf-map (compose #'round #'sqrt) '(((4 1) 25) (9 100) 64))
               '(((2 1) 5) (3 10) 8))))

  (test map-tree
    (is (equal (map-tree (lambda (subtree)
                           (if (and (consp subtree)
                                    (eql (car subtree) 'skip-me))
                               (throw 'skip 'skipped)
                               subtree))
                         '((a (b) (c (skip-me d (e f)))))
                         'skip)
               '((a (b) (c skipped)))))))

(suite hash-tables
  (test frequencies
    (let ((ns (loop repeat 100 collect (random 10))))

      (let ((freqs (frequencies ns)))
        (loop for i from 0 below 10 do
          (is (= (gethash i freqs) (count i ns)))))
      
      ;; With :test argument.
      (let* ((xs (mapcar #'princ-to-string ns))
             (freqs (frequencies xs :test 'equal)))
        (loop for i from 0 below 10
              for string = (princ-to-string i)
              do (is (= (gethash string freqs)
                        (count string xs :test 'equal)))))
      
      ;; With :key argument.
      (let ((freqs (frequencies ns :key (lambda (x) (* x 2)))))
        (loop for i from 0 below 10 do
          (is (= (gethash (* i 2) freqs) (count i ns)))))))

  (test hash-table-function
    (let ((ht (dict :x 1)))
      (is (= 1 (funcall (hash-table-function ht) :x)))
      (fbind ((ht (hash-table-function ht)))
        (ht :y 2)
        (is (= 2 (ht :y))))))

  (test hash-table-function/read-only
    (fbind ((ht (hash-table-function (dict :x 1) :read-only t)))
      (signals error
        (ht :y 2))
      (signals error
        (ht :x 3))
      (signals error
        (ht :x 1))))

  (test hash-table-function/strict
    (fbind ((ht (hash-table-function (dict :x 1) :strict t)))
      (is (= 1 (ht :x)))
      (signals error
        (ht :y))
      (signals error
        (ht :y 2))))

  (test hash-table-function/key-type
    (fbind ((ht (hash-table-function (dict :x 1) :key-type 'keyword)))
      (is (= 1 (ht :x)))
      (signals type-error
        (ht 'x))
      (signals type-error
        (ht 'y 2))))

  (test hash-table-function/value-type
    (fbind ((ht (hash-table-function (dict :x 1) :value-type 'integer)))
      (is (= 1 (ht :x)))
      (signals type-error
        (ht :y 2.0)))))

(suite files)

(suite symbols)

(suite arrays

  (test array-index-row-major
    (is-true
     (let ((a (make-array '(7 4 9 5)))
           (subscripts '(3 2 8 1)))
       (equal subscripts
              (array-index-row-major
               a
               (apply #'array-row-major-index a subscripts)))))))

(suite queue)

(suite box)

(suite vectors

  (test vect
    (is (adjustable-array-p (vect)))
    (is (fill-pointer (vect)))
    (is (equalp (vect 1 2 3) #(1 2 3)))))

(suite numbers

  (test parse-float
    ;; Clinger 1990.
    (is-true
     (= (parse-float "1.448997445238699" :type 'double-float)
        1.4489974452386990d0)))

  (test unbits
    (is-true
     (let ((n (random most-positive-fixnum)))
       (= n (unbits (bits n))))))

  (test random-in-range
    (is
     (floatp
      (random-in-range most-negative-double-float most-positive-double-float)))

    (for-all ((n (lambda () (random-in-range -20 -5))))
      (is (<= -20 n))
      (is (> -5 n)))

    (for-all ((n (lambda () (random-in-range -100 1))))
      (is (<= -100 n))
      (is (> 1 n)))

    (signals error (random-in-range 1 1))))

(suite octets

  #+ () (test unoctets
          (for-all ((n (a-fixnum)))
            (is (= (unoctets (octets n)) n)))))

(suite time

  (test interval
    (is (= 31626000 (interval :years 1 :days 1 :hours 1)))))

(suite clos)

(suite hooks)

(suite fbind

  (test fbind
    (is (eql 1 (let ((fn (lambda (x) (1+ x))))
                 (fbind fn
                   (fn 0)))))
    (is (eql 1 (fbind ((fn (lambda (x) (1+ x))))
                 (fn 0))))
    (fbind ((string-or-null (rcurry #'typep '(or string null))))
      (is (string-or-null "f")))
    (fbind ((string-or-null (disjoin #'stringp #'null)))
      (is (string-or-null "foo"))
      (is (not (string-or-null 'foo)))
      (is (not (string-or-null t))))
    (fbind ((singleton-string (conjoin #'stringp #'single)))
      (is (singleton-string "f"))
      (is (not (singleton-string '(#\f))))))

  (test fbindrec
    (let ((fold-case (plusp (random 2))))
      (fbindrec ((char-test
                  (if fold-case
                      #'char-equal
                      #'char=)))
        (is
         (if fold-case
             (every #'char-test "foo" "FOO")
             (not (every #'char-test "foo" "FOO"))))))

    #+ () (signals error
            (fbindrec ((make-adder (lambda (x)
                                     (lambda (y)
                                       (+ y x))))
                       (add1 (make-adder 1)))
              (add1 1))))

  (test fbindrec*
    (is
     (equal (list t nil)
            (fbindrec* ((vowelp (lambda (c) (find c "aeiou")))
                        (consonantp (complement #'vowelp)))
              (list (consonantp #\j) (consonantp #\a)))))

    (let ((fold-case (plusp (random 2))))
      (fbindrec* ((fold-case?
                   (lambda () fold-case))
                  (char-test
                   (if (fold-case?)
                       #'char-equal
                       #'char=)))
        (is
         (if (fold-case?)
             (every #'char-test "foo" "FOO")
             (not (every #'char-test "foo" "FOO"))))))

    #+ () (signals error
            (fbindrec* ((a (lambda () #'c))
                        (b (a))
                        (c (constantly 7)))
              (b)))))

(suite lists

  (test memq
    (let* ((list1 (list 'x 'y nil 'z))
           (list2 (delq nil list1)))
      (is (not (memq nil list2)))
      (is (eq list1 list2))))

  (test mapply
    (is (equal
         ;; Sans compiler macro.
         (locally (declare (optimize (speed 0) (safety 3) (debug 3)))
           (mapply #'cons '((x 1) (y 2))))
         '((x . 1) (y . 2))))
    (is (equal (mapply #'list '((a 1) (b 2)) '((c 3) (d 4)))
               '((a 1 c 3) (b 2 d 4))))))

(suite strings

  (test with-string
    (flet ((test* (designator)
             (with-string (s designator)
               (write-string "string" s))))
      (is (equal "string" (test* nil)))
      (is (equal "string"
                 (with-output-to-string (*standard-output*)
                   (test* t))))
      (is (equal "string"
                 (with-output-to-string (str)
                   (test* str))))
      (is (equal "the string"
                 (let ((out (make-array 4
                                        :adjustable t
                                        :fill-pointer 4
                                        :element-type 'character
                                        :initial-contents "the ")))
                   (with-output-to-string (str out)
                     (test* str))
                   out)))))

  (test word-wrap
    (is (equal
         (word-wrap "There is no way on god’s green earth I can perform that function, Will Robinson."
                    :column 40)
         "There is no way on god’s green earth I 
can perform that function, Will 
Robinson.")))

  (test collapse-whitespace
    (is (equal (collapse-whitespace "") ""))
    (is (equal (collapse-whitespace " ") " "))
    (is (equal (collapse-whitespace "x") "x"))
    (is (equal (collapse-whitespace "  ") " "))
    (is (equal (collapse-whitespace "  one   two    three  ") " one two three ")))

  (test mapconcat
    (is (equal "A B C" (mapconcat #'string-upcase #("a" "b" "c") " ")))
    (is (equal "A B C" (mapconcat #'string-upcase '("a" "b" "c") " "))))

  (test string-upcase-initials
    (is (equal (string-upcase-initials "") ""))
    (is (equal (string-upcase-initials "a") "A"))
    (is (equal (string-upcase-initials "an ACRONYM")
               "An ACRONYM")))

  (test same-case-p
    (is (not (same-case-p "")))
    (is (same-case-p "f"))
    (is (not (same-case-p ".")))
    (is (same-case-p "foo"))
    (is (same-case-p "foo-bar"))
    (is (not (same-case-p "Foo")))
    (is (not (same-case-p "-Foo"))))

  (test string-invert-case
    (is (equal "ZEBRA" (string-invert-case "zebra")))
    (is (equal "zebra" (string-invert-case "ZEBRA"))))

  (test escape
    (let ((in (concatenate 'string "foo" '(#\Tab) "bar" '(#\Tab) "baz"))
          (out "foo\\tbar\\tbaz")
          (table (lambda (c)
                   (case c
                     (#\Tab "\\t")))))
      (is (equal out (escape in table)))))

  (test string^=
    (is (string^= "foo" "foobar"))
    (is (string^= "foo" "foo"))
    (is (not (string^= "foo" "fo")))
    (is (string^= "a long string" "string" :start1 (length "a long ")))
    (is (not (string^= "a" "")))
    (is (string^= "a" "abe"))
    (is (string^= "a" '|abc|))
    (is (not (string^= "a" "be"))))

  (test string$=
    (is (string$= "bar" "foobar"))
    (is (string$= "bar" "bar"))
    (is (not (string$= "bar" "ar")))
    (is (not (string$= "1x" "2x")))
    (is (string$= "/" "foo/"))
    (is (not (string$= "/" "")))
    (is (string$= "c" '|abc|))
    (is (string$= "/" "/")))

  (test string*=
    (is (search nil "any string"))
    (is (not (string*= nil "any string")))
    (is (string*= nil "NIL"))
    (is (not (string*= "a" "")))
    (is (string*= "a" "a"))
    (is (string*= "a" '|abc|))
    (is (string*= "a" "abe")))

  (test string~=
    (is (string~= "foo" "foo bar"))
    (is (string~= "foo" "bar foo"))
    (is (string~= "foo" "bar foo baz"))
    (is (not (string~= "foo" "barfoo baz")))
    (is (not (string~= "foo" "foobar baz")))
    (is (string~= "foo-bar" "foo-bar")))

  (test string-replace-all
    (is (equal (string-replace-all "foo" "foobar" "baz") "bazbar"))
    (is-true
     (let ((s "foo, bar"))
       (eq s (string-replace-all ":" s ""))))
    (is (equal "The new way"
               (string-replace-all "old" "The old way" "new")))
    (is (equal "The new old way"
               (string-replace-all "old" "The old old way" "new" :start 3 :end 7)))
    (is (equal "quux quux quux"
               (string-replace-all "foo" "foo foo foo" "quux")))
    (is (equal "quux quux foo"
               (string-replace-all "foo" "foo foo foo" "quux" :count 2))))

  (test chomp
    ;; Remove the longest sequence first.
    (is (equal "abc"
               (chomp "abcxyz" (list "z" "yz" "xyz")))))

  (test string-count
    (is (zerop (string-count "foo" "")))
    (is (zerop (string-count "foo" "fofofo")))
    (is (= 1 (string-count "foo" "fofoofo")))))

(suite sequences

  (test filter-with-count
    (is (equal '(0 2 4 6 8) (filter #'evenp (iota 100) :count 5)))
    (is (equalp #(0 2 4 6 8) (filter #'evenp (coerce (iota 100) 'vector) :count 5)))
    (is (equal '(90 92 94 96 98)
               (filter #'evenp (iota 100) :count 5 :from-end t))))

  (test keep-with-count
    (is (equal '((a 1) (a 2))
               (keep 'a '((a 1) (b) (c) (a 2) (a 3) (b) (c) (a 4) (a 5))
                     :count 2 :key #'car)))
    (is (equal '((a 4) (a 5))
               (keep 'a '((a 1) (b) (c) (a 2) (a 3) (b) (c) (a 4) (a 5))
                     :count 2 :key #'car :from-end t))))

  (test partitions
    (is (equal (partitions (list #'oddp #'evenp) '(0 1 2 3 4 5 6 7 8 9))
               '((1 3 5 7 9) (0 2 4 6 8)))))

  (test assort
    (is (equal (assort (iota 10)
                       :key (lambda (x)
                              (mod x 3)))
               '((0 3 6 9) (1 4 7) (2 5 8))))

    (is (equal (assort "How Now Brown Cow" :key #'upper-case-p)
               '("HNBC" "ow ow rown ow"))))

  (test runs
    (is (equal '((1 2) (3 4 5 6 11 12 13))
               (runs '(1 2 3 4 5 6 11 12 13) :key (rcurry #'< 3)))))

  (test batches
    (is (equal '((a b) (c d) (e)) (batches '(a b c d e) 2)))
    (is (equal '("ab" "cd" "e") (batches "abcde" 2)))
    (is (equal '("a") (batches "abc" 2 :end 1)))
    (is (equal '((a)) (batches '(a b c) 2 :end 1))))

  (test gcp
    (is (equal (gcp '("miss" "molly")) "m")))

  (test gcs
    (is (equal (gcs '("how" "now")) "ow")))

  (test length<
    (is (length< #() 1))
    (is (length< '(1) 2))
    (is (not (length< '(1 2) 2)))
    (is (not (length< '(1 2 3) 2))))

  (test length>
    (is (not (length> '(1) 2)))
    (is (not (length> '(1 2) 2)))
    (is (length> '(1 2 3) 2))
    (is (not (length> nil 0))))

  (test slice
    (is (equal "in" (slice "string" -3 -1)))
    (is (equal "foo" (slice "foo" -0)))
    (is (equal "r" (slice "bar" -1))))

  (test ordering
    (for-all ((list (an-iota 1000)))
      (let ((list (shuffle list)))
        (is (equal list
                   (sort (shuffle (copy-list list))
                         (ordering list)))))))

  (test bestn
    (for-all ((list (a-list-of 1000 (lambda () (random 1000)))))
      (is
       (equal (firstn 20 (sort (copy-list list) #'>))
              (bestn 20 list #'>))))

    (for-all ((list (a-list-of 1000 (lambda () (random 1000)))))
      (is
       (equal (firstn 20 (sort (copy-list list) #'string> :key #'princ-to-string))
              (bestn 20 list #'string> :key #'princ-to-string)))))

  (test extrema
    (is (equal (multiple-value-list (extrema '(1 2 3 4 5) #'<)) '(1 5))))


  (test halves
    (is (equal (halves '(x)) '(x)))
    (is (equal (multiple-value-list (halves '(x y))) '((x) (y))))
    (is (equal (multiple-value-list (halves '(x y z))) '((x y) (z)))))

  (test deltas
    (is (equal '(4 5 -14 6 1) (deltas '(4 9 -5 1 2))))
    (is (equal '(4 5 -14 6 1) (deltas #(4 9 -5 1 2)))))

  (test intersperse
    (is (null (intersperse 'x '())))
    (is (equal (intersperse 'x '(z)) '(z)))
    (is (equal (intersperse 'y '(x z)) '(x y z)))
    (is (= (length (intersperse #\x "")) 0))
    (is (equal (intersperse #\x "z") "z"))
    (is (equal (intersperse #\y "xz") "xyz")))

  (test mvfold
    (is (equal '(((0 1) 2) 3) (mvfold (op (list _ _)) '(1 2 3) 0)))
    (is (equal '(1 (2 (3 0))) (mvfoldr (op (list _ _)) '(1 2 3) 0)))

    (is (equal (multiple-value-list
                (mvfold (lambda (min max item)
                          (values (min item min)
                                  (max item max)))
                        (iota 10) 0 0))
               '(0 9)))
    (is (equal (multiple-value-list
                (mvfold (lambda (item min max)
                          (values (min item min)
                                  (max item max)))
                        (iota 10) 0 0))
               '(0 9)))))
