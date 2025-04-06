(in-package :serapeum.tests)

(def-suite functions :in serapeum)
(in-suite functions)

(test eqs
  (is-true (funcall (eqs :a) :a))
  (is-false (funcall (eqs :a) :b))
  (locally (declare (notinline eqs))
    (is-true (funcall (eqs :a) :a))
    (is-false (funcall (eqs :a) :b))))

(test eqls
  (is-true (funcall (eqls 1) 1))
  (is-false (funcall (eqls 1) 2))
  (locally (declare (notinline eqls))
    (is-true (funcall (eqls 1) 1))
    (is-false (funcall (eqls 1) 2))))

(test equals
  (is-true (funcall (equals (list 1)) (list 1)))
  (is-false (funcall (equals (list 1)) (list 2)))
  (locally (declare (notinline equals))
    (is-true (funcall (equals (list 1)) (list 1)))
    (is-false (funcall (equals (list 1)) (list 2)))))

(test partial
  (is (equal '(:a :b :c :d) (funcall (partial #'list) :a :b :c :d)))
  (is (equal '(:a :b :c :d) (funcall (partial #'list :a) :b :c :d)))
  (is (equal '(:a :b :c :d) (funcall (partial #'list :a :b) :c :d)))
  (is (equal '(:a :b :c :d) (funcall (partial #'list :a :b :c) :d)))
  (is (equal '(:a :b :c :d) (funcall (partial #'list :a :b :c :d))))
  (locally (declare (notinline partial))
    (is (equal '(:a :b :c :d) (funcall (partial #'list) :a :b :c :d)))
    (is (equal '(:a :b :c :d) (funcall (partial #'list :a) :b :c :d)))
    (is (equal '(:a :b :c :d) (funcall (partial #'list :a :b) :c :d)))
    (is (equal '(:a :b :c :d) (funcall (partial #'list :a :b :c) :d)))
    (is (equal '(:a :b :c :d) (funcall (partial #'list :a :b :c :d))))))

(test juxt
  (is (equal (funcall (juxt #'remove-if-not #'remove-if)
                      #'evenp
                      '(1 2 4 3 5 6))
             '((2 4 6) (1 3 5))))

  (is (equal (funcall (juxt #'+ #'max #'min) 2 3 5 1 6 4)
             '(21 6 1))))

(test juxt-notinline
  (locally (declare (notinline juxt))
    (is (equal (funcall (juxt #'remove-if-not #'remove-if)
                        #'evenp
                        '(1 2 4 3 5 6))
               '((2 4 6) (1 3 5))))

    (is (equal (funcall (juxt #'+ #'max #'min) 2 3 5 1 6 4)
               '(21 6 1)))))

(test dynamic-closure
  (is (= 17 (funcall (dynamic-closure () (constantly 17)))))
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
     (dynamic-closure '(x) (lambda () (symbol-value 'x))))))

(test hook
  (is (null (funcall (hook #'= #'floor) 2.1)))
  (is (funcall (hook #'= #'floor) 3)))

(test fork
  (let ((sample (range 100)))
    (is (= (mean sample)
           (funcall (fork #'/
                          (partial #'reduce #'+)
                          #'length)
                    sample)))))

(test hook2
  (is (= 3.25
         (funcall (hook2 #'+ (op (/ _ 60)))
                  3 15))))

(test fork2
  (is (equal '(11 9)
             (funcall (fork2 #'list #'+ #'-)
                      10 1))))

(test trampoline
  ;; Example from
  ;; <http://jakemccrary.com/blog/2010/12/06/trampolining-through-mutual-recursion/>.
  (local
    (defun my-even? (n)
      (flet ((e? (n)
               (or (zerop n)
                   (op (1- (abs n)))))
             (o? (n)
               (if (zerop n)
                   nil
                   (op (1- (abs n))))))
        (trampoline #'e? n)))

    (defun my-odd? (n)
      (not (my-even? n)))

    (is-true (my-even? 1000000))
    (is-false (my-odd? 1000000))))

(test once
  (let* ((s "string")
         (fn (once (lambda () (copy-seq s))))
         (copy (funcall fn)))
    (is (string copy))
    (is (not (eq copy s)))
    (is (eq copy (funcall fn)))))

(test once/effects
  (let ((fn (once (lambda () (princ "Hello")))))
    (is (equal "Hello"
               (with-output-to-string (*standard-output*)
                 (funcall fn))))
    (is (emptyp
         (with-output-to-string (*standard-output*)
           (funcall fn))))))

(test once/mv
  (let ((fn (once (lambda () (values 1 2 3)))))
    (funcall fn)
    (is (equal '(1 2 3)
               (multiple-value-list (funcall fn))))))

(test fnil
  (is-false (funcall (fnil (constantly nil))))
  (fbind* ((say-hello
            (lambda (name)
              (string+ "Hello " name)))
           (say-hello-with-defaults
            (fnil #'say-hello "World")))
    (is (equal "Hello Sir" (say-hello-with-defaults "Sir")))
    (is (equal "Hello World" (say-hello-with-defaults nil))))

  (fbind* ((say-hello
            (lambda (first other)
              (string+ "Hello " first " and " other)))
           (say-hello-with-defaults (fnil #'say-hello "World" "People")))
    (is (equal "Hello World and People"
               (say-hello-with-defaults nil nil)))
    (is (equal "Hello Sir and People"
               (say-hello-with-defaults "Sir" nil)))
    (is (equal "Hello World and Ma'am"
               (say-hello-with-defaults nil "Ma'am")))
    (is (equal "Hello Sir and Ma'am"
               (say-hello-with-defaults "Sir" "Ma'am"))))

  (is (= (funcall (fnil #'1+ 0) nil) 1))

  (is (every #'numberp
             (mapcar (fnil #'1+ 0)
                     '(1 2 nil 4 6))))
  (locally (declare (notinline fnil))
    (is (every #'numberp
               (mapcar (fnil #'1+ 0)
                       '(1 2 nil 4 6)))))
  (locally (declare (notinline fnil))
    (is (equal '(1 2)
               (funcall (serapeum:fnil #'list 1 2) nil nil)))))

(test variadic->unary
  (is (= (max 1 2 3)
         (funcall (variadic->unary #'max) '(1 2 3))))
  (is (= (*)
         (funcall (variadic->unary #'*) nil))))

(test unary->variadic
  (is (= 1 (funcall (unary->variadic #'first) 1 2 3)))
  (is (= 3 (funcall (unary->variadic #'length) 1 2 3))))

(test nth-arg
  (is (= 3 (funcall (nth-arg 2) 1 2 3 4)))
  (is (= 3
         (locally (declare (notinline nth-arg))
           (funcall (nth-arg 2) 1 2 3 4)))))

(test distinct
  (let* ((d (distinct)))
    (flet ((f (x) (multiple-value-list (funcall d x))))
      (is (equal '(1 t) (f 1)))
      (is (equal '(nil nil) (f 1)))
      (is (equal '(2 t) (f 2))))))

(test distinct/key
  (is (equal '(0 1)
             (filter-map (distinct :key #'evenp) (iota 10)))))

(test distinct/ht
  "Test distinct works for longer lists, with or without a key."
  (is (length= 100 (filter-map (distinct) (iota 100))))
  (is (length= 100 (filter-map (distinct :key #'-) (iota 100)))))

(test flip
  (is (equal '(:b :a) (funcall (flip #'list) :a :b)))
  (locally (declare (notinline flip))
    (is (equal '(:b :a) (funcall (flip #'list) :a :b)))))

(test capped-fork
  (is (equal 15 (funcall (capped-fork (partial #'+ 2) (partial #'+ 3)) 10)))
  (is (equal '(:a (:b t :c) :d)
             (locally (declare (notinline capped-fork))
               (funcall (capped-fork (lambda (x) (list :a x :d))
                                     (lambda (y) (list :b y :c)))
                        t)))))
(test capped-fork2
  (is (equal 35 (funcall (capped-fork2 (partial #'+ 2) (partial #'+ 3)) 10 20)))
  (is (equal '(:a (:b 17 :c 25 :e) :d)
             (locally (declare (notinline capped-fork))
               (funcall (capped-fork2 (lambda (x) (list :a x :d))
                                      (lambda (y z) (list :b y :c z :e)))
                        17 25)))))

(test mvconstantly ()
  (macrolet ((test-body ()
               `(progn
                  (is (null (multiple-value-list
                             (funcall (mvconstantly)))))
                  (is (equal '(1)
                             (multiple-value-list
                              (funcall (mvconstantly 1)))))
                  (is
                   (equal '(1 2)
                          (multiple-value-list
                           (funcall (mvconstantly 1 2)))))
                  (is (equal '(1 1 1 1)
                             (let* ((x 0) (y 0)
                                    (fn (mvconstantly (incf x) (incf y))))
                               (multiple-value-call #'list
                                 (funcall fn)
                                 (funcall fn))))))))
    (test-body)
    (locally (declare (notinline mvconstantly))
      (test-body))))

(test fuel ()
  (let ((fuel (fuel 1)))
    (is (null (funcall fuel 2))))
  (let ((fuel (fuel 2)))
    (is (eql t (funcall fuel 2)))
    (is (null (funcall fuel 1))))
  (let ((fuel (fuel most-positive-double-float)))
    (signals error
      (funcall fuel double-float-epsilon))))

(test do-nothing ()
  (is (null (do-nothing)))
  (is (equal '() (multiple-value-list (do-nothing))))
  ;; Make sure we preserve side effects, in order.
  (let ((list '()))
    (do-nothing (push 1 list) (push 2 list))
    (is (equal list '(2 1)))))

(test repeat-until-stable
  (flet ((herons-method (S)
           "Return a function that iteratively estimates the square root of S."
           (lambda (n)
             (/ (+ n (/ S n))
                2d0))))
    (is (= 2.23606797749979d0 (repeat-until-stable (herons-method 5) 7)))
    (is (= 3.162319422150883d0 (repeat-until-stable (herons-method 10) 5 :max-depth 3)))))
