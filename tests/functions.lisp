(in-package :serapeum.tests)

(def-suite functions :in serapeum)
(in-suite functions)

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
                       '(1 2 nil 4 6))))))

(test variadic->unary
  (is (= (max 1 2 3)
         (funcall (variadic->unary #'max) '(1 2 3))))
  (is (= (*)
         (funcall (variadic->unary #'*) nil))))

(test unary->variadic
  (is (= 1 (funcall (unary->variadic #'first) 1 2 3)))
  (is (= 3 (funcall (unary->variadic #'length) 1 2 3))))
