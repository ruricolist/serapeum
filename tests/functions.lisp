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
  (is (null (funcall (serapeum::hook #'= #'floor) 2.1)))
  (is (funcall (serapeum::hook #'= #'floor) 3)))

(test fork
  (let ((sample (range 100)))
    (is (= (mean sample)
           (funcall (serapeum::fork #'/
                                    (curry #'reduce #'+)
                                    #'length)
                    sample)))))

(test hook2
  (is (= 3.25
         (funcall (serapeum::hook2 #'+ (op (/ _ 60)))
                  3 15))))

(test fork2
  (is (equal '(11 9)
             (funcall (serapeum::fork2 #'list #'+ #'-)
                      10 1))))
