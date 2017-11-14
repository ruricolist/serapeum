(in-package :serapeum.tests)

(def-suite op :in serapeum)
(in-suite op)

(test op
  ;; Constantly.
  (is (= (funcall (op 1)) 1))
  ;; Identity.
  (is (= (funcall (op _) 1) 1))
  ;; Identity with rest.
  (is (equal (funcall (op _*) 1 2 3) '(1 2 3)))
  ;; Positional.
  (is (= (funcall (op (+ 1 _)) 1) 2))
  ;; Backward reference.
  (is (= (funcall (op (+ _ _1)) 2) 4))
  ;; Rest.
  (is (equal (apply (op (list 1 _*)) '(2 3)) '(1 2 3)))
  ;; Splicing.
  (is (equal (funcall (op (list _* 1)) 3 2) '(3 2 1)))
  ;; Positional and rest.
  (is (equal (apply (op (list _ _*)) 1 '(2 3)) '(1 2 3)))
  ;; Flip
  (is (eql 4 (find '(4) (range 10) :test (op (member _2 _1)))))
  ;; nth-arg
  (is (equal '(4 5 6) (mapcar (op _2) '(1 2 3) '(4 5 6))))
  ;; Sparse argument lists.
  (is (= 9 (apply (op (+ _1 _3 _5)) '(1 2 3 4 5))))
  )

(test op-quasiquote
  ;; Backquotes.
  (is (equal '((:x 1) (:x 2) (:x 3))
             (mapcar (op `(:x ,_)) '(1 2 3)))))

(test op-fn
  (is (= 3 (funcall (op (_ _ _))
                    #'+ 1 2))))

(test op-quote
  (is (equal '(x _)
             (funcall (op (cons _ '(_)))
                      'x)))

  (is (equal '(x _)
             (macrolet ((tail () ''(_)))
               (funcall (op (cons _ (tail)))
                        'x)))))

;;; Failing tests (on SBCL at least).

(test (lexical-underscore-around :compile-at :run-time)
  "Make sure placeholders shadow lexical bindings in the surrounding
environment."
  (is (= 4
         (let ((_ 1)) (declare (ignorable _))
           (funcall
            (op (+ _ _))
            2 2)))))

;; (test (lexical-underscore-inside :compile-at :run-time)
;;   (is (= 2
;;          (funcall
;;           (op (+ (let ((_ 1))
;;                    _)
;;                  (let ((_ 1))
;;                    _)))
;;           2 2))))
