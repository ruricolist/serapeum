(in-package :serapeum.tests)

(def-suite hash-tables :in serapeum)
(in-suite hash-tables)

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

(test hash-table-function
  (fbind ((ht (hash-table-function (dict) :default 0)))
    (= 4 (+ 2 (ht :x 2)))))

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

(test hash-table-function/strict-read
  (fbind ((ht (hash-table-function (dict :x 1) :strict :read)))
    (is (= 1 (ht :x)))
    (signals error
      (ht :y))
    (ht :y 2)
    (is (= 2 (ht :y)))))

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
      (ht :y 2.0))))

(test set-hash-table
  (is
   (= 3 (hash-table-count
         (set-hash-table '(x y z)))))
  (signals error
    (set-hash-table '(x x y z))))

(test pairhash
  (is (equalp (pairhash '(1 2 3) '(4 5 6))
              (dict 'eql 1 4 2 5 3 6)))
  (signals error
    (pairhash '(1 2) '(3))))

(test maphash-new
  (let* ((old (dict "x" 1 "y" 2))
         (new (maphash-new #'values old)))
    (is (not (eql old new)))
    (is (equalp old new)))
  (let* ((old (dict "x" 1 "y" 2))
         (new (maphash-new #'values old :test 'eql)))
    (is (not (eql old new)))
    (is (eql (hash-table-test new) 'eql))
    (is (set-equal (hash-table-alist old)
                   (hash-table-alist new)
                   :test #'equal))))

(test maphash-into
  (let ((dict
          (maphash-into (dict)
                        (lambda (x)
                          (values (princ-to-string x) x))
                        '(1 2 3 4))))
    (is (eql 1 (gethash "1" dict))))
  (let ((dict
          (maphash-into (make-hash-table)
                        (lambda (x y z)
                          (values x (+ y z)))
                        '(1 2 3 4)
                        '(5 6 7 8)
                        '(9 10 11 12))))
    (is (= (gethash 4 dict) (+ 8 12)))))

(test flip-hash-table-docstring
  (is (equal '(:x t)
             (multiple-value-list
              (gethash :y (flip-hash-table (dict :x :y))))))
  (local
    (def number-names (dictq 1 one 2 two 3 three))

    (def name-numbers (flip-hash-table number-names))
    (def name-odd-numbers (flip-hash-table number-names :filter #'oddp))

    (is (equal '(2 t)
               (multiple-value-list
                (gethash 'two name-numbers))))
    (is (equal '(nil nil)
               (multiple-value-list
                (gethash 'two name-odd-numbers)))))
  (local
    (def number-names (dict 1 'one))
    (def negative-number-names (flip-hash-table number-names :key #'-))
    (is (equal '(-1 t)
               (multiple-value-list
                (gethash 'one negative-number-names))))))

(test flip-hash-table-test
  (let ((ht (dict 'eql 1 "one")))
    (is (eql 'eql (hash-table-test ht)))
    (is (eql 1 (gethash "one" (flip-hash-table ht :test 'equal))))))

(test hash-table-test-p
  (dolist (test '(eq eql equal equalp))
    (is (hash-table-test-p test)))
  (dolist (test '(eq eql equal equalp))
    (is (hash-table-test-p (symbol-function test))))
  (is (not (hash-table-test-p #'car))))

(test href-eval-order
  "Test that href arguments are evaluated left-to-right.
Regression for href/@ compiler macros."
  (let ((table (dict :x (dict :y (dict :z 'correct))))
        (list '()))
    (is (eql 'correct
             (href
              (progn (push 4 list) table)
              (progn (push 3 list) :x)
              (progn (push 2 list) :y)
              (progn (push 1 list) :z))))
    (is (equal list '(1 2 3 4))))
  (let ((table (dict :x (dict :y (dict :z 'correct))))
        (list '()))
    (is (eql 'correct
             (@
              (progn (push 4 list) table)
              (progn (push 3 list) :x)
              (progn (push 2 list) :y)
              (progn (push 1 list) :z))))
    (is (equal list '(1 2 3 4)))))
