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
      (ht :y 2.0))))
