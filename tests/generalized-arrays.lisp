(in-package :serapeum.tests)

(def-suite generalized-arrays :in serapeum)
(in-suite generalized-arrays)

(defun count* (n)
  (range 1 (1+ n)))

(test reshape
  (let* ((count (count* 48))
         (shape '(2 3 3 2))
         (array (reshape shape count)))
    (is (equal (shape array) '(2 3 3 2)))
    (is (= (last-elt (ravel array)) 36))))

(test ravel
  (is (array= (ravel (tell '(2 3)))
              #((0 0) (0 1) (0 2) (1 0) (1 1) (1 2)))))

(test each
  (is
   (array=
    '((1) (1 2) (1 2 3) (1 2 3 4))
    (each #'count* (count* 4)))))

(defun frequency (values array)
  (flet ((eq? (x y) (eif (equal x y) 1 0)))
    (each #'sum
          (each-left
           values
           (op (each-right _ #'eq? _))
           array))))

(test each-left
  (is
   (array=
    '("abc" "abcd" "abcde")
    (each-left '(3 4 5) #'reshape "abcde")))

  (assert
   (array= '(5 3 1)
           (frequency "abc" "The cat sat on the baseball bat"))))

(defun divisible-by (a b)
  (if (zerop (mod a b)) 1 0))

(test each-right
  (is
   (array= #*11010001
           (each-right 8 #'divisible-by (count* 8))))

  (is
   (= 4 (sum (each-right 8 #'divisible-by (count* 8))))))

(defun number-of-divisors (n)
  (sum (each-right n #'divisible-by (count* n))))

(defun prime? (n)
  (= 2 (number-of-divisors n)))

(defun sieve (numbers)
  (each #'prime? numbers))

(defun primes (n)
  (sieve (range (1+ n))))

(test prime-example
  (is (= 4 (number-of-divisors 8)))
  (is (prime? 7))
  (is (not (prime? 8)))
  (is (array= #(nil t t nil t nil t nil) (sieve (count* 8)))))

(test sum
  (is (= 0 (sum '())))
  (is (= 1 (sum '(1))))
  (let ((xs (range 1000)))
    (is (= (reduce #'+ xs)
           (sum xs))))
  (let ((xs (range 1001)))
    (is (= (reduce #'+ xs)
           (sum xs)))))

(test prod
  (is (= 1 (prod '())))
  (is (= 1 (prod '(1))))
  (let ((xs (range 1000)))
    (is (= (reduce #'* xs)
           (prod xs))))
  (let ((xs (range 1001)))
    (is (= (reduce #'* xs)
           (prod xs)))))
