(in-package :serapeum.tests)

(def-suite vectors :in serapeum)
(in-suite vectors)

(test ensure-vector
  (let ((vec #(1)))
    (is (eq vec (ensure-vector vec))))
  (is (vectorp (ensure-vector 1)))
  (is (vectorp (ensure-vector '(1)))))

(test vect
  (is (adjustable-array-p (vect)))
  (is (fill-pointer (vect)))
  (is (equalp (vect 1 2 3) #(1 2 3))))

(test vect-type
  (is (typep (vect) 'vect))
  (is (not (typep #() 'vect)))
  (is (not (typep (make-array 0 :adjustable t) 'vect)))
  (is (not (typep (make-array
                   10
                   :adjustable t
                   :fill-pointer 0
                   :element-type 'fixnum)
                  'vect)))
  (is (typep (make-array
              10
              :adjustable t
              :fill-pointer 0
              :element-type t)
             'vect)))

;;; Regression for #14.

(test dx-vect
  (is (equal
       (princ-to-string #((foo)))
       (princ-to-string ((lambda () (vect (list 'foo))))))))

(test values-vector
  (loop for i from 0 to 21
        for vec  = (range i)
        for list = (coerce vec 'list)
        do (is (equal (multiple-value-list (values-list list))
                      (multiple-value-list (values-vector vec))))))

(def-suite pad :in vectors)

(in-suite pad)

(test pad-start
  (is (equal "" (pad-start "" 0)))
  (is (equal "string" (pad-start "string" 3 "x")))
  (is (equal "string" (pad-start "string" 6 "x")))
  (is (equal "xstring" (pad-start "string" 7 "x")))
  (is (equal "xystring" (pad-start "string" 8 "xyz")))
  (is (equal "string" (pad-start "string" 8 "")))
  (is (equal "   string" (pad-start "string" 9)))
  (is (equal "abc" (pad-start "abc" 0)))
  (is (equal "abc" (pad-start "abc" 1)))
  (is (equal "abc" (pad-start "abc" 0 "")))
  (is (equal "abc" (pad-start "abc" 1 "")))
  (is (equal "0000000001" (pad-start "1" 10 #\0)))
  (is (equal "0000000012" (pad-start "12" 10 #\0)))
  (is (equal "0000123456" (pad-start "123456" 10 #\0)))
  (is (equal "YYYY-MM-12" (pad-start "12" 10 "YYYY-MM-DD")))
  (is (equal "YYYY-09-12" (pad-start "09-12" 10 "YYYY-MM-DD")))
  (is (equal "xxabc" (pad-start "abc" 5 #\x)))
  (is (equal "       abc" (pad-start "abc" 10)))
  (is (equal "00000abc" (pad-start "abc" 8 "0")))
  (is (equal "foofoofabc" (pad-start "abc" 10 "foo")))
  (is (equal "123abc" (pad-start "abc" 6 "123456")))

  (signals type-error
    (is (equal "x" (pad-start "" 3 #((progn (error "Don't eval me!")))))))

  ;; ECMA suite.
  (signals type-error
    (pad-start "abc" 10 'symbol))
  (signals type-error
    (pad-start nil 1))
  (signals type-error
    (pad-start 'symbol 1))
  (is (equal "abc" (pad-start "abc" 5 "")))
  (is (equal "   abc" (pad-start "abc" 6)))
  (is (equal "**abc" (pad-start "abc" 5 "*")))
  (locally (declare (notinline pad-start))
    (is (equal "**abc" (pad-start "abc" 5 "*"))))
  (is (equal "**abc" (pad-start "abc" 5 #\*))))

(test pad-end
  (is (equal "123   " (pad-end "123" 6)))
  (is (equal "123xxx" (pad-end "123" 6 #\x)))
  (is (equal "123xxx" (pad-end "123" 6 "x")))
  ;; Possibly surprising behaviors.
  (is (equal "2016YYYY-M" (pad-end "2016" 10 "YYYY-MM-DD")))
  (let ((year "2016"))
    (is (equal "2016-MM-DD" (pad-end year 10 (subseq "YYYY-MM-DD" (length year))))))

  (signals type-error
    (is (equal "x" (pad-end "" 3 #((progn (error "Don't eval me!")))))))

  ;; ECMA
  (signals type-error
    (pad-end "abc" 10 'symbol))
  (is (equal "abc" (pad-end "abc" 5 "")))
  (is (equal "abc  " (pad-end "abc" 5)))
  (is (equal "abcdefd" (pad-end "abc" 7 "def")))
  (is (equal "abc**" (pad-end "abc" 5 "*")))
  (locally (declare (notinline pad-end))
    (is (equal "abc**" (pad-end "abc" 5 "*"))))
  (is (equal "abc**" (pad-end "abc" 5 #\*))))

(in-suite vectors)

(test vector-conc-extend
  (let ((v (vect 1 2 3)))
    (is (= 3 (length (vector-conc-extend v '()))))
    (is (= 3 (length (vector-conc-extend v #()))))
    (is (seq= '(1 2 3 4)
              (vector-conc-extend v #(4))))
    (is (seq= '(1 2 3 4 5 6)
              (vector-conc-extend v #(5 6)))))
  (is (equal "abcxyz"
             (vector-conc-extend
              (make-array 3 :adjustable t
                            :element-type 'character
                            :fill-pointer 3
                            :initial-contents "abc")
              "xyz"))))

(test bisect-left
  (is (eql 1 (bisect-left #(1 2 2 3 5) 2 #'<)))
  (is (eql 1 (bisect-left #(#\A #\Z #\a #\z) #\B #'char<))))

(test bisect-right
  (is (eql 3 (bisect-right #(1 2 2 3 5) 2 #'<)))
  (is (eql 2 (bisect-right #(#\A #\Z #\a #\z) #\Z #'char<))))
