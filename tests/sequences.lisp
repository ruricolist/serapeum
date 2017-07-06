(in-package :serapeum.tests)

(def-suite sequences :in serapeum)
(in-suite sequences)

(test single
  ;; This is too trivial to really need a test, but it also serves a
  ;; regression for a package lock problem.
  (is (not (single '())))
  (is (single '(t)))
  (is (not (single '(t t))))

  (is (not (single "")))
  (is (single "x"))
  (is (not (single "xx")))

  (is (not (single #())))
  (is (single #(t)))
  (is (not (single #(t t)))))

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

(test nth-best
  (is (= 0 (nth-best 0 (shuffle (iota 1000)) #'<)))
  (is (= 1 (nth-best 1 (shuffle (iota 1000)) #'<)))
  (is (= 2 (nth-best 2 (shuffle (iota 1000)) #'<)))
  (is (= 5 (nth-best 5 (shuffle (iota 1000)) #'<)))
  (is (= 998 (nth-best 1 (shuffle (iota 1000)) #'< :key #'-)))
  (signals error
    (nth-best 1 () #'<))
  (signals error
    (nth-best 10000 (shuffle (iota 1000)) #'<))
  (signals error
    (nth-best -1 (shuffle (iota 1000)) #'<))
  (signals error
    (nth-best 1001 (shuffle (iota 1000)) #'<)))

(test extrema
  (is (equal (multiple-value-list (extrema '(1 2 3 4 5) #'<)) '(1 5))))

(test halves
  (is (equal (halves '(x)) '(x)))
  (is (equal (nth-value 1 (halves '(x) -1)) '(x)))
  (is (equal (multiple-value-list (halves '(x y))) '((x) (y))))
  (is (equal (multiple-value-list (halves '(x y) -1)) '((x) (y))))
  (is (equal (multiple-value-list (halves '(x y z))) '((x y) (z))))
  (is (equal (multiple-value-list (halves '(x y z) -2)) '((x) (y z))))

  (is (equal (halves "") ""))
  (is (equal (halves "" 1) ""))
  (is (equal (halves "" -1) ""))
  (is (equal (halves "x") "x"))
  (is (equal (nth-value 1 (halves "x" -1)) "x"))
  (is (equal (nth-value 1 (halves "x" -2)) "x")))

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
             '(0 9))))

(test repeat-sequence
  (is (equal "131313" (repeat-sequence "13" 3)))
  (is (equal '(13 13 13) (repeat-sequence '(13) 3)))
  (is (equal '("13" "13" "13") (repeat-sequence '("13") 3)))
  (is (vector= #(13 13 13) (repeat-sequence #(13) 3)))
  ;; 0 repetitions.
  (is (null (repeat-sequence '(x y z) 0)))
  (is (equal "" (repeat-sequence "foo" 0)))
  (is (stringp (repeat-sequence "foo" 0)))
  ;; Repeating empty sequences.
  (is (null (repeat-sequence nil 10)))
  (is (equal "" (repeat-sequence "" (1+ array-dimension-limit))))
  (is (stringp (repeat-sequence "" (1+ array-dimension-limit))))
  (is (equal "" (repeat-sequence "" (1+ most-positive-fixnum))))
  (is (stringp (repeat-sequence "" (1+ most-positive-fixnum)))))

(test take
  (is (equal "oo" (take -2 "foo")))
  (is (equal "fo" (take 2 "foo")))
  (is (equal "foo" (take -5 "foo"))))

(test drop
  (is (equal "" (drop -3 "foo")))
  (is (equal "" (drop -4 "foo")))
  (is (equal "f" (drop -2 "foo"))))
