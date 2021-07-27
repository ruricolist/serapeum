(in-package :serapeum.tests)

(def-suite sequences :in serapeum)
(in-suite sequences)

;;; TODO Actually test with extensible sequences.

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

(test only-elt
  (signals error
    (only-elt '()))
  (signals error
    (only-elt #()))
  (signals error
    (only-elt '(1 . 2)))
  (signals error
    (only-elt '(1 2)))
  (signals error
    (only-elt #(1 2)))
  (is (eql 1 (only-elt '(1))))
  (is (eql 1 (only-elt #(1)))))

(test scan
  (is (equal '() (scan #'+ '())))
  (is (equal '(1) (scan #'+ '(1))))
  (is (equal '(1)(scan #'+ '() :initial-value 1)))
  (is (equal '(1 3 6 10) (scan #'+ '(1 2 3 4))))
  (is (equal '(1 3 6 10) (scan #'+ '(2 3 4) :initial-value 1)))
  (is (equal '(1 3 6 10) (scan #'+ '(2 3 4) :initial-value 1)))
  (is (equal '(-1 -3 -6 -10)
             (scan #'+ '(1 2 3 4) :key #'-)))
  (is (equal (list (reduce #'+ '() :initial-value 1 :key #'-))
             (scan #'+ '() :initial-value 1 :key #'-))))

(test filter-with-count
  (is (seq= '(0 2 4 6 8) (filter #'evenp (range 100) :count 5)))
  (is (equalp #(0 2 4 6 8) (filter #'evenp (coerce (range 100) 'vector) :count 5)))
  (is (seq= '(90 92 94 96 98)
            (filter #'evenp (range 100) :count 5 :from-end t))))

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
  (is (seq= (assort (range 10)
                    :key (lambda (x)
                           (mod x 3)))
            '((0 3 6 9) (1 4 7) (2 5 8))))
  (is (seq= (assort (range 10)
                    :key (lambda (x)
                           (mod x 3))
                    :hash t)
            '((0 3 6 9) (1 4 7) (2 5 8))))

  (is (equal (assort "How Now Brown Cow" :key #'upper-case-p)
             '("HNBC" "ow ow rown ow")))
  (is (equal (assort "How Now Brown Cow" :key #'upper-case-p :hash t)
             '("HNBC" "ow ow rown ow"))))

(test assort-partial-order
  (is (equal (assort '(1 2 1 2 1 2) :test #'<=)
             '((1 1) (2 2 1 2)))))

(test assort-simple-vector
  (finishes (assort (coerce #(1 2 3) 'simple-vector)))
  (finishes (assort (coerce #(1 2 3) 'simple-vector)) :hash t))

(test runs
  (is (equal '((1 2) (3 4 5 6 11 12 13))
             (runs '(1 2 3 4 5 6 11 12 13) :key (rcurry #'< 3))))
  (is (equal '((1 2))
             (runs '(1 2 3 4 5 6 11 12 13)
                   :key (rcurry #'< 3)
                   :count 1))))

(test runs-compare-first
  (is (seq= (runs #(10 2 3 10 4 5) :test #'>)
            (runs '(10 2 3 10 4 5) :test #'>)))
  (is (seq= (runs #(10 2 3 10 4 5) :test #'> :count 0)
            (runs '(10 2 3 10 4 5) :test #'> :count 0)))
  (is (seq= (runs #(10 2 3 10 4 5) :test #'> :count 1)
            (runs '(10 2 3 10 4 5) :test #'> :count 1)))
  (is (seq= (runs #(1 2 3 1 2 3) :test #'<)
            (runs '(1 2 3 1 2 3) :test #'<)))
  (is (seq= (runs #(1 2 3 1 2 3) :test #'< :count 0)
            (runs '(1 2 3 1 2 3) :test #'< :count 0)))
  (is (seq= (runs #(1 2 3 1 2 3) :test #'< :count 1)
            (runs '(1 2 3 1 2 3) :test #'< :count 1))))

(test runs-count
  (is (null (runs '() :count 0)))
  (is (emptyp (runs #() :count 0)))
  (is (equal '((head) (tail tail))
             (runs '(head tail tail head head tail) :count 2)))
  (for-all ((i (lambda () (random 100)))
            (j (lambda () (random 10))))
    (let* ((ns (shuffle (range i)))
           (ns-list (coerce ns 'list)))
      (is (>= j (length (runs ns :test #'< :count j))))
      (is (>= j (length (runs ns-list :test #'< :count j)))))))

(test batches
  (is (equal '((a b) (c d) (e)) (batches '(a b c d e) 2)))
  (is (equal '("ab" "cd" "e") (batches "abcde" 2)))
  (is (equal '("a") (batches "abc" 2 :end 1)))
  (is (equal '((a)) (batches '(a b c) 2 :end 1))))

(test batches-even
  (signals error
    (batches '(a b c d e) 2 :even t))
  (signals error
    (batches "abcde" 2 :even t))

  (signals error
    (batches "abc" 2 :end 1 :even t))
  (is (equal '("ab") (batches "abc" 2 :end 2 :even t)))

  (signals error
    (batches '(a b c) 2 :end 1 :even t))
  (is (equal '((a b)) (batches '(a b c) 2 :end 2 :even t))))

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
  (is (equal "" (slice "" -1)))
  (is (equal "" (slice "" 0 -1)))
  (is (equal "" (slice "" -1 -1)))
  (is (equal "" (slice "x" 1 -1)))
  (for-all ((start (an-index))
            (end (an-index))
            (string
             (lambda ()
               (whichever "" "x" "string"))))
    (is (equal (drop (- end)
                     (drop start string))
               (slice string start (- end))))
    (is (equal (drop start
                     (drop (- end) string))
               (slice string start (- end))))))

(test setslice
  (let* ((string "xyz")
         (orig-string string))
    (setf (slice string 1 -1) "a")
    (is (equal string "xaz"))
    (is (not (eq string orig-string)))))

(test ordering
  (for-all ((vec (an-iota 1000)))
    (let ((vec (shuffle vec)))
      (is (vector= vec
                   (sort (reshuffle vec)
                         (ordering vec)))))))

(test reshuffle
  (let ((list (coerce (range 256) 'list)))
    (is (typep (reshuffle list :element-type '(unsigned-byte 8))
               '(simple-array (unsigned-byte 8) (*))))))

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
  (is (= 0 (nth-best 0 (shuffle (range 1000)) #'<)))
  (is (= 1 (nth-best 1 (shuffle (range 1000)) #'<)))
  (is (= 2 (nth-best 2 (shuffle (range 1000)) #'<)))
  (is (= 5 (nth-best 5 (shuffle (range 1000)) #'<)))
  (is (= 998 (nth-best 1 (shuffle (range 1000)) #'< :key #'-)))
  (signals error
    (nth-best 1 () #'<))
  (signals error
    (nth-best 10000 (shuffle (range 1000)) #'<))
  (signals error
    (nth-best -1 (shuffle (range 1000)) #'<))
  (signals error
    (nth-best 1001 (shuffle (range 1000)) #'<)))

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
                      (range 10) 0 0))
             '(0 9)))
  (is (equal (multiple-value-list
              (mvfold (lambda (item min max)
                        (values (min item min)
                                (max item max)))
                      (range 10) 0 0))
             '(0 9))))

(test (mvfold-compiler-macro :compile-at :run-time)
  (local
    (defun extract-format (args/format)
      (multiple-value-bind (format args)
          (mvfold (lambda (format args arg)
                    (if (keywordp arg)
                        (values arg args)
                        (values format (cons arg args))))
                  args/format :rows nil)
        (values format (reverse args))))

    (is (eql :single (extract-format '(name :single))))))

(test (mvfoldr-compiler-macro :compile-at :run-time)
  (local
    (defun foo ()
      (serapeum:mvfoldr (lambda (val min max)
                          (values (min min val) (max max val)))
                        (loop for i below 100 collect i)
                        0
                        0))

    (is (equal '(0 99) (multiple-value-list (foo))))))

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

(test take-while
  (is (equal "" (take-while #'whitespacep ""))))

(test drop
  (is (equal "" (drop -3 "foo")))
  (is (equal "" (drop -4 "foo")))
  (is (equal "f" (drop -2 "foo"))))

(test drop-while
  (is (equal "" (drop-while #'whitespacep ""))))

(test drop-prefix
  (let ((seq "x"))
    (is (eql seq (drop-prefix ":" seq)))
    (is (eql seq (drop-prefix '(#\:) seq)))
    (is (eql seq (drop-prefix #(#\:) seq)))
    (is (eql seq (drop-prefix "" seq)))
    (is (eql seq (drop-prefix nil seq))))
  (is (equal " world" (drop-prefix "hello" "hello world")))
  (is (equal " world" (drop-prefix '(#\h #\e #\l #\l #\o) "hello world")))
  (is (equal " world" (drop-prefix #(#\h #\e #\l #\l #\o) "hello world")))
  (is (equalp #(1 2 3) (drop-prefix #(0) #(0 1 2 3))))
  (is (equalp #(1 2 3) (drop-prefix '(0) #(0 1 2 3)))))

(test ensure-prefix
  (is (equal "x" (ensure-prefix "x" "")))
  (is (equal "x" (ensure-prefix "x" "x")))
  (is (equal "xy" (ensure-prefix "x" "y"))))

(test drop-suffix
  (let ((seq "x"))
    (is (eql seq (drop-suffix ":" seq)))
    (is (eql seq (drop-suffix '(#\:) seq)))
    (is (eql seq (drop-suffix #(#\:) seq)))
    (is (eql seq (drop-suffix "" seq)))
    (is (eql seq (drop-suffix nil seq))))
  (is (equal "hello " (drop-suffix "world" "hello ")))
  (is (equal "hello " (drop-suffix '(#\w #\o #\r #\l #\d) "hello world")))
  (is (equal "hello " (drop-suffix #(#\w #\o #\r #\l #\d) "hello world")))
  (is (equalp #(0 1 2) (drop-suffix #(3) #(0 1 2 3))))
  (is (equalp #(0 1 2) (drop-suffix '(3) #(0 1 2 3)))))

(test ensure-suffix
  (is (equal "x" (ensure-suffix "" "x")))
  (is (equal "x" (ensure-suffix "x" "x")))
  (is (equal "yx" (ensure-suffix "y" "x"))))

(test seq=
  (is (seq= '() ""))
  (is (seq= #() ""))
  (is (seq= #() '()))
  (is (seq= '(1) #(1)))
  (is (seq= "" ""))
  (is (not (seq= "" "xyz")))
  (is (seq= "xyz" "xyz"))
  (is (not (seq= "xyz" "XYZ")))
  (is (not (seq= '(1) #())))
  (is (not (seq= '() #(1))))
  (is (seq= '("xyz") #((#\x #\y #\z))))
  (is (not (seq= '("xyza") #((#\x #\y #\z)))))
  (is (not (seq= '("xyz") #((#\x #\y #\z #\a))))))

(defun split-seq/do-splits (seq fn &key from-end)
  (collecting
    (do-splits ((l r) (seq fn :from-end from-end))
      (collect (subseq seq l r)))))

(test do-splits
  (is (equalp (split-sequence-if #'oddp #())
              (split-seq/do-splits #() #'oddp)))
  (is (equalp (split-sequence-if #'oddp #(1 2))
              (split-seq/do-splits #(1 2) #'oddp)))
  (is (equalp (split-sequence-if #'oddp #(1 2 3))
              (split-seq/do-splits #(1 2 3) #'oddp))))

(test collapse-duplicates
  (is (seq= '(1 2 1)
            (collapse-duplicates #(1 1 2 2 1 1))
            (collapse-duplicates '(1 1 2 2 1 1)))))

(test toposort
  (local
    (def dem-bones '((toe foot)
                     (foot heel)
                     (heel ankle)
                     (ankle shin)
                     (shin knee)
                     (knee back)
                     (back shoulder)
                     (shoulder neck)
                     (neck head)))
    (def shuffle (reshuffle (mapcar #'car dem-bones)))
    (is (not (seq= shuffle dem-bones)))
    (is (every #'eql
               (mapcar #'car dem-bones)
               (sort shuffle (toposort dem-bones))))))

(test toposort-equal
  (let ((inconsistent-constraints '(("x" "y") ("y" "x"))))
    (finishes
      (let ((constraints (leaf-map #'copy-seq inconsistent-constraints)))
        (toposort constraints :test #'eql)))
    (signals inconsistent-graph
      (toposort inconsistent-constraints :test #'equal))))

(test same
  ;; See #88.
  (is-true (same #'length '((1 2 3) (a b c) (foo bar baz))))
  (is-true (same (distinct) '(1 2 3) :test (op (and _ _))))
  (is-false (same (distinct) '(1 1) :test (op (and _ _))))
  (is-true
   (same (juxt #'first #'second #'third)
         (make-list 3 :initial-element '(1 2 3))
         :test #'equal))
  (is-false (same #'numberp '(a 3 4 5)))
  (is-true (same #'numberp '(3 4 5 6)))
  (is-false (same #'numberp '(3 4 5 a)))
  (is-true (same #'numberp nil))
  (signals error
    (same #'numberp t))
  (is-true (same #'null nil))
  (is-true (same #'symbolp nil))
  (is-true (same #'numberp '(1)))
  (is-true (same #'symbolp '(a)))
  ;; Exercise: This is correct, but why?
  (is-true (same #'numberp '(a)))
  ;; Same exercise.
  (is-true (same #'oddp '(2 4 6 8 10)))
  (is-false (same #'oddp '(2 4 6 8 9 10)))
  (is-true (same #'null '(nil nil nil nil nil)))
  (is-true (same #'symbolp '(a b c d e f)))
  (is-false (same #'symbolp `(a b c d e f ,pi)))
  ;; Works on vectors too.
  (is-false (same #'numberp #(a 3 4 5)))
  (is-true (same #'numberp #(3 4 5 6))))

(test bisect-left
  (is (eql 1 (bisect-left #(1 2 2 3 5) 2 #'<)))
  (is (eql 1 (bisect-left #(#\A #\Z #\a #\z) #\B #'char<))))

(test bisect-right
  (is (eql 3 (bisect-right #(1 2 2 3 5) 2 #'<)))
  (is (eql 2 (bisect-right #(#\A #\Z #\a #\z) #\Z #'char<))))
