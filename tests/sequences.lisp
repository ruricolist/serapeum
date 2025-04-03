(in-package :serapeum.tests)

(def-suite sequences :in serapeum)
(in-suite sequences)

;;; Borrowed from the test suite of split-sequence. TODO: also test on
;;; ABCL and Clasp.
#+sbcl
(progn
  (defclass eseq (standard-object sequence)
    ((actual-seq :type list :initarg :actual-seq :initform nil
                 :accessor actual-seq))
    (:documentation "Extended sequence type in SBCL"))
  (defmethod sb-sequence:length ((s eseq))
    (length (actual-seq s)))
  (defmethod sb-sequence:elt ((s eseq) index)
    (elt (actual-seq s) index))
  (defmethod (setf sb-sequence:elt) (v (s eseq) index)
    (setf (elt (actual-seq s) index) v))
  (defmethod sb-sequence:adjust-sequence ((s eseq) len &rest args)
    (setf (actual-seq s)
          (apply #'sb-sequence:adjust-sequence (actual-seq s) len args)))
  (defmethod sb-sequence:make-sequence-like ((s eseq) len &rest args)
    (make-instance 'eseq :actual-seq (apply #'sb-sequence:make-sequence-like
                                            (actual-seq s) len args)))
  (defmethod print-object ((s eseq) stream)
    (print-unreadable-object (s stream :type t)
      (format stream "~a" (actual-seq s))))
  (defun eseq (&rest args)
    (make 'eseq :actual-seq args)))
#-sbcl
(defun eseq (&rest args) args)


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
  (is (not (single #(t t))))

  (is (not (single (eseq))))
  (is (single (eseq t)))
  (is (not (single (eseq t t)))))

(test only-elt
  (signals error
    (only-elt '()))
  (signals error
    (only-elt #()))
  (signals error
    (only-elt (eseq)))
  (signals error
    (only-elt '(1 . 2)))
  (signals error
    (only-elt '(1 2)))
  (signals error
    (only-elt #(1 2)))
  (signals error
    (only-elt (eseq 1 2)))
  (is (eql 1 (only-elt '(1))))
  (is (eql 1 (only-elt #(1))))
  (is (eql 1 (only-elt (eseq 1)))))

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
  (is (equalp #(0 2 4 6 8) (filter #'evenp (range 100) :count 5)))
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

(test assort-eseq
  (finishes (assort (eseq 1 2 3)))
  (finishes (assort (eseq 1 2 3)))
  (is (seq= (assort (eseq 1 2 1 2 1 2) :test #'<=)
            '((1 1) (2 2 1 2)))))

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
  (is (seq= (runs (eseq 10 2 3 10 4 5) :test #'>)
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
  (is (eql (length (runs (eseq) :count 0)) 0))
  (is (equal '((head) (tail tail))
             (runs '(head tail tail head head tail) :count 2)))
  (for-all ((i (lambda () (random 100)))
            (j (lambda () (random 10))))
    (let* ((ns (shuffle (range i)))
           (ns-list (coerce ns 'list)))
      (is (>= j (length (runs ns :test #'< :count j))))
      (is (>= j (length (runs ns-list :test #'< :count j)))))))

(test runs-compare-last
  (is (equal '(()) (runs '() :compare-last t)))
  (is (equalp '((1)) (runs '(1) :compare-last t)))
  (is (equalp '(#()) (runs #() :compare-last t)))
  (is (seq= (list (eseq)) (runs (eseq) :compare-last t)))
  (is (equalp '(#(1)) (runs #(1) :compare-last t)))
  (is (seq= (runs #(10 2 3 2 1) :test #'> :compare-last t)
            (runs '(10 2 3 2 1) :test #'> :compare-last t)
            '((10 2) (3 2 1)))))

(test batches
  (is (equal '((a b) (c d) (e)) (batches '(a b c d e) 2)))
  (is (equal '("ab" "cd" "e") (batches "abcde" 2)))
  (is (equal '("a") (batches "abc" 2 :end 1)))
  (is (equal '((a)) (batches '(a b c) 2 :end 1)))
  (is (seq= (list (eseq 'a 'b) (eseq 'c 'd)) (batches (eseq 'a 'b 'c 'd) 2))))

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
  (is (null (gcp nil)))
  (is (equal (gcp '("miss" "molly")) "m")))

(test gcs
  (is (null (gcs nil)))
  (is (equal (gcs '("how" "now")) "ow")))

(test length<
  (is (length< #() 1))
  (is (length< (eseq) 1))
  (is (length< '(1) 2))
  (is (not (length< '(1 2) 2)))
  (is (not (length< '(1 2 3) 2))))

(test length>
  (is (not (length> '(1) 2)))
  (is (not (length> '(1 2) 2)))
  (is (length> '(1 2 3) 2))
  (is (not (length> nil 0))))

(test length>=
  ;; Regression; see #147.
  (is (not (length>= '() '(1) '(2)))))

(test length-fns
  (let* ((equivs
           '((length= . =)
             (length< . <)
             (length> . >)
             (length<= . <=)
             (length>= . >=))))
    (for-all
        ((data
          (lambda ()
            (let* ((len (random-in-range 2 10))
                   (seq-lens
                     (loop repeat len collect (random 10)))
                   (seq-types
                     (loop repeat len collect (random-elt '(list vector integer)))))
              (list len seq-lens seq-types)))))
      (destructuring-bind (len seq-lens seq-types) data
        (let ((seqs
                (loop for type in seq-types
                      for len in seq-lens
                      collect (ecase type
                                (list (make-list len))
                                (vector (make-array (list len)))
                                (integer len)))))
          (flet ((len (x)
                   (etypecase x
                     (list (length x))
                     (vector (length x))
                     (integer x))))
            (dolist (equiv equivs)
              (let ((length-fn (car equiv))
                    (num-fn (cdr equiv)))
                (is (eql (apply length-fn seqs)
                         (apply num-fn (mapcar #'len seqs))))))))))))

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

(test list-halves
  (is (equal (halves '(x)) '(x)))
  (is (equal (halves '(x) 1) '(x)))
  (is (equal (multiple-value-list (halves '(x) -1)) '(nil (x))))
  (is (equal (multiple-value-list (halves '(x y))) '((x) (y))))
  (is (equal (multiple-value-list (halves '(x y z))) '((x y) (z))))
  (is (equal (multiple-value-list (halves '(x y z) -2)) '((x) (y z))))
  (is (equal '(1 2 3) (halves '(1 2 3) 6))))

(test list-halves/negative
  (for-all ((list (lambda () (iota (random 20))))
            (split (lambda () (- (random 20)))))
    (multiple-value-bind (left right) (halves list split)
      (is (equal left (butlast list (abs split))))
      (is (equal right (last list (abs split)))))))

(test string-halves
  (is (equal (halves "") ""))
  (is (equal (halves "" 1) ""))
  (is (equal (halves "" -1) ""))
  (is (equal (halves "x") "x"))
  (is (equal (nth-value 1 (halves "x" -1)) "x"))
  (is (equal (nth-value 1 (halves "x" -2)) "x")))

(test deltas
  (is (equal '(4 5 -14 6 1) (deltas '(4 9 -5 1 2))))
  (is (equal '(4 5 -14 6 1) (deltas #(4 9 -5 1 2))))
  (is (seq= '(4 5 -14 6 1) (deltas (eseq 4 9 -5 1 2)))))

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
  (is (seq= (eseq 13 13 13) (repeat-sequence (eseq 13) 3)))
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
  (is (equal "" (take-while #'whitespacep "")))
  (is (equal "  " (take-while #'whitespacep "foo  " :from-end t)))
  (is (equal "  " (take-while #'whitespacep "  ")))
  (is (equal "  " (take-while #'whitespacep "  " :from-end t)))
  (is (equal '(#\Space #\Space)
             (take-while #'whitespacep
                         (coerce "foo  " 'list)
                         :from-end t)))
  (is (equal "" (take-while #'whitespacep "foo" :from-end t)))
  (is (equal "" (take-while #'whitespacep "" :from-end t)))
  (is (equal "Really" (take-while #'alpha-char-p "Really!?")))
  (is (equal
       "!?"
       (take-while
        (complement #'alpha-char-p)
        "Really!?"
        :from-end t))))

(test drop
  (is (equal "" (drop -3 "foo")))
  (is (equal "" (drop -4 "foo")))
  (is (equal "f" (drop -2 "foo"))))

(test drop-while
  (is (equal "" (drop-while #'whitespacep "")))
  (is (equal "" (drop-while #'whitespacep "" :from-end t)))
  (is (equal "1" (drop-while #'whitespacep "   1")))
  (is (equal "1" (drop-while #'whitespacep "1   " :from-end t)))
  (is (equal "" (drop-while #'whitespacep "   ")))
  (is (equal "" (drop-while #'whitespacep "   " :from-end t)))
  (is (equal "" (drop-until (complement #'whitespacep) "   ")))
  (is (equal "" (drop-until (complement #'whitespacep) "   "
                            :from-end t)))
  (is (equal "!?" (drop-while #'alpha-char-p "Really!?")))
  (is (equal
       "Really"
       (drop-while
        (complement #'alpha-char-p)
        "Really!?"
        :from-end t))))

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
      (is (equalp #(1 2 3) (drop-prefix '(0) #(0 1 2 3))))
      (is (seq= #(1 2 3) (drop-prefix '(0) (eseq 0 1 2 3)))))

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

(test toposort-list-constraints
  "Check we can use constraints with elements that are lists."
  (let ((constraints '(((x 1) (y 2))
                       ((y 2) (z 3)))))
    (is (equal '((x 1) (y 2) (z 3))
               (sort (list '(z 3) '(y 2) '(x 1))
                     (toposort constraints :test #'equal))))))

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

(test copy-firstn
  ;; Simple cases
  (is (eq '() (copy-firstn '() 0)))
  (is (eq '() (copy-firstn '() 1)))
  (let ((list '(1)))
    (is (eq list (copy-firstn list 0))))
  ;; Proper list
  (let* ((list '(1 2 3 4 5 6))
         (copied (copy-firstn list 3)))
    (is (not (eq list copied)))
    (is (equal '(1 2 3) (subseq copied 0 3)))
    (is (eq (nthcdr 3 list) (nthcdr 3 copied))))
  ;; Dotted list
  (let* ((list '(1 2 3 . 4))
         (copied (copy-firstn list 3)))
    (is (not (eq list copied)))
    ;; Do not try to use SUBSEQ here, because the list is improper.
    (is (= 1 (first copied)))
    (is (= 2 (second copied)))
    (is (= 3 (third copied)))
    (is (eq (nthcdr 3 list) (nthcdr 3 copied))))
  ;; Cyclic list
  (let* ((cycle '#1=(1 2 3 . #1#))
         (copied (copy-firstn cycle 6)))
    (is (not (eq cycle copied)))
    ;; Do not try to use SUBSEQ here, because the list is improper.
    (is (= 1 (first copied)))
    (is (= 2 (second copied)))
    (is (= 3 (third copied)))
    (is (= 1 (fourth copied)))
    (is (= 2 (fifth copied)))
    (is (= 3 (sixth copied)))
    (is (eq cycle (nthcdr 6 copied)))))

(test splice-vector-simple
  (is (vector= #() (splice-seq #())))
  (is (vector= #() (splice-seq #(1 2 3))))
  (is (vector= #() (splice-seq #(1 2 3) :new #())))
  (flet ((frob (expected &rest args)
           (let* ((vector #(1 2 3 4 5 6 7 8 9))
                  (new #(a b c))
                  (actual (apply #'splice-seq vector :new new args)))
             (is (not (eq vector actual)))
             (is (vector= expected actual)))))
    (frob #(a b c))
    (frob #(a b c 1 2 3 4 5 6 7 8 9) :end 0)
    (frob #(1 2 3 4 5 6 7 8 9 a b c) :start 9)
    (frob #(1 a b c 2 3 4 5 6 7 8 9) :start 1 :end 1)
    (frob #(a b c 4 5 6 7 8 9) :end 3)
    (frob #(1 2 3 a b c 7 8 9) :start 3 :end 6)
    (frob #(1 2 3 4 5 6 a b c) :start 6)
    (frob #(1 a b c 9) :start 1 :end 8)))

(test splice-vector-complex
  (flet ((frob (expected &rest args)
           (let* ((vector #(1 2 3 4 5 6 7 8 9))
                  (new #(a b c))
                  (actual (make-array 9 :initial-contents vector
                                        :adjustable t))
                  (actual-2 (apply #'splice-seq actual :new new args)))
             (is (not (eq actual actual-2)))
             (is (vector= expected actual-2)))))
    (frob #(a b c))
    (frob #(a b c 1 2 3 4 5 6 7 8 9) :end 0)
    (frob #(1 2 3 4 5 6 7 8 9 a b c) :start 9)
    (frob #(1 a b c 2 3 4 5 6 7 8 9) :start 1 :end 1)
    (frob #(a b c 4 5 6 7 8 9) :end 3)
    (frob #(1 2 3 a b c 7 8 9) :start 3 :end 6)
    (frob #(1 2 3 4 5 6 a b c) :start 6)
    (frob #(1 a b c 9) :start 1 :end 8)))

(test nsplice-vector-simple
  (is (vector= #() (nsplice-seq #())))
  (is (vector= #() (nsplice-seq (vector 1 2 3))))
  (is (vector= #() (nsplice-seq (vector 1 2 3) :new #())))
  ;; VECTOR might be actually adjustable and therefore we will never hit
  ;; the path for non-adjustable vectors, but we can not care. By
  ;; NSPLICE-SEQ's contract, we are allowed to destroy the original vector and
  ;; the return value of NSPLICE-SEQ should not be discarded.
  (flet ((frob (expected &rest args)
           (let* ((vector (vector 1 2 3 4 5 6 7 8 9))
                  (new (vector 'a 'b 'c))
                  (actual (apply #'nsplice-seq vector :new new args)))
             (is (vector= expected actual)))))
    (frob #(a b c))
    (frob #(a b c 1 2 3 4 5 6 7 8 9) :end 0)
    (frob #(1 2 3 4 5 6 7 8 9 a b c) :start 9)
    (frob #(1 a b c 2 3 4 5 6 7 8 9) :start 1 :end 1)
    (frob #(a b c 4 5 6 7 8 9) :end 3)
    (frob #(1 2 3 a b c 7 8 9) :start 3 :end 6)
    (frob #(1 2 3 4 5 6 a b c) :start 6)
    (frob #(1 a b c 9) :start 1 :end 8)))

(test nsplice-vector-complex
  (flet ((frob (expected &rest args)
           (let* ((vector #(1 2 3 4 5 6 7 8 9))
                  (new #(a b c))
                  (actual (make-array 9 :initial-contents vector
                                        :adjustable t))
                  (actual-2 (apply #'nsplice-seq actual :new new args)))
             (is (eq actual actual-2))
             (is (vector= expected actual-2)))))
    (frob #(a b c))
    (frob #(a b c 1 2 3 4 5 6 7 8 9) :end 0)
    (frob #(1 2 3 4 5 6 7 8 9 a b c) :start 9)
    (frob #(1 a b c 2 3 4 5 6 7 8 9) :start 1 :end 1)
    (frob #(a b c 4 5 6 7 8 9) :end 3)
    (frob #(1 2 3 a b c 7 8 9) :start 3 :end 6)
    (frob #(1 2 3 4 5 6 a b c) :start 6)
    (frob #(1 a b c 9) :start 1 :end 8)))

(test splice-list
  (is (equal '() (splice-seq '())))
  (is (equal '() (splice-seq '(1 2 3))))
  (is (equal '() (splice-seq '(1 2 3) :new '())))
  (flet ((frob (expected &rest args)
           (let* ((list '(1 2 3 4 5 6 7 8 9))
                  (new '(a b c))
                  (actual (apply #'splice-seq list :new new args)))
             (is (not (eq list actual)))
             (is (equal expected actual)))))
    (frob '(a b c))
    (frob '(a b c 1 2 3 4 5 6 7 8 9) :end 0)
    (frob '(1 2 3 4 5 6 7 8 9 a b c) :start 9)
    (frob '(1 a b c 2 3 4 5 6 7 8 9) :start 1 :end 1)
    (frob '(1 2 3 4 5 6 7 8 a b c 9) :start 8 :end 8)
    (frob '(a b c 4 5 6 7 8 9) :end 3)
    (frob '(1 2 3 a b c 7 8 9) :start 3 :end 6)
    (frob '(1 2 3 4 5 6 a b c) :start 6)
    (frob '(1 a b c 9) :start 1 :end 8)))

(test nsplice-list
  (is (equal '() (nsplice-seq '())))
  (is (equal '() (nsplice-seq (list 1 2 3))))
  (is (equal '() (nsplice-seq (list 1 2 3) :new '())))
  (flet ((frob (expected &rest args)
           (let ((list (copy-list '(1 2 3 4 5 6 7 8 9)))
                 (new '(a b c)))
             (let ((actual (apply #'nsplice-seq list :new (copy-list new) args)))
               (is (equal expected actual))))))
    (frob '(a b c))
    (frob '(a b c 1 2 3 4 5 6 7 8 9) :end 0)
    (frob '(1 2 3 4 5 6 7 8 9 a b c) :start 9)
    (frob '(1 a b c 2 3 4 5 6 7 8 9) :start 1 :end 1)
    (frob '(a b c 4 5 6 7 8 9) :end 3)
    (frob '(1 2 3 a b c 7 8 9) :start 3 :end 6)
    (frob '(1 2 3 4 5 6 a b c) :start 6)
    (frob '(1 a b c 9) :start 1 :end 8)))

(test splice-mixed
  (labels ((frob (expected &rest args)
             (apply #'%frob 'list 'vector expected args)
             (apply #'%frob 'vector 'list expected args))
           (%frob (type-1 type-2 expected &rest args)
             (let* ((seq '(1 2 3 4 5 6 7 8 9))
                    (new '(a b c))
                    (actual (apply #'splice-seq (coerce (copy-seq seq) type-1)
                                   :new (coerce new type-2) args)))
               (is (typep actual type-1))
               (is (seq= (coerce expected type-1) actual)))))
    (frob '(a b c))
    (frob '(a b c 1 2 3 4 5 6 7 8 9) :end 0)
    (frob '(1 2 3 4 5 6 7 8 9 a b c) :start 9)
    (frob '(1 a b c 2 3 4 5 6 7 8 9) :start 1 :end 1)
    (frob '(1 2 3 4 5 6 7 8 a b c 9) :start 8 :end 8)
    (frob '(a b c 4 5 6 7 8 9) :end 3)
    (frob '(1 2 3 a b c 7 8 9) :start 3 :end 6)
    (frob '(1 2 3 4 5 6 a b c) :start 6)
    (frob '(1 a b c 9) :start 1 :end 8)))

(test nsplice-mixed
  (labels ((frob (expected &rest args)
             (apply #'%frob 'list 'vector expected args)
             (apply #'%frob 'vector 'list expected args))
           (%frob (type-1 type-2 expected &rest args)
             (let* ((seq '(1 2 3 4 5 6 7 8 9))
                    (new '(a b c))
                    (actual (apply #'nsplice-seq (coerce (copy-seq seq) type-1)
                                   :new (coerce new type-2) args)))
               (is (typep actual type-1))
               (is (seq= (coerce expected type-1) actual)))))
    (frob '(a b c))
    (frob '(a b c 1 2 3 4 5 6 7 8 9) :end 0)
    (frob '(1 2 3 4 5 6 7 8 9 a b c) :start 9)
    (frob '(1 a b c 2 3 4 5 6 7 8 9) :start 1 :end 1)
    (frob '(1 2 3 4 5 6 7 8 a b c 9) :start 8 :end 8)
    (frob '(a b c 4 5 6 7 8 9) :end 3)
    (frob '(1 2 3 a b c 7 8 9) :start 3 :end 6)
    (frob '(1 2 3 4 5 6 a b c) :start 6)
    (frob '(1 a b c 9) :start 1 :end 8)))

(test splice-list-regression
  (is (seq= (splice-seq '(1 2 3 4 5) :start 1 :end 3)
            (nsplice-seq (list 1 2 3 4 5) :start 1 :end 3)
            '(1 4 5))))

(test splice-vector-regression
  (is (seq= (nsplice-seq (vector 1 2 3 4 5) :start 1 :end 3)
            (nsplice-seq (vect 1 2 3 4 5) :start 1 :end 3)
            (splice-seq #(1 2 3 4 5) :start 1 :end 3)
            (splice-seq (vect 1 2 3 4 5) :start 1 :end 3)
            '(1 4 5))))

(defun naive-splice (v &key new (start 0) (end (length v)))
  (assert (>= end start))
  (concatenate 'vector
               (subseq v 0 start)
               new
               (subseq v end)))

(test splice-seq-generative
  (for-all ((vec (lambda () (range 100)))
            (i (lambda () (random 100)))
            (j (lambda () (random 100)))
            (new (lambda ()
                   ;; Makes it easier to tell where the splice begins
                   ;; and ends.
                   (nreverse (range (random 100))))))
    (local
      (when (> i j)
        (rotatef i j))

      (def naive
        (naive-splice vec :new new
                          :start i
                          :end j))

      (def spliced-vec
        (splice-seq vec :new new
                        :start i
                        :end j))
      (is (seq= naive spliced-vec))

      (def nspliced-vec
        (nsplice-seq (copy-seq vec)
                     :new new
                     :start i
                     :end j))
      (is (seq= naive nspliced-vec))

      (def list (coerce vec 'list))

      (def spliced-list
        (splice-seq list :new new
                         :start i
                         :end j))
      (is (seq= naive spliced-list))

      (def nspliced-list
        (nsplice-seq (copy-seq list) :new new
                                     :start i
                                     :end j))
      (is (seq= naive nspliced-list)))))

(test longer
  (let ((x '(1))
        (y '())
        (z '(3)))
    (is (eql x (longer x y)))
    (is (eql z (longer z y)))
    (is (eql x (longer x z)))))

(test shortest
  (let ((x '(1))
        (y '())
        (z '(3))
        (a '()))
    (is (eql (shortest (list x y z a)) y))
    (is (eql (shortest (list a x y z)) a))))

(test long-short-generative
  (for-all ((seqs
             (lambda ()
               (loop for i below (random 20)
                     for seq = (range (random 20))
                     if (zerop (random 2))
                       collect seq
                     else collect (coerce seq 'list)))))
    (is (eql (shortest seqs)
             (extremum seqs #'<= :key #'length)))
    (is (eql (longest seqs)
             (extremum seqs #'>= :key #'length)))
    (loop for seq1 in seqs
          for seq2 in (rest seqs)
          do (progn)
             (is (eql (shorter seq1 seq2)
                      (extremum (list seq1 seq2) #'<= :key #'length)))
             (is (eql (longer seq1 seq2)
                      (extremum (list seq1 seq2) #'>= :key #'length))))
    (let* ((sorted (sort-new seqs #'>= :key #'length))
           (rev (reverse sorted)))
      (is (every #'length>= sorted (drop 1 sorted)))
      (is (every #'length<= rev (drop 1 rev))))))

(test null-if-empty
  (is (null (null-if-empty nil)))
  (is (null (null-if-empty #())))
  (is (null (null-if-empty (eseq))))
  (is (null (null-if-empty "")))
  (is (null (null-if-empty (make-octet-vector 0))))
  (is (null (null-if-empty (make-array 0 :element-type 'bit))))
  (is (null (null-if-empty (make-array '(0 0)))))
  (is (null (null-if-empty (make-array 10 :fill-pointer 0))))
  (is (equal '(1) (null-if-empty '(1))))
  (is (null (null-if-empty (make-array '(0 0)))))
  (is (null (null-if-empty (make-hash-table))))
  (is (null (null-if-empty (dict))))
  (is (null-if-empty (dict :x 1))))
