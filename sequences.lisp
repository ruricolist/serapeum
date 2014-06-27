(in-package :serapeum)

(export '(keep filter
          partition partitions
          runs batches assort
          single
          frequencies
          scan
          nub
          gcp gcs
          length< length<= length> length>=
          longer longest
          ordering
          bestn
          extrema
          nsubseq
          slice
          vector=
          take drop
          halves
          dsu-sort))

(export '(split-sequence:split-sequence
          split-sequence:split-sequence-if
          split-sequence:split-sequence-if-not))

(defun nsubseq (seq start &optional end)
  "Return a subsequence that may share structure with SEQ.

Note that `nsubseq' gets its aposematic leading `n' not because it is
itself destructive, but because, unlike `subseq', destructive
operations on the subsequence returned may mutate the original.

`nsubseq' also works with `setf', with the same behavior as
`replace'."
  (etypecase seq
    (list
     (cond (end (subseq seq start end))
           ((= start 0) seq)
           (t (nthcdr start seq))))
    (vector
     (let* ((len (length seq))
            (end (or end len)))
       (if (and (= start 0) (= end len))
           seq
           ;; TODO Would it be better to undisplace the vector first?
           (make-array (- end start)
                       :element-type (array-element-type seq)
                       :displaced-to seq
                       :displaced-index-offset start))))
    (t (let ((end (length seq)))
         (if (and (= start 0)
                  (or (no end)
                      (= end (length seq))))
             seq
             (subseq seq start end))))))

(defun (setf nsubseq) (value seq start &optional end)
  "Destructively set SEQ between START and END to VALUE.
Uses `replace' internally."
  (replace seq value :start1 start :end1 end)
  value)

(defun counted-filter (pred key seq count reversed?)
  "Helper for FILTER."
  (fbind (pred key)
    (declare (dynamic-extent #'pred #'key))
    (let ((ret '()))
      (block nil
        (map nil
             (lambda (item)
               (when (pred (key item))
                 (push item ret)
                 (when (zerop (decf count))
                   (return))))
             seq))
      (if reversed?
          ret
          (nreverse ret)))))

(defun filter (pred seq &rest args &key count from-end (start 0) end
                                        (key #'identity)
                        &allow-other-keys)
  "Almost the opposite of `remove-if-not'.
The difference is the handling of COUNT."
  (if (null count)
      (apply #'remove-if-not pred seq args)
      (let* ((count (max 0 count))      ;COUNT can be negative.
             (seq (if from-end (reverse seq) seq))
             (items (counted-filter pred
                                    key
                                    (nsubseq seq start end)
                                    count
                                    from-end)))
        (if (listp seq)
            items
            ;; Return a sequence that is "like" the original.
            (replace (subseq seq 0 (length items)) items)))))

(assert (equal '(0 2 4 6 8) (filter #'evenp (iota 100) :count 5)))
(assert (equal '(90 92 94 96 98)
               (filter #'evenp (iota 100) :count 5 :from-end t)))

(define-compiler-macro filter (&whole decline
                                      pred seq
                                      &rest args
                                      &key count
                                      &allow-other-keys)
  "Expand to `remove-if-not' if there is no COUNT."
  (if (null count)
      `(remove-if-not ,pred ,seq ,@args)
      decline))

(defun keep (item seq &rest args &key (test #'eql) from-end count
                      &allow-other-keys)
  "Almost the opposite of `remove'.
Keep only those items in SEQ that are equivalent, under TEST and KEY,
to ITEM.

The difference is the handling of COUNT."
  (let* ((args (remove-from-plist args :test :count))
         (seq (apply #'remove item seq :test-not test args)))
    (if (null count)
        seq
        (apply #'filter (curry test item) seq
               :count count :from-end from-end
               args))))

(assert (equal '((a 1) (a 2))
               (keep 'a '((a 1) (b) (c) (a 2) (a 3) (b) (c) (a 4) (a 5))
                     :count 2 :key #'car)))
(assert (equal '((a 4) (a 5))
               (keep 'a '((a 1) (b) (c) (a 2) (a 3) (b) (c) (a 4) (a 5))
                     :count 2 :key #'car :from-end t)))

(define-compiler-macro keep (&whole decline
                                    item seq
                                    &rest args
                                    &key (test '#'eql) count
                                    &allow-other-keys)
  "Expand to `remove' if there is no COUNT."
  (if (null count)
      `(remove ,item ,seq :test-not ,test ,@(remove-from-plist args :test))
      decline))

(defsubst single (seq)
  "Is SEQ a sequence of one element?"
  (etypecase seq
    (list (and seq (null (cdr seq))))
    (sequence (= (length seq) 1))))

(defun partition (pred seq &key (start 0) end (key #'identity))
  "Partition elements of SEQ into those for which PRED returns true
and false.

Return two values, one with each sequence.

Exactly equivalent to:
     (values (remove-if predicate seq) (remove-if-not predicate seq))
except it visits each element only once.

Note that `partition` is not just `assort` with an up-or-down
predicate. `assort` returns its groupings in the order they occur in
the sequence; `partition` always returns the “true” elements first.

    (assort '(1 2 3) :key #'evenp) => ((1 3) (2))
    (partition #'evenp '(1 2 3)) => (2), (1 3)"
  (fbind ((test (compose pred key)))
    (etypecase seq
      (list
       (setf seq (nthcdr start seq))
       (let ((pass (queue))
             (fail (queue)))
         (flet ((pass/fail (item)
                  (if (test item)
                      (enq item pass)
                      (enq item fail))))
           (declare (dynamic-extent #'pass/fail))
           (if (null end)
               (dolist (item seq)
                 (pass/fail item))
               (loop for i from 0 below (- end start)
                     for item in seq
                     do (pass/fail item))))
         (values (qlist pass) (qlist fail))))
      (vector
       (let* ((pass (make-array 0
                                :element-type (array-element-type seq)
                                :adjustable t
                                :fill-pointer 0))
              (fail (copy-array pass)))
         (loop for i from start below (or end (length seq))
               for item = (aref seq i)
               do (if (test item)
                      (vector-push-extend item pass)
                      (vector-push-extend item fail)))
         (values pass fail)))
      (t (values (remove-if pred seq :key key)
                 (remove-if-not pred seq :key key))))))

(defun partitions (preds seq &key (start 0) end (key #'identity))
  "Generalized version of PARTITION.

PREDS is a list of predicates. For each predicate, `partitions'
returns a filtered copy of SEQ. As a second value, it returns an extra
sequence of the items that do not match any predicate.

Items are assigned to the first predicate they match."
  (etypecase seq
    (list
     (fbind key
       (setf seq (nthcdr start seq))
       (let ((buckets (loop for nil in preds collect (queue)))
             (extra (queue)))
         (flet ((bucket (item)
                  (loop for pred in preds
                        for bucket in buckets
                        if (funcall pred (key item))
                          return (enq item bucket)
                        finally (enq item extra))))
           (declare (dynamic-extent #'bucket))
           (if (no end)
               (mapc #'bucket seq)
               (loop for i from start below end
                     for item in seq
                     do (bucket item)))
           (values (mapcar #'qlist buckets)
                   (qlist extra))))))
    (vector
     (fbind ((key key)
             (make-buffer
              (lambda ()
                (make-array 0
                            :element-type (array-element-type seq)
                            :adjustable t
                            :fill-pointer 0))))
       (let ((buckets (loop for nil in preds collect (make-buffer)))
             (extra (make-buffer)))
         (loop for item across seq do
           (loop for pred in preds
                 for bucket in buckets
                 if (funcall pred (key item))
                   return (vector-push-extend item bucket)
                 finally (vector-push-extend item extra)))
         (values buckets extra))))
    (sequence
     (loop for pred in preds
           collect (remove-if-not pred seq
                                  :start start
                                  :end end
                                  :key key)))))

(assert (equal (partitions (list #'oddp #'evenp) '(0 1 2 3 4 5 6 7 8 9))
               '((1 3 5 7 9) (0 2 4 6 8))))

(defun list-assort (list &key (key #'identity) (test #'eql) (start 0) end)
  (declare (list list) (optimize speed))
  (fbind ((key key) (test test))
    (let ((list (nthcdr start list))
          (groups (queue)))
      (flet ((group-item (item)
               (if-let ((group
                         (let ((kitem (key item)))
                           (find-if
                            (lambda (group)
                              (test kitem (key (front group))))
                            (qlist groups)))))
                 (enq item group)
                 (enq (queue item) groups))))
        (if (no end)
            (dolist (item list)
              (group-item item))
            (loop for item in list
                  for i downfrom end above 0
                  do (group-item item))))
      (mapcar #'qlist (qlist groups)))))

(assert (equal (list-assort (iota 10)
                            :key (lambda (x)
                                   (mod x 3)))
               '((0 3 6 9) (1 4 7) (2 5 8))))

(defun vector-assort (vec &key (key #'identity) (test #'eql) (start 0) end)
  (let* ((end (or end (length vec)))
         (element-type (array-element-type vec))
         (groups (queue)))
    (fbind ((key key) (test test))
      (loop for i from start below end
            for item = (aref vec i)
            do (if-let ((group
                         (let ((kitem (key item)))
                           (find-if
                            (lambda (group)
                              (test kitem (key (aref group (1- (length group))))))
                            (qlist groups)))))
                 (vector-push-extend item group)
                 (enq (make-array 1
                                  :element-type element-type
                                  :adjustable t
                                  :fill-pointer 1
                                  :initial-contents (list item))
                      groups))))
    (qlist groups)))

(assert (equal (vector-assort "How Now Brown Cow" :key #'upper-case-p)
               '("HNBC" "ow ow rown ow")))

(defun assort (seq &key (key #'identity) (test #'eql) (start 0) end)
  "Return SEQ assorted by KEY.

     (assort (iota 10)
             :key (lambda (n) (mod n 3)))
     => '((0 3 6 9) (1 4 7) (2 5 8))

You can think of `assort' as being akin to `remove-duplicates':

     (mapcar #'first (assort list))
     ≡ (remove-duplicates list :from-end t)"
  (etypecase seq
    (list (list-assort seq :key key :test test :start start :end end))
    (vector (vector-assort seq :key key :test test :start start :end end))
    ;; Is there a more efficient way to do this while remaining
    ;; completely generic?
    (t (let* ((seq (nsubseq seq start end))
              (keys (nub (map 'list key seq) :test test)))
         (loop for k in keys
               collect (keep k seq :key key :test test))))))

(defun list-runs (list start end key test)
  (fbind ((test (key-test key test)))
    (declare (dynamic-extent #'test))
    (let ((runs
            (reduce
             (lambda (runs y)
               (if (null runs)
                   (list (list y))
                   (let ((x (caar runs)))
                     (if (test x y)
                         (cons (cons y (car runs))
                               (cdr runs))
                         (list* (list y)
                                (nreverse (car runs))
                                (cdr runs))))))
             list
             :start start
             :end end
             :initial-value nil)))
      (nreverse (cons (nreverse (car runs)) (cdr runs))))))

(defun seq-runs (seq start end key test)
  (fbind ((test (key-test key test)))
    (declare (dynamic-extent #'test))
    (collecting*
      (nlet runs ((start start))
        (let* ((elt (elt seq start))
               (pos (position-if-not (curry #'test elt)
                                     seq
                                     :start (1+ start)
                                     :end end)))
          (if (null pos)
              (collect (subseq seq start end))
              (progn
                (collect (subseq seq start pos))
                (runs pos))))))))

(defun runs (seq &key (start 0) end (key #'identity) (test #'eql))
  "Return a list of runs of similar elements in SEQ.
The arguments START, END, and KEY are as for `reduce'.

    (runs '(head tail head head tail))
    => '((head) (tail) (head head) (tail))"
  (if (emptyp seq)
      (list seq)
      (etypecase seq
        (list (list-runs seq start end key test))
        (sequence (seq-runs seq start end key test)))))

(assert (equal '((1 2) (3 4 5 6 11 12 13))
               (runs '(1 2 3 4 5 6 11 12 13) :key (rcurry #'< 3))))

(defun batches (seq n &key (start 0) end)
  "Return SEQ in batches of N elements.

    (batches (iota 11) 2)
    => ((0 1) (2 3) (4 5) (6 7) (8 9) (10))"
  (check-type n (integer 0 *))
  (etypecase seq
    (list
     (let ((seq (nthcdr start seq)))
       (if (null end)
           (loop while seq
                 collect (loop for i below n
                               for (elt . rest) on seq
                               collect elt
                               finally (setf seq rest)))
           (loop while seq
                 for i from start below end by n
                 collect (loop for i below (min n (- end i))
                               for (elt . rest) on seq
                               collect elt
                               finally (setf seq rest))))))
    (sequence
     (let ((end (or end (length seq))))
       (nlet batches ((i start)
                      (acc '()))
         (if (>= i end)
             (nreverse acc)
             (batches (+ i n)
                      (cons (subseq seq i (min (+ i n) end))
                            acc))))))))

(assert (equal '((a b) (c d) (e)) (batches '(a b c d e) 2)))
(assert (equal '("ab" "cd" "e") (batches "abcde" 2)))
(assert (equal '("a") (batches "abc" 2 :end 1)))
(assert (equal '((a)) (batches '(a b c) 2 :end 1)))

(defun frequencies (seq &rest hash-table-args)
  "Return a hash table with the count of each unique item in SEQ.

From Clojure."
  (lret ((table (multiple-value-call #'make-hash-table
                  (values-list hash-table-args)
                  :size (values (floor (length seq) 2))
                  :test 'equal)))
    (map nil
         (lambda (elt)
           (incf (gethash elt table 0)))
         seq)))

(defun scan (fn seq)
  "A version of `reduce' that shows its work.

Instead of returning just the final result, `scan' returns a list of
the successive results at each step.

    (reduce #'+ '(1 2 3 4))
    => 10

    (scan #'+ '(1 2 3 4))
    => '(1 3 6 10)

From APL and descendants."
  (fbind fn
    (nreverse
     (the list
          (reduce (lambda (acc x)
                    (cons (fn x (car acc)) acc))
                  (nsubseq seq 1)
                  :initial-value (list (elt seq 0)))))))

(defsubst nub (seq &rest args &key start end key (test #'equal))
  "Remove duplicates from SEQ, starting from the end.
TEST defaults to `equal'.

From Haskell."
  (declare (ignore start end key))
  (apply #'remove-duplicates seq :from-end t :test test args))

(defun gcp (seqs &key (test #'eql))
  "The greatest common prefix of SEQS."
  (let ((test (ensure-function test)))
    (labels ((gcp (x y)
               (let ((miss (mismatch x y :test test)))
                 (cond ((not miss) x)
                       ((> miss 0) (subseq x 0 miss))
                       (t nil)))))
      (block nil
        (reduce
         (lambda (x y)
           (or (gcp x y)
               (return)))
         seqs)))))

(assert (equal (gcp '("miss" "molly")) "m"))

(defun gcs (seqs &key (test #'eql))
  "The greatest common suffix of SEQS."
  (let ((test (ensure-function test)))
    (labels ((gcs (x y)
               (let ((miss (mismatch x y :from-end t :test test)))
                 (cond ((not miss) x)
                       ((< miss (length x))
                        (subseq x miss))
                       (t nil)))))
      (block nil
        (reduce
         (lambda (x y)
           (or (gcs x y)
               (return)))
         seqs)))))

(assert (equal (gcs '("how" "now")) "ow"))

(defun as-len ()
  "Return a closure that returns the length of the length
designators (sequences or integers) it is called with.

The idea is to minimize needless traversal of lists in length< and
length> by tracking the least length seen so far."
  (let ((min array-dimension-limit))
    (lambda (x)
      (etypecase x
        (array-length
         (minf min x)
         x)
        (list
         (loop for nil in x
               for i from 1
               repeat (1+ min)
               finally (minf min i)
                       (return i)))
        (sequence
         (let ((x (length x)))
           (minf min x)
           x))))))

(defun length< (&rest seqs)
  "Is each length-designator in SEQS shorter than the next?
A length designator may be a sequence or an integer."
  (nlet rec ((last 0)
             (seqs seqs))
    (if (endp seqs)
        t
        (destructuring-bind (seq . seqs) seqs
          (etypecase seq
            (array-length
             (when (< last seq)
               (rec seq seqs)))
            (list
             (when-let (tail (nthcdr last seq))
               (rec (+ last (length tail)) seqs)))
            (sequence
             (let ((len (length seq)))
               (when (< last len)
                 (rec len seqs)))))))))

(assert (length< '(1) 2))
(assert (not (length< '(1 2) 2)))
(assert (not (length< '(1 2 3) 2)))

(defun length> (&rest seqs)
  "Is each length-designator in SEQS longer than the next?
A length designator may be a sequence or an integer."
  (nlet rec ((last most-positive-fixnum)
             (seqs seqs))
    (if (endp seqs)
        t
        (destructuring-bind (seq . seqs) seqs
            (etypecase seq
              (array-length
               (when (> last seq)
                 (rec seq seqs)))
              (list
               (let ((len
                       ;; Get the length of SEQ, but only up to LAST.
                       (loop for nil in seq
                             for i from 1
                             repeat last
                             finally (return i))))
                 (when (> last len)
                   (rec len seqs))))
              (sequence
               (let ((len (length seq)))
                 (when (> last len)
                   (rec len seqs)))))))))

(assert (not (length> '(1) 2)))
(assert (not (length> '(1 2) 2)))
(assert (length> '(1 2 3) 2))

(defun length>= (&rest seqs)
  "Is each length-designator in SEQS longer or as long as the next?
A length designator may be a sequence or an integer."
  (not (apply #'length< seqs)))

(defun length<= (&rest seqs)
  "Is each length-designator in SEQS as long or shorter than the next?
A length designator may be a sequence or an integer."
  (not (apply #'length> seqs)))

(defun longer (x y)
  "Return the longer of X and Y.

If X and Y are of equal length, return X."
  (check-type x sequence)
  (check-type y sequence)
  (cond ((and (listp x) (listp y))
         (nlet longer ((xs x)
                       (ys y))
           (cond ((and (endp xs) (endp ys)) x)
                 ((endp ys) x)
                 ((endp xs) y)
                 (t (longer (rest xs) (rest ys))))))
        ((listp x)
         (if (length> x (length y))
             x
             y))
        ((listp y)
         (if (length> y (length x))
             y
             x))
        ((< (length x) (length y)) y)
        (t x)))

(defun longest (seqs)
  "Return the longest seq in SEQS."
  (reduce #'longer seqs))

(defun shortest (seqs)
  "Return the shortest seq in SEQS."
  (flet ((shorter (x y)
           (select (longer x y)
             (x y)
             (y x))))
    (reduce #'shorter seqs)))

(defun slice-bounds (seq start end)
  "Normalize START and END, which may be negative, to offsets
acceptable to SUBSEQ."
  (values (if (minusp start)
              (+ (length seq) start)
              start)
          (if (null end)
              nil
              (if (minusp end)
                  (+ (length seq) end)
                  end))))

(defun slice (seq start &optional (end (length seq)))
  "Like `subseq', but allows negative bounds to specify offsets.
Both START and END accept negative bounds.

     (slice \"string\" -3 -1) => \"in\"

Setf of `slice' is like setf of `ldb': afterwards, the place being set
holds a new sequence which is not EQ to the old."
  (multiple-value-bind (start end)
      (slice-bounds seq start end)
    (subseq seq start end)))

(defun setslice (seq1 seq2 start &optional end)
  "Helper to `set' a slice non-destructively."
  (multiple-value-bind (start end)
      (slice-bounds seq1 start end)
    (replace (copy-seq seq1) seq2 :start1 start :end1 end)))

(define-setf-expander slice (sequence start &optional end
                                            &environment env)
  (multiple-value-bind (temps vals stores setter getter)
      (get-setf-expansion sequence env)
    (when (rest stores)
      (error "Can't expand"))
    (let ((stemp (first stores)))
      (with-gensyms (s e store)
        (values (list* s e temps)
                (list* start end vals)
                (list store)
                `(let ((,stemp (setslice ,getter ,store ,s ,e)))
                   ,setter
                   ,store)
                `(slice ,getter ,s ,e))))))

(assert (equal "in" (slice "string" -3 -1)))
(assert (equal "foo" (slice "foo" -0)))
(assert (equal "r" (slice "bar" -1)))

(defun ordering (seq &key unordered-to-end
                          from-end
                          (test 'eql))
  "Given a sequence, return a function that, when called with `sort',
restores the original order of the sequence.

That is, for any SEQ (without duplicates), it is always true that

     (equal seq (sort (shuffle (copy-seq seq)) (ordering seq)))

FROM-END controls what to do in case of duplicates. If FROM-END is
true, the last occurrence of each item is preserved; otherwise, only
the first occurrence counts.

TEST controls identity; it should be a valid test for a hash table.

UNORDERED-TO-END controls where to sort items that are not present in
the original ordering. By default they are sorted first but, if
UNORDERED-TO-END is true, they are sorted last. In either case, they
are left in no particular order."
  ;; NB `finc' is not available yet.
  (let ((table (make-hash-table :test test))
        (i -1))
    (map nil
         (if from-end
             (lambda (item)
               (setf (gethash item table) (incf i)))
             (lambda (item)
               (ensure-gethash item table (incf i))))
         seq)

    (let ((default
            (if unordered-to-end
                (1+ i)
                -1)))
      (lambda (x y)
        (< (gethash x table default)
           (gethash y table default))))))

(dotimes (i 100)
  (assert (let ((list (shuffle (loop for i to 1000 collect i))))
            (equal list
                   (sort (shuffle (copy-list list))
                         (ordering list))))))

(defsubst take (n seq)
  "Return the first N elements of SEQ, as a *new* sequence of the same
type as SEQ."
  (subseq seq 0 n))

(defsubst drop (n seq)
  "Return all but the first N elements of SEQ.
The sequence returned is a new sequence of the same type as SEQ."
  (subseq seq n))

(defsubst take-while (pred seq)
  (etypecase seq
    (list
     (ldiff seq (member-if-not pred seq)))
    (sequence
     (subseq seq 0 (position-if-not pred seq)))))

(defsubst drop-while (pred seq)
  (etypecase seq
    (list
     (member-if-not pred seq))
    (sequence
     (subseq seq (position-if-not pred seq)))))

(defsubst count-while (pred seq)
  (position-if-not pred seq))

;;;# `bestn'
(defun bisect-left (vec item pred &key key)
  "Return the index in VEC to insert ITEM and keep VEC sorted."
  (declare ((simple-array * (*)) vec))
  (fbind ((pred pred) (key key))
    (let ((start 0)
          (end (length vec)))
      (declare (array-length start end))
      (let ((kitem (key item)))
        (loop while (< start end) do
          (let ((mid (floor (+ start end) 2)))
            (if (pred (key (svref vec mid)) kitem)
                (setf start (1+ mid))
                (setf end mid)))
              finally (return start))))))

;;; The heap implementation is borrowed from Zach Beane's timers
;;; package for SBCL.

;;; TODO Allow the heap to have variable arity?
;;; http://www.pvk.ca/Blog/2014/04/13/number-systems-for-implicit-data-structures/

(defsubst heap-parent (i)
  (ash (1- i) -1))

(defsubst heap-left (i)
  (1+ (ash i 1)))

(defsubst heap-right (i)
  (+ 2 (ash i 1)))

(defun heapify (heap start &key (key #'identity) (test #'>=))
  (declare (function key test))
  (fbind (key (ge test))
    (let ((l (heap-left start))
          (r (heap-right start))
          (size (length heap))
          largest)
      (setf largest (if (and (< l size)
                             (not (ge (key (aref heap start))
                                      (key (aref heap l)))))
                        l
                        start))
      (when (and (< r size)
                 (not (ge (key (aref heap largest))
                          (key (aref heap r)))))
        (setf largest r))
      (when (/= largest start)
        (rotatef (aref heap largest) (aref heap start))
        (heapify heap largest :key key :test test)))
    heap))

(defun heap-insert (heap new-item &key (key #'identity) (test #'>=))
  (declare (function key test))
  (fbind (key (ge test))
    (vector-push-extend nil heap)
    (loop for i = (1- (length heap)) then parent-i
          for parent-i = (heap-parent i)
          while (and (> i 0)
                     (not (ge (key (aref heap parent-i))
                              (key new-item))))
          do (setf (aref heap i) (aref heap parent-i))
          finally (setf (aref heap i) new-item)
                  (return-from heap-insert i))))

(defun heap-maximum (heap)
  (unless (zerop (length heap))
    (aref heap 0)))

(defun heap-extract (heap i &key (key #'identity) (test #'>=))
  (unless (> (length heap) i)
    (error "Heap underflow"))
  (prog1
      (aref heap i)
    (setf (aref heap i) (aref heap (1- (length heap))))
    (decf (fill-pointer heap))
    (heapify heap i :key key :test test)))

(defun heap-extract-maximum (heap &key (key #'identity) (test #'>=))
  (heap-extract heap 0 :key key :test test))

(defun heap-extract-all (heap &key (key #'identity) (test #'>=))
  (loop while (> (length heap) 0)
        collect (heap-extract-maximum heap :key key :test test)))

(defun make-heap (&optional (size 100))
  (make-array size :adjustable t :fill-pointer 0))

(defun coerce-by-example (seq1 seq2)
  (typecase seq2
    (list (coerce seq1 'list))
    (vector (coerce seq1 'vector))
    (t (replace (subseq seq2 0 (length seq1)) seq1))))

(defun bestn (n seq pred &key (key #'identity) memo)
  "Partial sorting.
Equivalent to (firstn N (sort SEQ PRED)), but much faster, at least
for small values of N.

The name is from Arc."
  (declare (array-length n) (optimize speed))
  (cond ((= n 0)
         (coerce-by-example '() seq))
        ((= n 1)
         (coerce-by-example (list (extremum seq pred :key key)) seq))
        ((length<= seq n)
         (sort (copy-seq seq) pred :key key))
        (t (fbind ((key (if memo
                            (let ((dict (dict))
                                  (key (ensure-function key)))
                              (lambda (&rest args)
                                (ensure2 (gethash args dict)
                                  (apply key args))))
                            key))
                   (test (complement pred)))
             (let ((heap (make-heap n))
                   (i 0))
               (declare (array heap) (array-length i))
               (map nil
                    (lambda (elt)
                      (cond ((< i n)
                             (heap-insert heap elt :key key :test #'test))
                            ((test (key (heap-maximum heap)) (key elt))
                             (heap-extract-maximum heap :key key
                                                        :test #'test)
                             (heap-insert heap elt :key key :test #'test)))
                      (incf i))
                    seq)
               (coerce-by-example
                (take n (nreverse (heap-extract-all heap :key key :test #'test)))
                seq))))))

(dotimes (i 100)
  (let ((list (map-into (make-list 1000) (lambda () (random 1000)))))
    (assert
     (equal (firstn 20 (sort (copy-list list) #'>))
            (bestn 20 list #'>)))))
(dotimes (i 100)
  (let ((list (map-into (make-list 1000) (lambda () (random 1000)))))
    (assert
     (equal (firstn 20 (sort (copy-list list) #'string> :key #'princ-to-string))
            (bestn 20 list #'string> :key #'princ-to-string)))))

(defun extrema (seq pred &key (key #'identity) (start 0) end)
  "Like EXTREMUM, but returns both the minimum and the maximum (as two
values).

     (extremum (iota 10) #'>) => 9
     (extrema (iota 10) #'>) => 9, 0"
  (let (min max kmin kmax (init t))
    (flet ((update-extrema (x)
             (if init
                 (setf min x max x
                       kmin (funcall key x)
                       kmax kmin
                       init nil)
                 (let ((kx (funcall key x)))
                   (cond ((funcall pred kx kmin)
                          (setf kmin kx min x))
                         ((funcall pred kmax kx)
                          (setf kmax kx max x)))))))
      (declare (dynamic-extent #'update-extrema))
      (map nil #'update-extrema
           (nsubseq seq start end)))
    (values min max)))

(assert (equal (multiple-value-list (extrema '(1 2 3 4 5) #'<)) '(1 5)))

(defun vector= (v1 v2 &key (test #'eql)
                           (start1 0)
                           (end1 nil)
                           (start2 0)
                           (end2 nil))
  "Like `string=' for any vector."
  (declare (vector v1 v2))
  (let ((end1 (or end1 (length v1)))
        (end2 (or end2 (length v2))))
    (and (= (- end1 start1)
            (- end2 start2))
         (loop for i from start1 below end1
               for j from start2 below end2
               always (funcall test (aref v1 i) (aref v2 j))))))

(defun halves (seq &optional split)
  "Return, as two values, the first and second halves of SEQ.
SPLIT designates where to split SEQ; it defaults to half the length,
but can be specified.

If SEQ is of an odd length, the split is made using `ceiling' rather
than `truncate'. This is on the theory that, if SEQ is a
single-element list, it should be returned unchanged."
  (if (and split (listp seq))
      ;; If we know where to split in advance we only have to traverse
      ;; the list once.
      (loop for i below split
            for (x . right) on seq
            collect x into left
            finally (return (values left right)))
      (let ((split (or split
                       (let ((len (length seq)))
                         (if (evenp len)
                             (truncate len 2)
                             (ceiling len 2))))))
        (if (listp split)
            (halves seq split)
            (values
             (subseq seq 0 split)
             (subseq seq split))))))

(assert (equal (halves '(x)) '(x)))
(assert (equal (multiple-value-list (halves '(x y))) '((x) (y))))
(assert (equal (multiple-value-list (halves '(x y z))) '((x y) (z))))

(defun dsu-sort (seq fn &key (key #'identity))
  "Decorate-sort-undecorate using KEY.
Useful when KEY is an expensive function (e.g. database access)."
  (fbind (key)
    (map-into seq
              #'cdr
              ;; Vectors sort faster.
              (sort (map 'vector
                         (lambda (item)
                           (cons (key item) item))
                         seq)
                    fn
                    :key #'car))))
