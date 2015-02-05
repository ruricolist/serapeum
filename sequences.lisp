(in-package :serapeum)

(export '(keep filter
          filterf
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
          take drop
          halves
          dsu-sort
          deltas
          intersperse
          toposort
          inconsistent-graph
          inconsistent-graph-constraints
          mvfold mvfoldr))

(export '(split-sequence:split-sequence
          split-sequence:split-sequence-if
          split-sequence:split-sequence-if-not))

(defun make-sequence-like (seq len &rest args &key initial-element
                                                   (initial-contents nil ic?))
  "Helper function: make a sequence of length LEN having the same type as SEQ."
  (seq-dispatch seq
    (if ic?
        (map 'list #'identity initial-contents)
        (make-list len :initial-element initial-element))
    (apply #'make-array len :element-type (array-element-type seq) args)
    #+(or sbcl abcl) (apply #'sequence:make-sequence-like seq len args)))

(defun map-subseq (fn seq &optional start end from-end)
  "Helper function to map SEQ between START and END."
  (declare (type (or null array-index) start end))
  (when (and start end)
    (assert (<= start end)))
  (let ((start (or start 0)))
    (fbind fn
      (seq-dispatch seq
        (if (no end)
            (if from-end
                (dolist (item (reverse (nthcdr start seq)))
                  (fn item))
                (dolist (item (nthcdr start seq))
                  (fn item)))
            (if from-end
                (let ((subseq '()))
                  (loop for item in (nthcdr start seq)
                        for i below (- end start)
                        do (push item subseq))
                  (mapc #'fn subseq))
                (loop for item in (nthcdr start seq)
                      for i below (- end start)
                      do (fn item))))
        (let ((end (or end (length seq))))
          (if from-end
              (loop for i downfrom (1- end) to start
                    do (fn (aref seq i)))
              (loop for i from start below end
                    do (fn (aref seq i)))))
        (let ((end (or end (length seq))))
          (if from-end
              (loop for i downfrom (1- end) to start
                    do (fn (elt seq i)))
              (loop for i from start below end
                    do (fn (elt seq i)))))))))

;;; Define a protocol for accumulators so we can write functions like
;;; `assort', `partition', &c. generically.

(defgeneric make-bucket (seq &optional init))

(defgeneric bucket-push (seq item bucket))

(defgeneric bucket-seq (seq bucket))

(defgeneric bucket-front (seq bucket))

(defmethod make-bucket ((seq list) &optional (init nil initp))
  (if initp (queue init) (queue)))

(defmethod make-bucket ((seq vector) &optional (init nil initp))
  (if initp
      (make-array 1
                  :element-type (array-element-type seq)
                  :adjustable t
                  :fill-pointer 1
                  :initial-contents (list init))
      (make-array 0
                  :element-type (array-element-type seq)
                  :adjustable t
                  :fill-pointer 0)))

(defmethod make-bucket ((seq sequence) &optional (init nil initp))
  (if initp (make-bucket () init) (make-bucket ())))

(defmethod bucket-push ((seq list) item bucket)
  (enq item bucket))

(defmethod bucket-push ((seq vector) item bucket)
  (vector-push-extend item bucket))

(defmethod bucket-push ((seq sequence) item bucket)
  (bucket-push () item bucket))

(defmethod bucket-seq ((seq list) bucket)
  (qlist bucket))

(defmethod bucket-seq ((seq vector) bucket)
  bucket)

(defmethod bucket-seq ((seq sequence) bucket)
  (let ((len (qlen bucket)))
    (make-sequence-like seq len :initial-contents (qlist bucket))))

(defmethod bucket-front ((seq list) bucket)
  (front bucket))

(defmethod bucket-front ((seq vector) bucket)
  (when (> (length bucket) 0)
    (aref bucket 0)))

(defmethod bucket-front ((seq sequence) bucket)
  (bucket-front () bucket))

(defun nsubseq (seq start &optional end)
  "Return a subsequence that may share structure with SEQ.

Note that `nsubseq' gets its aposematic leading `n' not because it is
itself destructive, but because, unlike `subseq', destructive
operations on the subsequence returned may mutate the original.

`nsubseq' also works with `setf', with the same behavior as
`replace'."
  (seq-dispatch seq
    (cond (end (subseq seq start end))
          ((= start 0) seq)
          (t (nthcdr start seq)))
    (let* ((len (length seq))
           (end (or end len)))
      (if (and (= start 0) (= end len))
          seq
          ;; TODO Would it be better to undisplace the vector first?
          (make-array (- end start)
                      :element-type (array-element-type seq)
                      :displaced-to seq
                      :displaced-index-offset start)))
    (let ((end (length seq)))
      (if (and (= start 0)
               (or (no end)
                   (= end (length seq))))
          seq
          (subseq seq start end)))))

(defun (setf nsubseq) (value seq start &optional end)
  "Destructively set SEQ between START and END to VALUE.
Uses `replace' internally."
  (replace seq value :start1 start :end1 end)
  value)

(defun filter/counted (pred seq &rest args
                       &key count from-end (start 0) end
                            (key #'identity))
  "Helper for FILTER."
  (fbind (pred key)
    (declare (dynamic-extent #'pred #'key))
    (cond
      ;; Simple cases.
      ((= count 0) (make-sequence-like seq 0))
      ((> count (length seq)) (apply #'filter pred seq :count nil args))
      (t (let ((ret (make-bucket seq)))
           (block nil
             (map-subseq (lambda (item)
                           (when (pred (key item))
                             (bucket-push seq item ret)
                             (when (zerop (decf count))
                               (return))))
                         seq start end from-end))
           (let ((seq2 (bucket-seq seq ret)))
             (if from-end (nreverse seq2) seq2)))))))

(defun filter (pred seq &rest args &key count &allow-other-keys)
  "Almost, but not quite, an alias for `remove-if-not'.

The difference is the handling of COUNT: for `filter', COUNT is the
number of items to *keep*, not remove.

     (remove-if-not #'oddp '(1 2 3 4 5) :count 2)
     => '(1 3 5)

     (filter #'oddp '(1 2 3 4 5) :count 2)
     => '(1 3)"
  (if count
      (apply #'filter/counted pred seq args)
      (apply #'remove-if-not pred seq args)))

(define-compiler-macro filter (&whole decline
                                      pred seq
                                      &rest args
                                      &key count
                                      &allow-other-keys)
  "In the absence of COUNT, expand directly to `remove-if-not'."
  (if (null count)
      `(remove-if-not ,pred ,seq ,@args)
      decline))

(declaim (inline filter/swapped-arguments))
(defun filter/swapped-arguments (seq pred &rest args)
  (apply #'filter pred seq args))

(define-modify-macro filterf (pred &rest args)
  filter/swapped-arguments
  "Modify-macro for FILTER.
The place designed by the first argument is set to th result of
calling FILTER with PRED, the place, and ARGS.")

(defun keep (item seq &rest args &key (test #'eql) from-end key count
             &allow-other-keys)
  "Almost, but not quite, an alias for `remove'.

The difference is the handling of COUNT. For `keep', COUNT is the
number of items to *keep*, not remove.

     (remove 'x '(x y x y x y) :count 2)
     => '(y y x y)

     (keep 'x '(x y x y x y) :count 2)
     => '(x x)

`keep' becomes useful with the KEY argument:

     (keep 'x ((x 1) (y 2) (x 3)) :key #'car)
     => '((x 1) (x 3))"
  (declare (ignore from-end key))
  (let ((args (remove-from-plist args :test)))
    (if (null count)
        (apply #'remove item seq :test-not test args)
        (fbind ((test (curry test item)))
          (declare (dynamic-extent #'test))
          (apply #'filter #'test seq :count count args)))))

(define-compiler-macro keep (&whole decline
                                    item seq
                                    &rest args
                                    &key (test '#'eql) count
                                    &allow-other-keys)
  "In the absence of COUNT, expand directly to `remove'."
  (if (null count)
      `(remove ,item ,seq :test-not ,test ,@(remove-from-plist args :test))
      decline))

(defsubst single (seq)
  "Is SEQ a sequence of one element?"
  (seq-dispatch seq
    (and seq (null (cdr seq)))
    (= (length seq) 1)))

(defun partition (pred seq &key (start 0) end (key #'identity))
  "Partition elements of SEQ into those for which PRED returns true
and false.

Return two values, one with each sequence.

Exactly equivalent to:
     (values (remove-if-not predicate seq) (remove-if predicate seq))
except it visits each element only once.

Note that `partition` is not just `assort` with an up-or-down
predicate. `assort` returns its groupings in the order they occur in
the sequence; `partition` always returns the “true” elements first.

    (assort '(1 2 3) :key #'evenp) => ((1 3) (2))
    (partition #'evenp '(1 2 3)) => (2), (1 3)"
  (fbind ((test (compose pred key)))
    (let ((pass (make-bucket seq))
          (fail (make-bucket seq)))
      (map-subseq (lambda (item)
                    (if (test item)
                        (bucket-push seq item pass)
                        (bucket-push seq item fail)))
                  seq start end)
      (values (bucket-seq seq pass) (bucket-seq seq fail)))))

(defun partitions (preds seq &key (start 0) end (key #'identity))
  "Generalized version of PARTITION.

PREDS is a list of predicates. For each predicate, `partitions'
returns a filtered copy of SEQ. As a second value, it returns an extra
sequence of the items that do not match any predicate.

Items are assigned to the first predicate they match."
  (fbind key
    (let ((buckets (loop for nil in preds collect (make-bucket seq)))
          (extra (make-bucket seq)))
      (map-subseq (lambda (item)
                    (loop for pred in preds
                          for bucket in buckets
                          for fn = (ensure-function pred)
                          if (funcall fn (key item))
                            return (bucket-push seq item bucket)
                          finally (bucket-push seq item extra)))
                  seq start end)
      (values (mapcar-into (lambda (bucket)
                             (bucket-seq seq bucket))
                           buckets)
              (bucket-seq seq extra)))))

(defun assort (seq &key (key #'identity) (test #'eql) (start 0) end)
  "Return SEQ assorted by KEY.

     (assort (iota 10)
             :key (lambda (n) (mod n 3)))
     => '((0 3 6 9) (1 4 7) (2 5 8))

You can think of `assort' as being akin to `remove-duplicates':

     (mapcar #'first (assort list))
     ≡ (remove-duplicates list :from-end t)"
  (fbind (key test)
    (let ((groups (queue)))
      (map-subseq (lambda (item)
                    (if-let ((group
                              (let ((kitem (key item)))
                                (find-if
                                 (lambda (group)
                                   (test kitem (key (bucket-front seq group))))
                                 (qlist groups)))))
                      (bucket-push seq item group)
                      (enq (make-bucket seq item) groups)))
                  seq start end)
      (mapcar-into (lambda (bucket)
                     (bucket-seq seq bucket))
                   (qlist groups)))))

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

(defun runs (seq &key (start 0) end (key #'identity) (test #'eql))
  "Return a list of runs of similar elements in SEQ.
The arguments START, END, and KEY are as for `reduce'.

    (runs '(head tail head head tail))
    => '((head) (tail) (head head) (tail))"
  (if (emptyp seq)
      (list seq)
      (seq-dispatch seq
        (list-runs seq start end key test)
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
                      (runs pos))))))))))

(defun batches (seq n &key (start 0) end)
  "Return SEQ in batches of N elements.

    (batches (iota 11) 2)
    => ((0 1) (2 3) (4 5) (6 7) (8 9) (10))"
  (check-type n (integer 0 *))
  (seq-dispatch seq
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
                              finally (setf seq rest)))))
    (let ((end (or end (length seq))))
      (nlet batches ((i start)
                     (acc '()))
        (if (>= i end)
            (nreverse acc)
            (batches (+ i n)
                     (cons (subseq seq i (min (+ i n) end))
                           acc)))))))

(defun frequencies (seq &rest hash-table-args)
  "Return a hash table with the count of each unique item in SEQ.
As a second value, return the length of SEQ.

From Clojure."
  (let ((total 0)
        (table (multiple-value-call #'make-hash-table
                 (values-list hash-table-args)
                 :size (values (floor (length seq) 2))
                 :test 'equal)))
    (declare (fixnum total))
    (map nil
         (lambda (elt)
           (incf total)
           (incf (gethash elt table 0)))
         seq)
    (values table total)))

(defun scan (fn seq &key (key #'identity) (initial-value nil initial-value?))
  "A version of `reduce' that shows its work.

Instead of returning just the final result, `scan' returns a sequence
of the successive results at each step.

    (reduce #'+ '(1 2 3 4))
    => 10

    (scan #'+ '(1 2 3 4))
    => '(1 3 6 10)

From APL and descendants."
  (multiple-value-bind (seq initial-value)
      (if initial-value?
          (values seq (list initial-value))
          (values (nsubseq seq 1) (list (elt seq 0))))
    (fbind (fn key)
      (nreverse
       (reduce (lambda (acc x)
                 (cons (fn x (key (car acc))) acc))
               seq
               :initial-value initial-value)))))

(defsubst nub (seq &rest args &key start end key (test #'equal))
  "Remove duplicates from SEQ, starting from the end.
TEST defaults to `equal'.

From Haskell."
  (declare (ignore start end key))
  (apply #'remove-duplicates seq :from-end t :test test args))

(defun gcp (seqs &key (test #'eql))
  "The greatest common prefix of SEQS.

If there is no common prefix, return NIL."
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

(defun gcs (seqs &key (test #'eql))
  "The greatest common suffix of SEQS.

If there is no common suffix, return NIL."
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
  (reduce (lambda (x y)
            (if (eql (longer x y) x) y x))
          seqs))

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

(defun ordering (seq &key unordered-to-end
                          from-end
                          (test 'eql)
                          (key #'identity))
  "Given a sequence, return a function that, when called with `sort',
restores the original order of the sequence.

That is, for any SEQ (without duplicates), it is always true that

     (equal seq (sort (shuffle (copy-seq seq)) (ordering seq)))

FROM-END controls what to do in case of duplicates. If FROM-END is
true, the last occurrence of each item is preserved; otherwise, only
the first occurrence counts.

TEST controls identity; it should be a valid test for a hash table. If
the items cannot be compared that way, you can use KEY to transform
them.

UNORDERED-TO-END controls where to sort items that are not present in
the original ordering. By default they are sorted first but, if
UNORDERED-TO-END is true, they are sorted last. In either case, they
are left in no particular order."
  (fbind key
    (let ((table (make-hash-table :test test))
          (i -1))
      (map nil
           (if from-end
               (lambda (item)
                 (setf (gethash (key item) table) (incf i)))
               (lambda (item)
                 (ensure-gethash (key item) table (incf i))))
           seq)

      (let ((default
              (if unordered-to-end
                  (1+ i)
                  -1)))
        (declare (fixnum default))
        (lambda (x y)
          (< (gethash (key x) table default)
             (gethash (key y) table default)))))))

(defsubst take (n seq)
  "Return, at most, the first N elements of SEQ, as a *new* sequence
of the same type as SEQ.

If N is longer than SEQ, SEQ is simply copied."
  (check-type n array-index)
  (seq-dispatch seq
    (firstn n seq)
    (subseq seq 0 (min n (length seq)))))

(defsubst drop (n seq)
  "Return all but the first N elements of SEQ.
The sequence returned is a new sequence of the same type as SEQ.

If N is greater than the length of SEQ, returns an empty sequence of
the same type."
  (check-type n array-index)
  (seq-dispatch seq
    (nthcdr n seq)
    (subseq seq (min (length seq) n))))

(defsubst take-while (pred seq)
  (seq-dispatch seq
    (ldiff seq (member-if-not pred seq))
    (subseq seq 0 (position-if-not pred seq))))

(defsubst drop-while (pred seq)
  (seq-dispatch seq
    (member-if-not pred seq)
    (subseq seq (position-if-not pred seq))))

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

(defun bestn (n seq pred &key (key #'identity) memo)
  "Partial sorting.
Equivalent to (firstn N (sort SEQ PRED)), but much faster, at least
for small values of N.

The name is from Arc."
  (declare (array-length n))
  (cond ((= n 0)
         (make-sequence-like seq 0))
        ((= n 1)
         (make-sequence-like seq 1
                             :initial-contents (list (extremum seq pred :key key))))
        ((length<= seq n)
         (sort (copy-seq seq) pred :key key))
        (t (fbind ((key (if memo
                            (let ((dict (make-hash-table :test 'equal))
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
                      (locally (declare (optimize speed))
                        (cond ((< i n)
                               (heap-insert heap elt :key key :test #'test))
                              ((test (key (heap-maximum heap)) (key elt))
                               (heap-extract-maximum heap :key key
                                                          :test #'test)
                               (heap-insert heap elt :key key :test #'test))))
                      (incf i))
                    seq)
               (let ((bestn (take n (nreverse (heap-extract-all heap :key key :test #'test)))))
                 (make-sequence-like seq n :initial-contents bestn)))))))

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
      (map-subseq #'update-extrema seq start end))
    (values min max)))

(defun split-at (list k)
  (declare (list list))
  (loop for i below k
        for (x . right) on list
        collect x into left
        finally (return (values left right))))

(defsubst halfway-point (seq)
  (let ((len (length seq)))
    (declare (type array-index len) (optimize speed))
    (ceiling len 2)))

(defun halves (seq &optional split)
  "Return, as two values, the first and second halves of SEQ.
SPLIT designates where to split SEQ; it defaults to half the length,
but can be specified.

The split is made using `ceiling' rather than `truncate'. This is on
the theory that, if SEQ is a single-element list, it should be
returned unchanged."
  (seq-dispatch seq
    (if split
        ;; If we know where to split in advance we only have to
        ;; traverse the list once.
        (split-at seq split)
        (split-at seq (halfway-point seq)))
    (let ((split (or split (halfway-point seq))))
      (values (subseq seq 0 split)
              (subseq seq split)))))

(defun dsu-sort (seq fn &key (key #'identity) stable)
  "Decorate-sort-undecorate using KEY.
Useful when KEY is an expensive function (e.g. database access)."
  (fbind (key)
    (map-into seq
              #'cdr
              ;; Vectors sort faster.
              (funcall (if stable #'stable-sort #'sort)
                       (map 'vector
                            (lambda (item)
                              (cons (key item) item))
                            seq)
                       fn
                       :key #'car))))

(defun deltas (seq &optional (fn #'-))
  "Return the successive differences in SEQ.

     (deltas '(4 9 -5 1 2))
     => '(4 5 -14 6 1)

Note that the first element of SEQ is also the first element of the
return value.

By default, the delta is the difference, but you can specify another
function as a second argument:

    (deltas '(2 4 2 6) #'/)
    => '(2 2 1/2 3)

From Q."
  (let ((fn (ensure-function fn)))
    (seq-dispatch seq
      (cons (car seq)
            (mapcar fn (cdr seq) seq))
      (when (> (length seq) 0)
        (cons (elt seq 0)
              (map 'list fn (nsubseq seq 1) seq))))))

(defcondition inconsistent-graph (error)
  ((constraints :initarg :constraints
                :reader inconsistent-graph-constraints
                :documentation "The offending constraints"))
  (:documentation "A graph that cannot be consistently sorted.")
  (:report (lambda (self stream)
             (format stream
                     "Inconsistent graph: ~a"
                     (inconsistent-graph-constraints self)))))

(setf (documentation 'inconsistent-graph-constraints 'function)
      "The constraints of an `inconsistent-graph' error.
Cf. `toposort'.")

(defun default-tie-breaker (min-elts constraints)
  "The default tie breaker for a topological sort."
  (declare (ignore constraints))
  (first min-elts))

(defun tsort (elts constraints tie-breaker)
  "Do the initial topological sort."
  (loop while elts
        for min-elts = (or (remove-if
                            (lambda (x)
                              (member x constraints
                                      :key #'second))
                            elts)
                           (error 'inconsistent-graph
                                  :constraints constraints))
        for choice = (if (null (rest min-elts))
                         (first min-elts)
                         (funcall tie-breaker min-elts (reverse results)))
        do (removef elts choice)
           (removef constraints choice :test #'member)
        collect choice into results
        finally (return results)))

(defun toposort (constraints
                 &key (test #'eql)
                      (tie-breaker #'default-tie-breaker)
                      from-end unordered-to-end)
  "Turn CONSTRAINTS into a predicate for use with SORT.

Each constraint should be two-element list.

    (def dem-bones '((toe foot)
                     (foot heel)
                     (heel ankle)
                     (ankle shin)
                     (shin knee)
                     (knee back)
                     (back shoulder)
                     (shoulder neck)
                     (neck head)))
    (sort (shuffle (mapcar #'car dem-bones))
          (toposort dem-bones))
    => (TOE FOOT HEEL ANKLE SHIN KNEE BACK SHOULDER NECK)

If the graph is inconsistent, signals an error of type
`inconsistent-graph`:

    (toposort '((chicken egg) (egg chicken)))
    => Inconsistent graph: ((CHICKEN EGG) (EGG CHICKEN))

TEST, FROM-END, and UNORDERED-TO-END are passed through to
`ordering'."
  ;; Adapted from AMOP.
  (let ((elts (remove-duplicates (flatten constraints))))
    (ordering (tsort elts constraints tie-breaker)
              :test test
              :unordered-to-end unordered-to-end
              :from-end from-end)))

(defun intersperse/list (new-elt list)
  (loop for (item . rest) on list
        if (null rest)
          collect item
        else collect item
             and collect new-elt))

(defun intersperse/seq (new-elt seq)
  (if (< (length seq) 2)
      (copy-seq seq)
      (let* ((len1 (length seq))
             (len2 (1- (* 2 len1)))
             (ret (make-sequence-like seq len2))
             (j 0))
        (loop for i below len2 do
          (if (oddp i)
              (setf (elt ret i) new-elt)
              (progn
                (setf (elt ret i) (elt seq j))
                (incf j))))
        ret)))

(defsubst intersperse (new-elt seq)
  "Return a sequence like SEQ, but with NEW-ELT inserted between each
element."
  (seq-dispatch seq
    (intersperse/list new-elt seq)
    (intersperse/seq new-elt seq)))

(defun mvfold-aux (fn seq seeds &optional from-end)
  (cond ((null seeds)
         (reduce fn seq :from-end from-end))
        ((null (rest seeds))
         (reduce fn seq :from-end from-end
                        :initial-value (first seeds)))
        (t (let ((fn (ensure-function fn)))
             (values-list
              (reduce (lambda (l r)
                        ;; We use `replace' so the function is always
                        ;; called with the right number of values.
                        (replace (fill (copy-list seeds) nil)
                                 (multiple-value-list
                                  (if from-end
                                      (apply fn l r)
                                      (multiple-value-call fn
                                        (values-list l)
                                        r)))))
                      seq
                      :initial-value seeds
                      :from-end from-end))))))

(defun mvfold (fn seq &rest seeds)
  "Like `reduce' extended to multiple values.

Calling `mvfold' with one seed is equivalent to `reduce':

    (mvfold fn xs seed) ≡ (reduce fn xs :initial-value seed)

However, you can also call `mvfold' with multiple seeds:

    (mvfold fn xs seed1 seed2 seed3 ...)

How is this useful? Consider extracting the minimum of a sequence:

    (reduce #'min xs)

Or the maximum:

    (reduce #'max xs)

But both?

    (reduce (lambda (cons item)
              (cons (min (car cons) item)
                    (max (cdr cons) item)))
            xs
            :initial-value (cons (elt xs 0) (elt xs 0)))

You can do this naturally with `mvfold'.

    (mvfold (lambda (min max item)
              (values (min item min)
                      (max item max)))
            xs (elt xs 0) (elt xs 0))

In general `mvfold' provides a functional idiom for “loops with
book-keeping” where we might otherwise have to use recursion or
explicit iteration.

Has a compiler macro that generates efficient code when the number of
SEEDS is fixed at compile time (as it usually is)."
  (mvfold-aux fn seq seeds))

(defun mvfoldr (fn seq &rest seeds)
  "Like `(reduce FN SEQ :from-end t)' extended to multiple
values. Cf. `mvfold'."
  (mvfold-aux fn seq seeds t))

(eval-when (:compile-toplevel :load-toplevel)
  (defun expand-mvfold (fn seq seeds &optional from-end)
    (cond ((null seeds)
           `(reduce ,fn ,seq :from-end ,from-end))
          ((null (cdr seeds))
           `(reduce ,fn ,seq
                    :from-end ,from-end
                    :initial-value ,(car seeds)))
          (t (let ((tmps (make-gensym-list (length seeds))))
               (with-gensyms (item)
                 (once-only (fn)
                   `(let ,(mapcar #'list tmps seeds)
                      ,(if from-end
                           `(map-subseq (lambda (,item)
                                          (setf (values ,@tmps)
                                                (funcall ,fn ,item ,@tmps)))
                                        ,seq
                                        :from-end t)

                           `(map nil (lambda (,item)
                                       (setf (values ,@tmps)
                                             (funcall ,fn ,@tmps ,item)))
                                 ,seq))
                      (values ,@tmps)))))))))

(define-compiler-macro mvfold (fn seq &rest seeds)
  "Optimize `mvfold' with a fixed number of seeds."
  (expand-mvfold fn seq seeds))

(define-compiler-macro mvfoldr (fn seq &rest seeds)
  "Optimize `mvfoldr' with a fixed number of seeds."
  (expand-mvfold fn seq seeds t))
