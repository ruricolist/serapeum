(in-package :serapeum)

;;; Some of this API is based on
;;; https://common-lisp.net/project/sequence-iterators/.

(deftype function-name ()
  '(and symbol (not (member t nil))))

(deftype key-designator ()
  '(or null function-name function))

(deftype test-designator ()
  '(or null function-name function))

(deftype signed-array-index ()
  "A (possibly negated) array index."
  '#.(let ((limit (1- array-dimension-limit)))
       `(integer (,(- limit)) (,limit))))

(deftype signed-array-length ()
  "A (possibly negated) array length."
  '#.(let ((limit (1- array-dimension-limit)))
       `(integer ,(- limit) ,limit)))

(defsubst sequence? (x)
  (typep x 'sequence))

(-> canonicalize-key (key-designator) function)
(defun canonicalize-key (k)
  (etypecase-of key-designator k
    (null #'identity)
    (function-name (fdefinition k))
    (function k)))

(-> canonicalize-test (test-designator &optional test-designator)
    function)
(defun canonicalize-test (test &optional test-not)
  (flet ((canonicalize (test)
           (etypecase-of test-designator test
             (null #'eql)
             (function-name (fdefinition test))
             (function test))))
    (cond ((and test test-not)
           (error "Cannot supply both ~s and ~s to a sequence function" 'test 'test-not))
          (test (canonicalize test))
          (test-not (canonicalize test-not))
          (t #'eql))))

(-> key-test (key-designator test-designator &optional test-designator)
    function)
(defun key-test (key test &optional test-not)
  "Return a function of two arguments which uses KEY to extract the
part of the arguments to compare, and compares them using TEST."
  (declare (optimize (safety 1) (debug 0)))
  (let ((key (canonicalize-key key))
        (test (canonicalize-test test test-not)))
    (if (eql key #'identity) test
        (fbind key
          (with-test-fn (test)
            (lambda (x y)
              (test (key x) (key y))))))))

(defun make-sequence-like (seq len &rest args &key initial-element
                                                   (initial-contents nil ic?))
  "Helper function: make a sequence of length LEN having the same type as SEQ."
  (seq-dispatch seq
    (if ic?
        (map 'list #'identity initial-contents)
        (make-list len :initial-element initial-element))
    (apply #'make-array len :element-type (array-element-type seq) args)
    #+(or sbcl abcl) (apply #'sequence:make-sequence-like seq len args)))

(define-do-macro do-vector ((var vec &optional return) &body body)
  "Iterate over the items of a vector in a manner similar to `dolist'."
  ;; TODO See if any other implementations expose something similar.
  #+ccl `(ccl:dovector (,var ,vec ,@(unsplice return))
           ,@body)
  #-ccl `(map nil (lambda (,var) ,@body) ,vec))

(define-do-macro %do-each ((var seq &optional return) &body body)
  "Only for Lisps that do not support extensible sequences."
  (once-only (seq)
    `(seq-dispatch ,seq
       (dolist (,var ,seq)
         ,@body)
       (do-vector (,var ,seq)
         ,@body))))

(defmacro do-each ((var seq &optional return) &body body)
  "Iterate over the elements of SEQ, a sequence.
If SEQ is a list, this is equivalent to `dolist'."
  ;; We hoist the body and use sb-ext:muffle-conditions to prevent
  ;; SBCL from spamming us with code deletion notes. (It may also be
  ;; desirable in itself to avoid needless code duplication in Lisps
  ;; without type inference.)
  (with-thunk (body var)
    (let ((iter-spec `(,var ,seq ,@(unsplice return))))
      `(locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
         #+(or sbcl abcl)
         (sequence:dosequence ,iter-spec
           (,body ,var))
         #-(or sbcl abcl)
         (%do-each ,iter-spec
           (,body ,var))))))

(declaim (inline map-subseq))
(defun map-subseq (fn seq &optional start end from-end)
  "Helper function to map SEQ between START and END."
  (declare (type (or null array-index) start end)
           (optimize (debug 0) (safety 1)
                     (compilation-speed 0)))
  ;; (when (and start end)
  ;;   (assert (<= start end)))
  (let ((start (or start 0))
        (fn (ensure-function fn)))
    (fbind (fn)
      (seq-dispatch seq
        (if (null end)
            (if from-end
                (list-map-from-end/bordeaux fn seq :start start)
                (dolist (item (nthcdr start seq))
                  (fn item)))
            (if from-end
                (list-map-from-end/bordeaux fn seq :start start :end end)
                (loop for item in (nthcdr start seq)
                      for i below (- end start)
                      do (fn item))))
        (with-subtype-dispatch vector
            (simple-bit-vector
             bit-vector
             (simple-array character (*))
             simple-base-string)
            seq
          (let ((end (or end (length seq))))
            (if from-end
                (loop for i downfrom (1- end) to start
                      do (fn (vref seq i)))
                (loop for i from start below end
                      do (fn (vref seq i))))))
        (let ((end (or end (length seq))))
          (if from-end
              (loop for i downfrom (1- end) to start
                    do (fn (elt seq i)))
              (loop for i from start below end
                    do (fn (elt seq i)))))))))
(declaim (notinline map-subseq))

(define-do-macro do-subseq ((var seq &optional return
                                 &key start
                                 end
                                 from-end)
                            &body body)
  `(map-subseq
    (lambda (,var)
      ,@body)
    ,seq
    ,start ,end
    ,from-end))

;;; Define a protocol for accumulators so we can write functions like
;;; `assort', `partition', &c. generically.

(defun make-bucket (seq &optional (init nil initp))
  (seq-dispatch seq
    (if initp
        (queue init)
        (queue))
    (with-boolean initp
      (make-array (eif initp 1 0)
                  :element-type (array-element-type seq)
                  :adjustable t
                  :fill-pointer (eif initp 1 0)
                  :initial-contents (and initp (list init))))
    (if initp
        (make-bucket () init)
        (make-bucket ()))))

(defun bucket-push (seq item bucket)
  (seq-dispatch seq
    (enq item bucket)
    (vector-push-extend item bucket)
    (bucket-push () item bucket)))

(defun bucket-seq (seq bucket)
  (seq-dispatch seq
    (qlist bucket)
    bucket
    (let ((len (qlen bucket)))
      (make-sequence-like seq len :initial-contents (qlist bucket)))))

(defun bucket-front (seq bucket)
  (seq-dispatch seq
    (front bucket)
    (and (> (length bucket) 0)
         (vref bucket 0))
    (bucket-front () bucket)))

(-> nsubseq
    (sequence array-index &optional (or null array-length))
    sequence)
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
  (cond
    ;; Simple cases.
    ((= count 0) (make-sequence-like seq 0))
    ((> count (length seq)) (apply #'filter pred seq :count nil args))
    (t (fbind (pred)
         (with-key-fn (key)
           (let ((ret (make-bucket seq)))
             (do-subseq (item seq nil :start start :end end :from-end from-end)
               (when (pred (key item))
                 (bucket-push seq item ret)
                 (when (zerop (decf count))
                   (return))))
             (let ((seq2 (bucket-seq seq ret)))
               (if from-end (nreverse seq2) seq2))))))))

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
(defun filter/swapped-arguments (seq pred &rest args &key &allow-other-keys)
  (apply #'filter pred seq args))

(define-modify-macro filterf (pred &rest args)
  filter/swapped-arguments
  "Modify-macro for FILTER.
The place designed by the first argument is set to th result of
calling FILTER with PRED, the place, and ARGS.")

(defun keep (item seq &rest args &key (test #'eql) from-end key count
             &allow-other-keys)
  "Almost, but not quite, an alias for `remove` with `:test-not` instead of `:test`.

The difference is the handling of COUNT. For keep, COUNT is the number of items to keep, not remove.

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
        (fbind ((test (partial test item)))
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

(-> single (sequence) boolean)
(defsubst single (seq)
  "Is SEQ a sequence of one element?"
  (seq-dispatch seq
    (and seq (endp (cdr seq)))
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
  (fbind ((test (compose pred (canonicalize-key key))))
    (let ((pass (make-bucket seq))
          (fail (make-bucket seq)))
      (do-subseq (item seq nil :start start :end end)
        (if (test item)
            (bucket-push seq item pass)
            (bucket-push seq item fail)))
      (values (bucket-seq seq pass) (bucket-seq seq fail)))))

(defun partitions (preds seq &key (start 0) end (key #'identity))
  "Generalized version of PARTITION.

PREDS is a list of predicates. For each predicate, `partitions'
returns a filtered copy of SEQ. As a second value, it returns an extra
sequence of the items that do not match any predicate.

Items are assigned to the first predicate they match."
  (with-key-fn (key)
    (let ((buckets (loop for nil in preds collect (make-bucket seq)))
          (extra (make-bucket seq)))
      (do-subseq (item seq nil :start start :end end)
        (loop for pred in preds
              for bucket in buckets
              for fn = (ensure-function pred)
              if (funcall fn (key item))
                return (bucket-push seq item bucket)
              finally (bucket-push seq item extra)))
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
  (fbind (test)
    (with-key-fn (key)
      (let ((groups (queue)))
        (do-subseq (item seq nil :start start :end end)
          (if-let ((group
                    (let ((kitem (key item)))
                      (find-if
                       (lambda (group)
                         (test kitem (key (bucket-front seq group))))
                       (qlist groups)))))
            (bucket-push seq item group)
            (enq (make-bucket seq item) groups)))
        (mapcar-into (lambda (bucket)
                       (bucket-seq seq bucket))
                     (qlist groups))))))

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
                     (pos (position-if-not (partial #'test elt)
                                           seq
                                           :start (1+ start)
                                           :end end)))
                (if (null pos)
                    (collect (subseq seq start end))
                    (progn
                      (collect (subseq seq start pos))
                      (runs pos))))))))))

(defun batches (seq n &key (start 0) end even)
  "Return SEQ in batches of N elements.

    (batches (iota 11) 2)
    => ((0 1) (2 3) (4 5) (6 7) (8 9) (10))

If EVEN is non-nil, then SEQ must be evenly divisible into batches of
size N, with no leftovers."
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
  (check-type n (integer 0 *))
  (check-type start (integer 0 *))
  (flet ((uneven ()
           (error "A ~@[sub~*~]sequence of length ~a cannot be evenly divided into batches of size ~a."
                  end
                  (- (or end (length seq)) start)
                  n)))
    (declare (dynamic-extent #'uneven))
    (flet ((check-bounds-even (start end)
             (when even
               (unless (zerop (rem (- end start) n))
                 (uneven)))))
      (declare (inline check-bounds-even))
      (with-boolean even
        (seq-dispatch seq
          (let ((seq (nthcdr start seq)))
            (if (null end)
                (loop while seq
                      collect (loop for i below n
                                    for (elt . rest) on seq
                                    collect elt
                                    finally (setf seq rest)
                                            (when even
                                              (unless (= i n)
                                                (uneven)))))
                (progn
                  (check-bounds-even start end)
                  (loop while seq
                        for i from start below end by n
                        collect
                        (loop with m = (min n (- end i))
                              for i below m
                              for (elt . rest) on seq
                              collect elt
                              finally (setf seq rest)
                                      (when even
                                        (unless (= i m)
                                          (uneven))))))))
          (let ((end (or end (length seq))))
            (check-bounds-even start end)
            (nlet batches ((i start)
                           (acc '()))
              (if (>= i end)
                  (nreverse acc)
                  (batches (+ i n)
                           (cons (subseq seq i (min (+ i n) end))
                                 acc))))))))))

(defun frequencies (seq &rest hash-table-args &key (key #'identity)
                    &allow-other-keys)
  "Return a hash table with the count of each unique item in SEQ.
As a second value, return the length of SEQ.

From Clojure."
  (let ((total 0)
        ;; Using multiple-value-call lets us specify defaults while
        ;; still ensuring the caller can override them.
        (table (multiple-value-call #'make-hash-table
                 (values-list (remove-from-plist hash-table-args :key))
                 :size (values (floor (length seq) 2))
                 :test 'equal)))
    (declare (fixnum total))
    (with-key-fn (key)
      (if (typep seq 'bit-vector)
          (with-subtype-dispatch bit-vector (simple-bit-vector) seq
            (setf (gethash (key 0) table) (count 0 seq)
                  (gethash (key 1) table) (count 1 seq)
                  total (length seq)))
          (do-each (elt seq)
            (incf total)
            (incf (gethash (key elt) table 0)))))
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
    (fbind (fn)
      (with-key-fn (key)
        (nreverse
         (with-key-fn (key)
           (reduce (lambda (acc x)
                     (cons (fn x (key (car acc))) acc))
                   seq
                   :initial-value initial-value)))))))

(defsubst nub (seq &rest args &key start end key (test #'equal))
  "Remove duplicates from SEQ, starting from the end.
TEST defaults to `equal'.

From Haskell."
  (declare (ignore start end key))
  (apply #'remove-duplicates seq :from-end t :test test args))

(define-compiler-macro nub (seq &rest args &key (test '#'equal) &allow-other-keys)
  `(remove-duplicates ,seq :from-end t :test ,test ,@args))

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

(-> of-length (array-length) function)
(defun of-length (length)
  "Return a predicate that returns T when called on a sequence of
length LENGTH.

    (funcall (of-length 3) '(1 2 3)) => t
    (funcall (of-length 1) '(1 2 3)) => nil"
  (lambda (seq)
    (sequence-of-length-p seq length)))

(define-compiler-macro of-length (&whole call length &environment env)
  (if (constantp length env)
      `(lambda (seq)
         (sequence-of-length-p seq ,length))
      call))

(defun length< (&rest seqs)
  "Is each length-designator in SEQS shorter than the next?
A length designator may be a sequence or an integer."
  (declare (dynamic-extent seqs))
  (apply #'length> (reverse seqs)))

(defun length> (&rest seqs)
  "Is each length-designator in SEQS longer than the next?
A length designator may be a sequence or an integer."
  (nlet rec ((last most-positive-fixnum)
             (seqs seqs))
    (if (endp seqs) t
        (destructuring-bind (seq . seqs) seqs
          (etypecase seq
            (array-length
             (and (> last seq)
                  (rec seq seqs)))
            (list
             (let ((len
                     ;; Get the length of SEQ, but only up to LAST.
                     (loop with len = 0
                           until (endp seq) do
                             (incf len)
                             (pop seq)
                           finally (return len))))
               (and (> last len)
                    (rec len seqs))))
            (sequence
             (let ((len (length seq)))
               (and (> last len)
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

(-> longest (sequence) sequence)
(defun longest (seqs)
  "Return the longest seq in SEQS."
  (reduce #'longer seqs))

(-> shortest (sequence) sequence)
(defun shortest (seqs)
  "Return the shortest seq in SEQS."
  (reduce (lambda (x y)
            (if (eql (longer x y) x) y x))
          seqs))

(defsubst slice-bounds (len start end)
  "Normalize START and END, which may be negative, to offsets
acceptable to SUBSEQ."
  (declare (type signed-array-index start)
           (type signed-array-length end)
           (type array-index len))
  (values (if (minusp start)
              (+ len start)
              start)
          (if (null end)
              nil
              (if (minusp end)
                  (+ len end)
                  end))))

(-> slice
    (sequence signed-array-index &optional (or null signed-array-length))
    sequence)
(defun slice (seq start &optional (end (length seq)))
  "Like `subseq', but allows negative bounds to specify offsets.
Both START and END accept negative bounds.

     (slice \"string\" -3 -1) => \"in\"

Setf of `slice' is like setf of `ldb': afterwards, the place being set
holds a new sequence which is not EQ to the old."
  (multiple-value-bind (start end)
      (slice-bounds (length seq) start end)
    (subseq seq start end)))

(defun setslice (seq1 seq2 start &optional end)
  "Helper to `set' a slice non-destructively."
  (multiple-value-bind (start end)
      (slice-bounds (length seq1) start end)
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

     (equal seq (sort (reshuffle seq) (ordering seq)))

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
  (let ((table (make-hash-table :test test))
        (i -1))
    (with-key-fn (key)
      (with-boolean from-end
        (do-each (item seq)
          (if from-end
              (setf (gethash (key item) table) (incf i))
              (ensure-gethash (key item) table (incf i)))))

      (let ((default
              (if unordered-to-end
                  (1+ i)
                  -1)))
        (declare (fixnum default))
        (lambda (x y)
          (< (gethash (key x) table default)
             (gethash (key y) table default)))))))

(-> take (signed-array-length sequence) sequence)
(defsubst take (n seq)
  "Return, at most, the first N elements of SEQ, as a *new* sequence
of the same type as SEQ.

If N is longer than SEQ, SEQ is simply copied.

If N is negative, then |N| elements are taken (in their original
order) from the end of SEQ."
  (declare (type signed-array-length n))
  (seq-dispatch seq
    (if (minusp n)
        (last seq (abs n))
        (firstn n seq))
    (if (minusp n)
        (subseq seq (max 0 (+ (length seq) n)))
        (subseq seq 0 (min n (length seq))))))

(-> drop (signed-array-length sequence) sequence)
(defsubst drop (n seq)
  "Return all but the first N elements of SEQ.
The sequence returned is a new sequence of the same type as SEQ.

If N is greater than the length of SEQ, returns an empty sequence of
the same type.

If N is negative, then |N| elements are dropped from the end of SEQ."
  (declare (type signed-array-length n))
  (seq-dispatch seq
    (if (minusp n)
        (butlast seq (abs n))
        (nthcdr n seq))
    (if (minusp n)
        (subseq seq 0 (max 0 (+ (length seq) n)))
        (subseq seq (min (length seq) n)))))

(-> take-while (function sequence) sequence)
(defsubst take-while (pred seq)
  "Return the prefix of SEQ for which PRED returns true."
  (seq-dispatch seq
    (ldiff seq (member-if-not pred seq))
    (subseq seq 0 (position-if-not pred seq))))

(-> drop-while (function sequence) sequence)
(defsubst drop-while (pred seq)
  "Return the largest possible suffix of SEQ for which PRED returns
false when called on the first element."
  (seq-dispatch seq
    (member-if-not pred seq)
    (subseq seq (position-if-not pred seq))))

;;;# `bestn'
(defun bisect-left (vec item pred &key key)
  "Return the index in VEC to insert ITEM and keep VEC sorted."
  (declare ((simple-array * (*)) vec))
  (fbind (pred key)
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

;;; The heap implementation is adapted from Zach Beane's timers
;;; package for SBCL.

(deftype heap ()
  '(vector t))

(defsubst heap-parent (i)
  (declare (array-index i))
  (ash (1- i) -1))

(defsubst heap-left (i)
  (declare (array-index i))
  (1+ (ash i 1)))

(defsubst heap-right (i)
  (declare (array-index i))
  (+ 2 (ash i 1)))

(defun heapify (heap start &key (key #'identity) (test #'>=))
  (declare (function key test)
           (array-index start)
           (heap heap))
  (fbind (key (ge test))
    (declare (ftype (-> (t) t) key)
             (ftype (-> (t t) t) ge))
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
  (declare (function key test) (heap heap))
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
  (declare (heap heap))
  (unless (zerop (length heap))
    (aref heap 0)))

(defun heap-extract (heap i &key (key #'identity) (test #'>=))
  (declare (heap heap) (array-index i))
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
  (declare (heap heap))
  (loop while (> (length heap) 0)
        collect (heap-extract-maximum heap :key key :test test)))

(defsubst make-heap (&optional (size 100))
  (make-array size :adjustable t :fill-pointer 0))

(defun bestn (n seq pred &key (key #'identity) memo)
  "Partial sorting.
Equivalent to (firstn N (sort SEQ PRED)), but much faster, at least
for small values of N.

With MEMO, use a decorate-sort-undecorate transform to ensure KEY is
only ever called once per element.

The name is from Arc."
  (declare (array-length n))
  (setf key (canonicalize-key key))
  (cond (memo
         (fbind (key)
           ;; Can't just copy SEQ, because it may not allow conses as
           ;; elements (e.g. octet vectors).
           (let* ((temp (map 'vector (op (cons (key _1) _1)) seq))
                  (bestn (bestn n temp pred :key #'car :memo nil))
                  (bestn (map-into bestn #'cdr bestn))
                  ;; The original sequence might have been shorter
                  ;; than N.
                  (out (make-sequence-like seq (length bestn))))
             (assert (length= bestn out))
             (replace out bestn))))
        ((= n 0)
         (make-sequence-like seq 0))
        ((= n 1)
         (make-sequence-like seq 1
                             :initial-contents (list (extremum seq pred :key key))))
        ((length<= seq n)
         (sort (copy-seq seq) pred :key key))
        (t (fbind ((key key)
                   (test (complement pred)))
             (let ((heap (make-heap n))
                   (i 0))
               (declare (array heap) (array-length i))
               (do-each (elt seq)
                 (locally (declare (optimize speed))
                   (cond ((< i n)
                          (heap-insert heap elt :key key :test #'test))
                         ((test (key (heap-maximum heap)) (key elt))
                          (heap-extract-maximum heap :key key
                                                     :test #'test)
                          (heap-insert heap elt :key key :test #'test))))
                 (incf i))
               (let ((bestn (take n (nreverse (heap-extract-all heap :key key :test #'test)))))
                 (make-sequence-like seq n :initial-contents bestn)))))))

(defun nth-best (n seq pred &key (key #'identity))
  "Return the Nth-best element of SEQ under PRED.

Equivalent to

    (elt (sort (copy-seq seq) pred) n)

Or even

    (elt (bestn (1+ n) seq pred) n)

But uses a selection algorithm for better performance than either."
  (check-type n array-index)
  (if (zerop n)
      (extremum seq pred :key key)
      (let* ((seq  (copy-sequence 'vector seq))
             (pred (ensure-function pred))
             (key  (ensure-function key))
             (pred (key-test key pred)))
        (quickselect seq n pred))))

(-> quickselect (vector array-index function) t)
(defun quickselect (a k lt)
  "Hoare's quickselect, as implemented by Wirth (\"FIND\"), with
  refinements by V. Zabrodsky (\"MODIFIND\")."
  (declare (optimize (debug 0) (safety 1)))
  (assert (< k (length a)))
  (fbind (lt)
    (with-vector-dispatch () a
      (loop with n = (length a)
            with l of-type array-index = 0
            with r of-type array-index = (1- n)
            for x = (vref a k)
            for i = l
            for j = r
            while (< l r) do
              (loop until (or (< j k) (< k i))
                    do (loop while (lt (vref a i) x) do (incf i))
                       (loop while (lt x (vref a j)) do (decf j))
                       (rotatef (vref a i) (vref a j))
                       (incf i)
                       (decf j))
              (when (< j k) (setf l i))
              (when (< k i) (setf r j))
            finally (return (vref a k))))))

(-> reshuffle (sequence &key (:element-type t)) (simple-array * (*)))
(defun reshuffle (seq &key (element-type '*))
  "Like `alexandria:shuffle', but non-destructive.

Regardless of the type of SEQ, the return value is always a vector.

If ELEMENT-TYPE is provided, this is the element type (modulo
upgrading) of the vector returned.

If ELEMENT-TYPE is not provided, then the element type of the vector
returned is T, if SEQ is not a vector. If SEQ is a vector, then the
element type of the vector returned is the same as the as the element
type of SEQ."
  (shuffle (copy-sequence `(simple-array ,element-type (*))
                          seq)))

(-> sort-new (sequence function
                       &key
                       (:key (or function symbol))
                       (:element-type t))
    (simple-array * (*)))
(defun sort-new (seq pred &key (key #'identity)
                               (element-type '*))
  "Return a sorted vector of the elements of SEQ.

You can think of this as a non-destructive version of `sort', except
that it always returns a vector. (If you're going to copy a sequence
for the express purpose of sorting it, you might as well copy it into
a form that can be sorted efficiently.)

ELEMENT-TYPE is interpreted as for `reshuffle'."
  (sort (copy-sequence `(simple-array ,element-type (*))
                       seq)
        pred
        :key key))

(-> stable-sort-new (sequence function
                              &key
                              (:key (or function symbol))
                              (:element-type t))
    (simple-array * (*)))
(defun stable-sort-new (seq pred &key (key #'identity) (element-type '*))
  "Like `sort-new', but sort as if by `stable-sort' instead of `sort'."
  (stable-sort (copy-sequence `(simple-array ,element-type (*))
                              seq)
               pred
               :key key))

(defun extrema (seq pred &key (key #'identity) (start 0) end)
  "Like EXTREMUM, but returns both the minimum and the maximum (as two
values).

     (extremum (iota 10) #'>) => 9
     (extrema (iota 10) #'>) => 9, 0"
  (fbind (pred)
    (with-key-fn (key)
      (let (min max kmin kmax (init t))
        (flet ((update-extrema (x)
                 (if init
                     (setf min x max x
                           kmin (key x)
                           kmax kmin
                           init nil)
                     (let ((kx (key x)))
                       (cond ((pred kx kmin)
                              (setf kmin kx min x))
                             ((pred kmax kx)
                              (setf kmax kx max x)))))))
          (declare (dynamic-extent #'update-extrema))
          (map-subseq #'update-extrema seq start end))
        (values min max)))))

(-> split-at (list array-index) (values list list))
(defun split-at (list k)
  (declare (list list)
           (optimize speed))
  (nlet rec ((left '())
             (right list)
             (k k))
    (declare (array-index k))
    (if (zerop k)
        (values (nreverse left) right)
        (rec (cons (car right) left)
             (cdr right)
             (1- k)))))

(-> halves
    (sequence &optional (or null signed-array-index))
    (values sequence sequence))
(defun halves (seq &optional split)
  "Return, as two values, the first and second halves of SEQ.
SPLIT designates where to split SEQ; it defaults to half the length,
but can be specified.

If SPLIT is not provided, the length is halved using `ceiling' rather
than `truncate'. This is on the theory that, if SEQ is a
single-element list, it should be returned unchanged.

If SPLIT is negative, then the split is determined by counting |split|
elements from the right (or, equivalently, length+split elements from
the left."
  (declare ((or null signed-array-index) split))
  (flet ((halfway-point (seq)
           (ceiling (length seq) 2)))
    (seq-dispatch seq
      (if split
          (if (minusp split)
              (split-at seq (max 0 (+ (length seq) split)))
              ;; If we know where to split in advance we only have to
              ;; traverse the list once.
              (split-at seq split))
          (split-at seq (halfway-point seq)))
      (let* ((len (length seq))
             (split (or (and split
                             (clamp
                              (if (minusp split)
                                  (+ len split)
                                  split)
                              0 len))
                        (halfway-point seq))))
        (values (subseq seq 0 split)
                (subseq seq split))))))

(defun dsu-sort (seq fn &key (key #'identity) stable)
  "Decorate-sort-undecorate using KEY.
Useful when KEY is an expensive function (e.g. database access)."
  (with-key-fn (key)
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
      (and (> (length seq) 0)
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

Each constraint should be two-element list, where the first element of
the list should come before the second element of the list.

    (def dem-bones '((toe foot)
                     (foot heel)
                     (heel ankle)
                     (ankle shin)
                     (shin knee)
                     (knee back)
                     (back shoulder)
                     (shoulder neck)
                     (neck head)))
    (sort (reshuffle (mapcar #'car dem-bones))
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

(-> intersperse (t sequence) sequence)
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
                 (rebinding-functions (fn)
                   `(let ,(mapcar #'list tmps seeds)
                      ,(if from-end
                           `(do-subseq (,item ,seq :from-end t)
                              (setf (values ,@tmps)
                                    (funcall ,fn ,item ,@tmps)))
                           `(do-each (,item ,seq)
                              (setf (values ,@tmps)
                                    (funcall ,fn ,@tmps ,item))))
                      (values ,@tmps)))))))))

(define-compiler-macro mvfold (fn seq &rest seeds)
  "Optimize `mvfold' with a fixed number of seeds."
  (expand-mvfold fn seq seeds))

(define-compiler-macro mvfoldr (fn seq &rest seeds)
  "Optimize `mvfoldr' with a fixed number of seeds."
  (expand-mvfold fn seq seeds t))

;;; The obvious signature:
;;; (-> repeat-sequence (sequence array-length) sequence)
;;; is not used because N could be any positive integer as long as
;;; SEQUENCE is empty.

(-> repeat-sequence (sequence (integer 0 *)) sequence)
(defun repeat-sequence (seq n)
  "Return a sequence like SEQ, with the same content, but repeated N times.

    (repeat-sequence \"13\" 3)
    => \"131313\"

The length of the sequence returned will always be the length of SEQ
times N.

This means that 0 repetitions results in an empty sequence:

    (repeat-sequence \"13\" 0)
    => \"\"

Conversely, N may be greater than the possible length of a sequence,
as long as SEQ is empty.

    (repeat-sequence \"\" (1+ array-dimension-limit))
    => \"\"
"
  (check-type n (integer 0 *))
  (seq-dispatch seq
    (repeat-list seq n)
    (repeat-vector seq n)
    (let ((len (length seq)))
      (if (zerop len)
          (make-sequence-like seq 0)
          (loop with out = (make-sequence-like seq (* len n))
                repeat n
                for offset from 0 by (length seq)
                do (replace out seq :start1 offset)
                finally (return out))))))

(defun repeat-list (list n)
  (declare (optimize speed (safety 0)))
  (if (null list) nil
      (let ((n (assure array-index n)))
        (collecting*
          (loop repeat n do
            (loop for item in list do
              (collect item)))))))

(defun repeat-vector (vec n)
  (declare (type vector vec)
           (optimize (safety 0) (debug 0)))
  (when (= (length vec) 0)
    (return-from repeat-vector
      (make-array 0 :element-type (array-element-type vec))))
  (unless (< (* (length vec) n) array-dimension-limit)
    (error "A vector of size ~a*~a is too big" (length vec) n))
  (let* ((len (length vec))
         (n n)
         (len-out (* len n)))
    (declare (array-index len n len-out))
    (with-vector-dispatch (bit-vector simple-bit-vector (simple-array character (*)))
      vec
      (let ((out (make-array len-out :element-type (array-element-type vec))))
        (nlet rec ((n n) (offset 0))
              (declare (array-index n offset))
              (if (zerop n)
                  out
                  (progn
                    (replace out vec :start1 offset)
                    (rec (1- n) (+ offset len)))))))))

(labels ((%seq= (x y)
           (or (equal x y)
               (and (typep x 'sequence)
                    (typep y 'sequence)
                    (length= x y)
                    (every #'%seq= x y)))))

  (-> seq=/2 (t t) boolean)
  (defun seq=/2 (x y)
    (%seq= x y))

  (-> seq= (&rest t) boolean)
  (defun seq= (&rest xs)
    "Like `equal', but recursively compare sequences element-by-element.

Two elements X and Y are `seq=' if they are `equal', or if they are
both sequences of the same length and their elements are all `seq='."
    (declare (dynamic-extent xs))
    (match xs
      ((list) t)
      ((list _) t)
      (otherwise
       (loop for x in xs
             for y in (rest xs)
             always (%seq= x y))))))

(define-compiler-macro seq= (&whole call &rest xs)
  (match xs
    ((list) t)
    ((list x) `(progn ,x t))
    ((list x y) `(seq=/2 ,x ,y))
    (otherwise call)))
