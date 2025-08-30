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

(defsubst sequencep (x)
  "Is X a sequence?"
  (typep x 'sequence))

(-> null-if-empty ((or sequence array hash-table))
    (values (or sequence array hash-table) boolean &optional))
(defun null-if-empty (xs)
  "Return nil if XS is empty, XS otherwise.
If XS was empty the second value is nil; otherwise t.

This function also accepts multidimensional arrays. Arrays are
considered empty if their total size (from `array-total-size`) is
zero.

Hash tables are considered empty if their count is 0."
  (etypecase xs
    (hash-table
     (if (zerop (hash-table-count xs))
         (values nil nil)
         (values xs t)))
    (sequence
     (null-if xs 0 :test #'length=))
    ;; Handle multidimensional arrays.
    (array
     (if (eql (array-total-size xs) 0)
         (values nil nil)
         (values xs t)))))

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
           (error "Cannot supply both ~s and ~s to a sequence function"
                  :test :test-not))
          (test (canonicalize test))
          (test-not (complement (canonicalize test-not)))
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
          (with-two-arg-test (test)
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
  #+sbcl `(sb-int:dovector (,var ,vec ,@(unsplice return))
            ,@body)
  #-(or ccl sbcl) `(map nil (lambda (,var) ,@body) ,vec))

(define-do-macro %do-each ((var seq &optional return) &body body)
  "Only for Lisps that do not support extensible sequences."
  (once-only (seq)
    `(seq-dispatch ,seq
       (dolist (,var ,seq)
         ,@body)
       (do-vector (,var ,seq)
         ,@body))))

(define-do-macro do-each/map ((var seq &optional return) &body body)
  "The simple, out-of-line version."
  (with-thunk ((body :name do-each) var)
    `(map nil ,body ,seq)))

(defmacro do-each ((var seq &optional return) &body body &environment env)
  "Iterate over the elements of SEQ, a sequence.
If SEQ is a list, this is equivalent to `dolist'."
  (unless (speed-matters? env)
    (return-from do-each
      `(do-each/map (,var ,seq ,@(unsplice return))
         ,@body)))
  ;; We hoist the body and use sb-ext:muffle-conditions to prevent
  ;; SBCL from spamming us with code deletion notes. (It may also be
  ;; desirable in itself to avoid needless code duplication in Lisps
  ;; without type inference.)
  (with-thunk ((body :name do-each) var)
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
                                 &key start end from-end)
                            &body body)
  (declare #+sbcl (sb-ext:muffle-conditions style-warning))
  `(locally (declare (inline map-subseq))
     (map-subseq
      (lambda (,var)
        ,@body)
      ,seq
      ,start ,end
      ,from-end)))

;;; Define a protocol for accumulators so we can write functions like
;;; `assort', `partition', &c. generically.

(defmacro with-list-bucket ((seq) &body body)
  "Wrap BODY with inlined bucket accessors for lists."
  (declare (ignore seq))
  `(flet ((make-bucket (seq &optional (init nil initp))
            (declare (ignore seq))
            (if initp
                (queue init)
                (queue)))
          (bucket-push (seq item bucket)
            (declare (ignore seq)
                     (queue bucket))
            (enq item bucket))
          (bucket-seq (seq bucket)
            (declare (ignore seq)
                     (queue bucket))
            (qlist bucket)))
     (declare (ignorable #'make-bucket #'bucket-push #'bucket-seq))
     (declare (inline make-bucket bucket-push bucket-seq))
     ,@body))

(defmacro with-string-bucket ((seq) &body body)
  "Wrap BODY with inlined bucket accessors for strings."
  (declare (ignore seq))
  `(flet ((make-bucket (seq &optional (init nil initp))
            (let ((stream
                    (make-string-output-stream
                     :element-type (array-element-type seq))))
              (and initp (write-char init stream))
              stream))
          (bucket-push (seq item bucket)
            (declare (ignore seq))
            (write-char item bucket))
          (bucket-seq (seq bucket)
            (declare (ignore seq))
            (get-output-stream-string bucket)))
     (declare (ignorable #'make-bucket #'bucket-push #'bucket-seq))
     (declare (inline make-bucket bucket-push bucket-seq))
     ,@body))

(defmacro with-vector-bucket ((seq) &body body)
  "Wrap BODY with inlined bucket accessors for vectors (including strings)."
  `(if (stringp ,seq)
       (with-string-bucket (,seq)
         ,@body)
       (flet ((make-bucket (seq &optional (init nil initp))
                (with-boolean (initp)
                  (make-array (boolean-if initp 1 0)
                              :element-type (array-element-type seq)
                              :adjustable t
                              :fill-pointer (boolean-if initp 1 0)
                              :initial-contents (and initp (list init)))))
              (bucket-push (seq item bucket)
                (declare (ignore seq))
                (vector-push-extend item bucket))
              (bucket-seq (seq bucket)
                (declare (ignore seq))
                bucket))
         (declare (ignorable #'make-bucket #'bucket-push #'bucket-seq))
         (declare (inline make-bucket bucket-push bucket-seq))
         ,@body)))

(defmacro with-sequence-bucket ((seq) &body body)
  "Wrap BODY with inlined bucket accessors for generic sequences.

This might not seem worthwhile, and it's not for `bucket-seq', but for
`bucket-push' (and even `make-bucket') it is, since accumulating for
generic sequences just uses queues."
  (declare (ignore seq))
  `(flet ((make-bucket (seq &optional (init nil initp))
            (declare (ignore seq))
            (if initp
                (queue init)
                (queue)))
          (bucket-push (seq item bucket)
            (declare (ignore seq)
                     (queue bucket))
            (enq item bucket))
          (bucket-seq (seq bucket)
            (declare (queue bucket))
            (let ((len (qlen bucket)))
              (make-sequence-like seq len :initial-contents (qlist bucket)))))
     (declare (ignorable #'make-bucket #'bucket-push #'bucket-seq))
     (declare (inline make-bucket bucket-push bucket-seq))
     ,@body))

(defmacro with-specialized-buckets ((seq) &body body)
  "Ensure BODY is run with the appropriate specialized, inlined
versions of the bucket accessors.

This is only likely to be worthwhile around a loop; if you're calling
a bucket accessor once or twice the code bloat isn't worth it."
  (multiple-value-bind (body decls) (parse-body body)
    `(locally
         (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
       ,@decls
       (with-type-declarations-trusted ()
         (seq-dispatch ,seq
           (with-list-bucket (,seq)
             ,@body)
           (with-vector-bucket (,seq)
             ,@body)
           (with-sequence-bucket (,seq)
             ,@body))))))

;;; Fallback versions for non-specialized code.

(defun make-bucket (seq &optional (init nil initp))
  "Return a \"bucket\" suitable for collecting elements from SEQ.

If SEQ is restricted as to the type of elements it can hold (for
example, if SEQ is an array with an element type) the same restriction
will apply to the bucket."
  (with-specialized-buckets (seq)
    (if initp
        (make-bucket seq init)
        (make-bucket seq))))

(defun bucket-push (seq item bucket)
  "Insert ITEM at the end of BUCKET according to SEQ."
  (with-specialized-buckets (seq)
    (bucket-push seq item bucket)))

(defun bucket-seq (seq bucket)
  "Return a sequence \"like\" SEQ using the elements of BUCKET.

Note that it is not safe to call the function more than once on the
same bucket."
  (with-specialized-buckets (seq)
    (bucket-seq seq bucket)))

;;; Not currently used, but probably should be.
(defun bucket-append (seq items bucket)
  "Append ITEMS to the end of BUCKET according to SEQ.

The items will appear, together and in the same order, in the
sequence taken from the bucket by BUCKET-SEQ."
  (cond ((and (listp seq)
              (listp items))
         (qappend bucket items))
        ((stringp seq)
         (write-string items bucket))
        (t
         (map nil (lambda (item)
                    (bucket-push seq item bucket))
              items))))

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
         (let ((ret (make-bucket seq)))
           (with-item-key-function (key)
             (with-specialized-buckets (seq)
               (do-subseq (item seq nil :start start :end end :from-end from-end)
                 (when (pred (key item))
                   (bucket-push seq item ret)
                   (when (zerop (decf count))
                     (return))))))
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
  (declare (dynamic-extent pred))
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
The place designed by the first argument is set to the result of
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
  (declare (dynamic-extent key test))
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

(deftype single ()
  '(and sequence (satisfies single)))

(defun only-elt (seq)
  "Return the only element of SEQ.
If SEQ is empty, or contains more than one element, signal an error."
  (flet ((fail ()
           (error 'type-error
                  :expected-type 'single
                  :datum seq)))
    (declare (dynamic-extent #'fail))
    (seq-dispatch seq
      (if (and seq
               (endp (rest seq)))
          (first seq)
          (fail))
      (if (= (length seq) 1)
          (elt seq 0)
          (fail)))))

;;; TODO Export once you're sure of the name.
(defun rotation (seq n)
  "Like `rotate', but non-destructive."
  (rotate (copy-seq seq) n))

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
  (declare (dynamic-extent pred key))
  (fbind ((test (compose pred (canonicalize-key key))))
    (let ((pass (make-bucket seq))
          (fail (make-bucket seq)))
      (with-specialized-buckets (seq)
        (do-subseq (item seq nil :start start :end end)
          (if (test item)
              (bucket-push seq item pass)
              (bucket-push seq item fail))))
      (values (bucket-seq seq pass) (bucket-seq seq fail)))))

(defun partitions (preds seq &key (start 0) end (key #'identity))
  "Generalized version of PARTITION.

PREDS is a list of predicates. For each predicate, `partitions'
returns a filtered copy of SEQ. As a second value, it returns an extra
sequence of the items that do not match any predicate.

Items are assigned to the first predicate they match."
  (declare (dynamic-extent preds key))
  (with-item-key-function (key)
    (with-specialized-buckets (seq)
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
                (bucket-seq seq extra))))))

(defconstructor agroup
  "Auxiliary data structure for `assort'. A pair of an exemplar (to
compare against) and a bucket of matching items. Note that while the
agroup is immutable, the bucket itself is mutable."
  (exemplar t)
  (bucket t))

(defun assort (seq &key (key #'identity) (test #'eql) (start 0) end hash
               &aux (orig-test test))
  "Return SEQ assorted by KEY.

     (assort (iota 10)
             :key (lambda (n) (mod n 3)))
     => '((0 3 6 9) (1 4 7) (2 5 8))

Groups are ordered as encountered. This property means you could, in
principle, use `assort' to implement `remove-duplicates' by taking the
first element of each group:

     (mapcar #'first (assort list))
     ≡ (remove-duplicates list :from-end t)

However, if TEST is ambiguous (a partial order), and an element could
qualify as a member of more than one group, then it is not guaranteed
that it will end up in the leftmost group that it could be a member
of.

    (assort '(1 2 1 2 1 2) :test #'<=)
    => '((1 1) (2 2 1 2))

The default algorithm used by `assort' is, in the worst case, O(n) in
the number of groups. If HASH is specified, then a hash table is used
instead. However TEST must be acceptable as the `:test' argument to
`make-hash-table'."
  (declare (dynamic-extent key test))
  (fbind (test)
    (with-item-key-function (key)
      (with-boolean (hash)
        (let ((groups (queue))
              (table (boolean-if hash (make-hash-table :test orig-test) nil))
              last-group)
          (declare (ignorable table))
          (with-specialized-buckets (seq)
            (do-subseq (item seq nil :start start :end end)
              (let ((kitem (key item)))
                (if-let ((group
                             (if (match last-group
                                   ((agroup exemplar _)
                                    (test kitem exemplar)))
                                 last-group
                                 (boolean-if
                                  hash
                                  (gethash kitem table)
                                  (find-if
                                   (lambda (exemplar)
                                     (test kitem exemplar))
                                   (qlist groups)
                                   :key #'agroup-exemplar)))))
                  (progn
                    (setf last-group group)
                    (bucket-push seq item (agroup-bucket group)))
                  (let ((new-group (agroup kitem (make-bucket seq item))))
                    (boolean-when hash
                      (setf (gethash kitem table) new-group))
                    (enq new-group groups)))))
            (mapcar-into (lambda (group)
                           (bucket-seq seq (agroup-bucket group)))
                         (qlist groups))))))))

(defun list-runs (list start end key test count compare-last)
  (declare ((and fixnum unsigned-byte) count))
  (when (zerop count)
    (return-from list-runs nil))
  (fbind ((test (key-test key test)))
    (declare (dynamic-extent #'test))
    (with-boolean (compare-last)
      ;; This is a little more complicated than you might expect,
      ;; because we need to keep hold of the first element of each list.
      (let ((runs
              (nlet rec ((runs nil)
                         (count count)
                         (list
                          (nthcdr start
                                  (if end
                                      (ldiff list (nthcdr (- end start) list))
                                      list))))
                (if (endp list) runs
                    (let ((y (car list)))
                      (if (null runs)
                          (rec (list (list y))
                               count
                               (cdr list))
                          (let ((x (caar runs)))
                            (if (test x y)
                                (rec (cons
                                      (boolean-if compare-last
                                                  (cons y (car runs))
                                                  (list* x y (cdar runs)))
                                      (cdr runs))
                                     count
                                     (rest list))
                                (if (zerop (1- count))
                                    runs
                                    (rec (list*
                                          (list y)
                                          (boolean-if compare-last
                                                      (nreverse (car runs))
                                                      (cons (caar runs)
                                                            (nreverse (cdar runs))))
                                          (cdr runs))
                                         (1- count)
                                         (rest list)))))))))))
        (nreverse
         (boolean-if compare-last
                     (cons (nreverse (car runs)) (cdr runs))
                     (cons (cons (caar runs)
                                 (nreverse (cdar runs)))
                           (cdr runs))))))))

(defun runs (seq &key (start 0) end (key #'identity) (test #'eql) compare-last
                   (count most-positive-fixnum))
  "Return a list of runs of similar elements in SEQ.
The arguments START, END, and KEY are as for `reduce'.

    (runs '(head tail head head tail))
    => '((head) (tail) (head head) (tail))

By defualt, the function TEST is called with the first element of the
run as its first argument.

    (runs #(10 1 5 10 1) :test #'>)
    => (#(10 1 5) #(10))

COMPARE-LAST changes this behavior to test against the previous
element of the run:

    (runs #(10 1 5 10 1) :test #'> :compare-last t)
    (#(10 1) #(5) #(10))

The COUNT argument limits how many runs are returned.

    (runs '(head tail tail head head tail) :count 2)
    => '((head) (tail tail))

If COUNT is zero, `runs' returns an empty list. Otherwise, since
`runs' always returns a list of subsequences of SEQ, if SEQ is empty,
the return value will be a single-element list having the original
sequence as its only value:

    (runs \"\") -> '(\"\")
    (runs #())  -> '(#())
    (runs '())  -> '(())"
  (declare ((and fixnum unsigned-byte) count))
  (declare (dynamic-extent key test))
  (cond ((zerop count) (list))
        ((emptyp seq) (list seq))
        (t (seq-dispatch seq
             (list-runs seq start end key test count compare-last)
             (fbind ((test (key-test key test)))
               (declare (dynamic-extent #'test))
               (collecting*
                 (nlet runs ((start start)
                             (count count))
                   (when (plusp count)
                     (let* ((elt (elt seq start))
                            (run-end-pos
                              (position-if-not
                               (if compare-last
                                   (lambda (x)
                                     (when (test elt x)
                                       (setf elt x)
                                       t))
                                   (partial #'test elt))
                               seq
                               :start (1+ start)
                               :end end)))
                       (if (null run-end-pos)
                           (collect (subseq seq start end))
                           (progn
                             (collect (subseq seq start run-end-pos))
                             (runs run-end-pos (1- count)))))))))))))

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
      (seq-dispatch seq
        (let ((seq (nthcdr start seq)))
          (if (null end)
              (with-boolean (even)
                (loop while seq
                      collect (loop for i below n
                                    for (elt . rest) on seq
                                    collect elt
                                    finally (setf seq rest)
                                            (boolean-when even
                                              (unless (= i n)
                                                (uneven))))))
              (progn
                (check-bounds-even start end)
                (with-boolean (even)
                  (loop while seq
                        for i from start below end by n
                        collect
                        (loop with m = (min n (- end i))
                              for i below m
                              for (elt . rest) on seq
                              collect elt
                              finally (setf seq rest)
                                      (boolean-when even
                                        (unless (= i m)
                                          (uneven)))))))))
        (let ((end (or end (length seq))))
          (check-bounds-even start end)
          (nlet batches ((i start)
                         (acc '()))
            (if (>= i end)
                (nreverse acc)
                (batches (+ i n)
                         (cons (subseq seq i (min (+ i n) end))
                               acc)))))))))

(-> frequencies (sequence &key (:key function) &allow-other-keys)
  (values hash-table array-length))
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
    (with-item-key-function (key)
      (if (typep seq 'bit-vector)
          (with-subtype-dispatch bit-vector (simple-bit-vector) seq
            (setf (gethash (key 0) table) (count 0 seq)
                  (gethash (key 1) table) (count 1 seq)
                  total (length seq)))
          (do-each (elt seq)
            (incf total)
            (incf (gethash (key elt) table 0)))))
    (values table total)))

(defun scan (fn seq
              &rest args
              &key from-end
              (start 0)
              (end (length seq))
              (initial-value nil initial-value-supplied?)
              &allow-other-keys)
  "Return the partial reductions of SEQ.

Each element of the result sequence is the result of calling `reduce'
on the elements of the sequence up to that point (inclusively).

    (reduce #'+ '(1))       => 1
    (reduce #'+ '(1 2))     => 3
    (reduce #'+ '(1 2 3))   => 6
    (reduce #'+ '(1 2 3 4)) => 10
    (scan   #'+ '(1 2 3 4)) => '(1 3 6 10)

The result of calling `scan` on an empty sequence is always an empty
sequence, however.

    (reduce #'+ '()) => 0
    (scan   #'+ '()) => '()

This is sometimes called a \"prefix sum\", \"cumulative sum\", or
\"inclusive scan\".

From APL."
  (declare (dynamic-extent fn))
  (fbind (fn)
    (if (= start end)
        (if initial-value-supplied?
            ;; NB reduce does not apply the key to the initial value
            ;; if the sequence is empty.
            (list initial-value)
            (list))
        (collecting
          (collect
              (apply #'reduce
                     (if from-end
                         (lambda (x y)
                           (collect y)
                           (fn x y))
                         (lambda (x y)
                           (collect x)
                           (fn x y)))
                     seq
                     args))))))

(defsubst nub (seq &rest args &key start end key (test #'equal))
  "Remove duplicates from SEQ, starting from the end.
That means, for each duplicate, the first occurrence will be the kept, and subsequent occurrences will be discarded.

TEST defaults to `equal'.

From Haskell."
  (declare (ignore start end key))
  (apply #'remove-duplicates seq :from-end t :test test args))

(define-compiler-macro nub (seq &rest args &key (test '#'equal) &allow-other-keys)
  `(remove-duplicates ,seq :from-end t :test ,test ,@args))

(defun gcp (seqs &key (test #'eql))
  "The greatest common prefix of SEQS.

If there is no common prefix, return NIL."
  (if (emptyp seqs) nil
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
             seqs))))))

(defun gcs (seqs &key (test #'eql))
  "The greatest common suffix of SEQS.

If there is no common suffix, return NIL."
  (if (emptyp seqs) nil
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
             seqs))))))

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

(defmacro length-gt (cmp offset-fn seqs)
  `(nlet rec ((prev most-positive-fixnum)
              (seqs ,seqs))
     (if (endp seqs) t
         (destructuring-bind (seq . seqs) seqs
           (etypecase seq
             (array-length
              (and (,cmp prev seq)
                   (rec seq seqs)))
             (list
              (let ((len
                      ;; Get the length of SEQ, but only up to LAST.
                      (loop with len = 0
                            repeat (,offset-fn prev)
                            until (endp seq) do
                              (incf len)
                              (pop seq)
                            finally (return len))))
                (and (,cmp prev len)
                     (rec len seqs))))
             (sequence
              (let ((len (length seq)))
                (and (,cmp prev len)
                     (rec len seqs)))))))))

(defun length> (&rest seqs)
  "Is each length-designator in SEQS longer than the next?
A length designator may be a sequence or an integer."
  (length-gt > identity seqs))

(defun length>= (&rest seqs)
  "Is each length-designator in SEQS longer or as long as the next?
A length designator may be a sequence or an integer."
  (length-gt >= 1+ seqs))

(defun length< (&rest seqs)
  "Is each length-designator in SEQS shorter than the next?
A length designator may be a sequence or an integer."
  (declare (dynamic-extent seqs))
  (apply #'length> (reverse seqs)))

(defun length<= (&rest seqs)
  "Is each length-designator in SEQS as long or shorter than the next?
A length designator may be a sequence or an integer."
  (declare (dynamic-extent seqs))
  (apply #'length>= (reverse seqs)))

(defun longer (x y)
  "Return the longer of X and Y.

If X and Y are of equal length, return X.

If X and Y are lists, this will only traverse the shorter of X and Y."
  (check-type x sequence)
  (check-type y sequence)
  (cond ((and (listp x) (listp y))
         (nlet longer ((xs x)
                       (ys y))
           (cond ((and (endp xs) (endp ys)) x)
                 ((endp ys) x)
                 ((endp xs) y)
                 (t (longer (rest xs) (rest ys))))))
        (t (if (length>= x y) x y))))

(defun shorter (x y)
  "Return the shorter of X and Y."
  (cond ((and (listp x) (listp y))
         (nlet shorter ((xs x)
                        (ys y))
           (cond ((endp xs) x)
                 ((endp ys) y)
                 (t (shorter (rest xs) (rest ys))))))
        (t (if (length<= x y) x y))))

(defun shortest-seq (seqs)
  (extremum seqs #'<= :key #'length))

(defun longest-seq (seqs)
  (extremum seqs #'>= :key #'length))

(defun shortest-list (lists)
  "Find the shortest list in LISTS."
  (let ((pairs (map 'list
                    (lambda (list)
                      (cons list list))
                    lists)))
    (loop named ret do
      (dolist (pair pairs)
        (when (null (pop (car pair)))
          (return-from ret (cdr pair)))))))

(defun longest-list (lists)
  "Find the longest list in LISTS."
  (let ((lists (map 'list
                    (lambda (list)
                      (cons list list))
                    lists)))
    (nlet rec ((prev lists))
      (let ((maybe-longest (cdar prev))
            (lists
              (delete-if (lambda (cons)
                           (null (pop (car cons))))
                         prev)))
        (cond ((null lists) maybe-longest)
              ((single lists) (cdar lists))
              (t (rec lists)))))))

(defun shortest/longest (seqs extreme-seq extreme-list length-predicate)
  (fbind (extreme-seq extreme-list length-predicate)
    (cond ((emptyp seqs) nil)
          ((single seqs) (elt seqs 0))
          (t
           (multiple-value-bind (lists non-lists)
               (partition #'listp seqs)
             (cond ((emptyp lists) (extreme-seq non-lists))
                   ((emptyp non-lists) (extreme-list lists))
                   (t (let* ((extreme-seq (extreme-seq non-lists))
                             (extreme-list (extreme-list lists))
                             (leftmost
                               (find-if (lambda (item)
                                          (or (eql item extreme-list)
                                              (eql item extreme-seq)))
                                        seqs))
                             (rightmost
                               (if (eql leftmost extreme-list)
                                   extreme-seq
                                   extreme-list)))
                        ;; Make sure we return the first if there
                        ;; is no unique extremum.
                        (if (length-predicate rightmost leftmost) rightmost
                            leftmost)))))))))

(-> longest (sequence) sequence)
(defun longest (seqs)
  "Return the longest seq in SEQS.

If there are lists in SEQS, then the total number of conses traversed
will never exceed n*m, where n is the number of lists in SEQS and m
is the length of the next-to-longest list (unless the longest list is
not unique!)."
  (values
   (shortest/longest seqs #'longest-seq #'longest-list #'length>)))

(-> shortest (sequence) sequence)
(defun shortest (seqs)
  "Return the shortest seq in SEQS.

If there are lists in SEQS, then the total number of conses traversed
will never exceed n*m, where n is the number of lists in SEQS and m
is the length of the shortest list."
  (values
   (shortest/longest seqs #'shortest-seq #'shortest-list #'length<)))

(defsubst slice-bounds (len start end)
  "Normalize START and END, which may be negative, to offsets
acceptable to SUBSEQ."
  (declare (type signed-array-index start)
           (type signed-array-length end)
           (type array-index len))
  (let* ((start
           (if (minusp start)
               (max 0 (+ len start))
               (min start len)))
         (end
           (if (null end)
               nil
               (if (minusp end)
                   (max 0 (+ len end))
                   (min len end)))))
    (values start (max start end))))

(-> slice
    (sequence signed-array-index &optional (or null signed-array-length))
    sequence)
(defun slice (seq start &optional (end (length seq)))
  "Like `subseq', but allows negative bounds to specify offsets.
Both START and END accept negative bounds.

     (slice \"string\" -3 -1) => \"in\"

A call to `slice' where the first argument is positive and the second argument is negative is equivalent to chaining two calls to `drop':

    (drop 3 (drop -1 \"string\")) = \"in\"
    (slice \"string\" 3 -1)       = \"in\"

If the bounds cross in the middle, the result is an empty string:

    (slice \"x\" 1 -1) => \"\"

Note that `slice' implicitly clamps bounds, even when they are not negative:

    (slice \"x\" 0 100) => \"x\"

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
    (with-item-key-function (key)
      (with-boolean (from-end)
        (do-each (item seq)
          (boolean-if
           from-end
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
  #+sbcl (declare (sb-ext:muffle-conditions style-warning))
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
  #+sbcl (declare (sb-ext:muffle-conditions style-warning))
  (declare (type signed-array-length n))
  (seq-dispatch seq
    (if (minusp n)
        (butlast seq (abs n))
        (nthcdr n seq))
    (if (minusp n)
        (subseq seq 0 (max 0 (+ (length seq) n)))
        (subseq seq (min (length seq) n)))))

(-> take-while (function sequence &key (:from-end t)) sequence)
(defsubst take-while (pred seq &key from-end)
  "Return the prefix of SEQ for which PRED returns true.

    (take-while #'alpha-char-p \"Really!?\")
    => \"Really\"

If FROM-END is non-nil, return the suffix instead.

    (take-while (complement #'alpha-char-p) \"Really!?\" :from-end t)
    => \"!?\"

If PRED returns true for all elements of SEQ, the result is a sequence
with the same type and contents as SEQ."
  #+sbcl (declare (sb-ext:muffle-conditions style-warning))
  (flet ((list-take-while (pred list)
           (let ((pred (ensure-function pred)))
             (loop for x in list
                   while (funcall pred x)
                   collect x))))
    (declare (inline list-take-while))
    (seq-dispatch seq
      (if from-end
          (nreverse (list-take-while pred (reverse seq)))
          (list-take-while pred seq))
      (if from-end
          (let* ((start (position-if-not pred seq :from-end t))
                 (start
                   (if start
                       (1+ start)
                       0)))
            (subseq seq start))
          (let ((end (position-if-not pred seq)))
            (if end (subseq seq 0 end)
                seq))))))

(-> take-until (function sequence &key (:from-end t)) sequence)
(defsubst take-until (pred seq &key from-end)
  "Like `take-while' with the complement of PRED."
  (take-while (complement pred) seq :from-end from-end))

(-> drop-while (function sequence &key (:from-end t)) sequence)
(defsubst drop-while (pred seq &key from-end)
  "Return the largest possible suffix of SEQ for which PRED returns
false when called on the first element.

    (drop-while #'alpha-char-p \"Really!?\")
    => \"!?\"

If FROM-END is non-nil, then drop the longest possible suffix of SEQ
for which PRED returns true when called on the first element.

    (drop-while (complement #'alpha-char-p) \"Really!?\" :from-end t)
    => \"Really\"

If PRED returns true for all elements of SEQ, then the result is
always an empty sequence of the same type as SEQ."
  #+sbcl (declare (sb-ext:muffle-conditions style-warning))
  (seq-dispatch seq
    (if from-end
        (nreverse (member-if-not pred (reverse seq)))
        (member-if-not pred seq))
    (if from-end
        (let* ((end (position-if-not pred seq :from-end t))
               (end (if end (1+ end) 0)))
          (subseq seq 0 end))
        (let ((start (position-if-not pred seq)))
          (if start (subseq seq start)
              (subseq seq (length seq)))))))

(-> drop-until (function sequence &key (:from-end t)) sequence)
(defsubst drop-until (pred seq &key from-end)
  "Like `drop-while' with the complement of PRED."
  (drop-while (complement pred) seq :from-end from-end))

(-> drop-prefix (sequence sequence &key (:test (or symbol function)))
    sequence)
(defun drop-prefix (prefix seq &key (test #'eql))
  "If SEQ starts with PREFIX, remove it."
  (cond ((emptyp prefix) seq)
        ((starts-with-subseq prefix seq :test test)
         (drop (length prefix) seq))
        (t seq)))

(-> drop-suffix (sequence sequence &key (:test (or symbol function)))
  sequence)
(defun drop-suffix (suffix seq &key (test #'eql))
  "If SEQ ends with SUFFIX, remove it."
  (cond ((emptyp suffix) seq)
        ((ends-with-subseq suffix seq :test test)
         (drop (- (length suffix)) seq))
        (t seq)))

(-> ensure-prefix (sequence sequence &key (:test (or symbol function)))
  sequence)
(defun ensure-prefix (prefix seq &key (test #'eql))
  "Return a sequence like SEQ, but starting with PREFIX.
If SEQ already starts with PREFIX, return SEQ."
  (if (starts-with-subseq prefix seq :test test)
      seq
      (seq-dispatch seq
        (concatenate 'list prefix seq)
        (concatenate `(simple-array ,(array-element-type seq)
                                    (*))
                     prefix seq)
        (concatenate (type-of seq) prefix seq))))

(-> ensure-suffix (sequence sequence &key (:test (or symbol function)))
  sequence)
(defun ensure-suffix (seq suffix &key (test #'eql))
  "Return a sequence like SEQ, but ending with SUFFIX.
If SEQ already ends with SUFFIX, return SEQ."
  (if (ends-with-subseq suffix seq :test test)
      seq
      (seq-dispatch seq
        (concatenate 'list seq suffix)
        (concatenate `(simple-array ,(array-element-type seq)
                                    (*))
                     seq suffix)
        (concatenate (type-of seq) seq suffix))))

;;;# `bestn'
(defun bisect-left (vec item pred &key key (start 0) (end (length vec)))
  "Return the index in VEC to insert ITEM and keep VEC sorted.

If a value equivalent to ITEM already exists in VEC, then the index
returned is to the left of that existing item."
  (declare (array-length start end))
  (fbind (pred)
    (with-item-key-function (key)
      (with-vector-dispatch () vec
        (let ((kitem (key item)))
          (loop while (< start end) do
            (let ((mid (floor (+ start end) 2)))
              (if (pred (key (vref vec mid)) kitem)
                  (setf start (1+ mid))
                  (setf end mid)))
                finally (return start)))))))

(defun bisect-right (vec item pred &key key (start 0) (end (length vec)))
  "Return the index in VEC to insert ITEM and keep VEC sorted.

If a value equivalent to ITEM already exists in VEC, then the index
returned is to the right of that existing item."
  (declare (array-length start end))
  (fbind (pred)
    (with-item-key-function (key)
      (with-vector-dispatch () vec
        (let ((kitem (key item)))
          (loop while (< start end) do
            (let ((mid (floor (+ start end) 2)))
              (if (pred kitem (key (vref vec mid)))
                  (setf end mid)
                  (setf start (1+ mid))))
                finally (return start)))))))

(defun bestn (n seq pred &key (key #'identity) memo)
  "Partial sorting.
Equivalent to (take N (sort SEQ PRED)), but much faster, at least
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
        (t (fbind ((test (complement pred)))
             (let ((heap (make-heap :size n
                                    :key key
                                    :test #'test))
                   (i 0))
               (declare (array-length i))
               (with-item-key-function (key)
                 (do-each (elt seq)
                   (locally (declare (optimize speed))
                     (cond ((< i n)
                            (heap-insert heap elt))
                           ((test (key (heap-maximum heap)) (key elt))
                            (heap-extract-maximum heap)
                            (heap-insert heap elt))))
                   (incf i)))
               (let ((bestn (take n (nreverse (heap-extract-all heap)))))
                 (make-sequence-like seq n :initial-contents bestn)))))))

(defun nth-best (n seq pred &key (key #'identity))
  "Return the Nth-best element of SEQ under PRED.

Equivalent to

    (elt (sort (copy-seq seq) pred) n)

Or even

    (elt (bestn (1+ n) seq pred) n)

But uses a selection algorithm for better performance than either."
  (nth-best! n
             (copy-sequence 'vector seq)
             pred
             :key key))

(defun nth-best! (n seq pred &key (key #'identity))
  "Destructive version of `nth-best'.
Note that this function requires that SEQ be a vector."
  (check-type n array-index)
  (check-type seq vector)
  (if (zerop n)
      (extremum seq pred :key key)
      (let* ((pred (ensure-function pred))
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

(-> reshuffle (sequence &key (:element-type t))
    (values (simple-array * (*)) &optional))
(defun reshuffle (seq &key (element-type '*))
  "Like `alexandria:shuffle', but non-destructive.

Regardless of the type of SEQ, the return value is always a vector.

If ELEMENT-TYPE is provided, this is the element type (modulo
upgrading) of the vector returned.

If ELEMENT-TYPE is not provided, then the element type of the vector
returned is T, if SEQ is not a vector. If SEQ is a vector, then the
element type of the vector returned is the same as the as the element
type of SEQ."
  ;; TODO Would it be worthwhile to implement the "inside-out"
  ;; Fisher-Yates shuffle so we can shuffle and copy in one go?
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
    (with-item-key-function (key)
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
          (declare (dynamic-extent #'update-extrema)
                   (inline map-subseq))
          (map-subseq #'update-extrema seq start end))
        (values min max)))))

(-> split-at (list signed-array-index) (values list list))
(defun split-at (list k)
  (declare (list list)
           (optimize speed))
  (econd
    ((zerop k)
     (values list nil))
    ((minusp k)
     ;; Adapted from the definition of `butlast' in SBCL.
     (let* ((k (abs k))
            (head (nthcdr (1- k) list)))
       (if (or (endp head)
               (endp (cdr head)))
           (values nil list)
           (loop for trail on list
                 and head on head
                 ;; HEAD is n-1 conses ahead of TRAIL;
                 ;; when HEAD is at the last cons, return
                 ;; the data copied so far.
                 until (endp (cdr head))
                 collect (car trail) into copy
                 finally (return (values copy trail))))))
    ((plusp k)
     (nlet rec ((left '())
                (right list)
                (k k))
       (declare (array-index k))
       (if (or (zerop k) (endp right))
           (values (nreverse left) right)
           (rec (cons (car right) left)
                (cdr right)
                (1- k)))))))

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
the left). Note that providing a negative argument to a list works
similarly to `butlast' (a single traversal)."
  (declare ((or null signed-array-index) split))
  (flet ((halfway-point (seq)
           (ceiling (length seq) 2)))
    (seq-dispatch seq
      (split-at seq
                ;; If we know where to split in advance we only
                ;; have to traverse the list once.
                (or split
                    (halfway-point seq)))
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
  (let ((vec (dsu-sort-new seq fn :key key :stable stable)))
    (assert (length= vec seq))
    (replace seq vec)))

(defun dsu-sort-new (seq fn &key (key #'identity) stable)
  "Like `dsu-sort', but returning a new vector."
  (let* ((vec
           ;; Vectors sort faster.
           (map 'vector
                (with-item-key-function (key)
                  (lambda (item)
                    (cons (key item) item)))
                seq))
         (vec
           (funcall (if stable #'stable-sort #'sort)
                    vec fn
                    :key #'car)))
    (map-into vec #'cdr vec)))

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

(defun tsort/list (elts constraints tie-breaker &key (test #'eql))
  "Do the initial topological sort."
  ;; Adapted from AOMP.
  (declare (function test tie-breaker))
  (loop while elts
        for min-elts = (or (remove-if
                            (lambda (x)
                              (member x constraints
                                      :key #'second
                                      :test test))
                            elts)
                           (if (null elts)
                               (loop-finish)
                               (error 'inconsistent-graph
                                      :constraints constraints)))
        for choice = (if (null (rest min-elts))
                         (first min-elts)
                         (funcall tie-breaker min-elts (reverse results)))
        do (removef elts choice :test test)
           (removef constraints choice
                    :test
                    (lambda (x ys)
                      (member x ys :test test)))
        collect choice into results
        finally (return results)))

(defun tsort/hash-table (objects constraints tie-breaker &key (test #'eql))
  ;; Adapted from SBCL.
  (declare (list objects constraints)
           (function tie-breaker))
  (let ((obj-info (make-hash-table :size (length objects) :test test))
        (free-objs nil)
        (result nil))
    (loop for (obj1 obj2) in constraints do
      (incf (first (ensure-gethash obj2 obj-info (list 0))))
      (push obj2 (rest (ensure-gethash obj1 obj-info (list 0)))))
    (do-hash-table (obj info obj-info)
      (setf (gethash obj obj-info)
            (cons (car info)
                  (nreverse (cdr info)))))
    (dolist (obj objects)
      (let ((info (gethash obj obj-info)))
        (when (or (not info) (zerop (first info)))
          (push obj free-objs))))
    (loop
      (flet ((next-result (obj)
               (push obj result)
               (dolist (successor (rest (gethash obj obj-info)))
                 (let* ((successor-info (gethash successor obj-info))
                        (count (1- (first successor-info))))
                   (setf (first successor-info) count)
                   (when (zerop count)
                     (push successor free-objs))))))
        (cond ((endp free-objs)
               (do-hash-table (obj info obj-info)
                 (declare (ignore obj))
                 (unless (zerop (first info))
                   (error 'inconsistent-graph :constraints constraints)))
               (return (nreverse result)))
              ((endp (rest free-objs))
               (next-result (pop free-objs)))
              (t
               (let ((obj (funcall tie-breaker free-objs result)))
                 (removef free-objs obj)
                 (next-result obj))))))))

(defun tsort (elts constraints tie-breaker &key (test #'eql))
  (if (hash-table-test-p test)
      (tsort/hash-table elts constraints tie-breaker :test test)
      (tsort/list elts constraints tie-breaker :test test)))

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
  (let ((elts (remove-duplicates
               (apply #'append constraints)
               :test test)))
    (ordering (tsort elts constraints tie-breaker :test test)
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

(-> intersperse (t sequence) (values sequence &optional))
(defsubst intersperse (new-elt seq)
  "Return a sequence like SEQ, but with NEW-ELT inserted between each
element."
  #+sbcl (declare (sb-ext:muffle-conditions style-warning))
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
          (t (let ((tmps (make-gensym-list (length seeds) (string 'seed))))
               (with-gensyms (item)
                 (rebinding-functions (fn)
                   `(let ,(mapcar #'list tmps seeds)
                      ,(if from-end
                           `(do-subseq (,item ,seq (values ,@tmps) :from-end t)
                              (setf (values ,@tmps)
                                    (funcall ,fn ,item ,@tmps)))
                           `(do-each (,item ,seq (values ,@tmps))
                              (setf (values ,@tmps)
                                    (funcall ,fn ,@tmps ,item))))))))))))

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

(-> repeat-sequence (sequence (integer 0 *))
    (values sequence &optional))
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
  #-sbcl (check-type n (integer 0 *))
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

(define-compiler-macro repeat-sequence (&whole call seq n &environment env)
  (multiple-value-bind (seq constant?)
      (eval-if-constant seq env)
    (if (not constant?) call
        (match seq
          ((string c)
           `(make-string ,n
                         :initial-element ,c
                         :element-type ',(array-element-type seq)))
          ((vector x)
           `(make-array (list ,n)
                        :initial-element ,x
                        :element-type ',(array-element-type seq)))
          ((list x)
           `(make-list ,n :initial-element ',x))
          (otherwise call)))))

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
    (with-simple-vector-dispatch
        (simple-bit-vector (simple-array character (*)))
        (vec from to)
      (let ((out (make-array len-out :element-type (array-element-type vec))))
        (nlet rec ((n n) (offset from))
          (declare (array-index n offset))
          (if (zerop n)
              out
              (progn
                (replace out vec :start1 offset :start2 from :end2 to)
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

(defun map-splits (fn split-fn seq start end from-end)
  (if from-end
      (map-splits-from-end fn split-fn seq start end)
      (map-splits-from-start fn split-fn seq start end)))

(defun map-splits-from-start (fn split-fn seq start end)
  (fbind (fn)
    (let ((start (or start 0))
          (end (or end (length seq))))
      (loop with len = (length seq)
            for left = start then (1+ right)
            for right = (min (or (position-if split-fn seq
                                              :start left)
                                 len)
                             end)
            do (fn left right (/= right end))
            until (>= right end)))))

(defun map-splits-from-end (fn split-fn seq start end)
  (fbind (fn)
    (loop
      for right = end then left
      for left = (max (or (position-if split-fn
                                       seq
                                       :end right
                                       :from-end t)
                          -1)
                      (1- start))
      collect (fn left right (/= right end))
      until (< left start))))

(define-do-macro do-splits (((left right &optional not-at-end?)
                             (seq split-fn &key (start 0) end from-end)
                             &optional return)
                            &body body)
  "For each run of elements in SEQ that does not satisfy SPLIT-FN, call the body with LEFT bound to the start of the run and RIGHT bound to the end of the run.

If `split-sequence-if' did not exist, you could define a simple version trivially with `do-splits' and `collecting':

    (defun split-sequence-if (fn seq &key (start 0) end from-end)
      (collecting
        (do-splits ((l r) (seq fn :start start :end end :from-end from-end))
          (collect (subseq seq l r)))))

Providing NOT-AT-END? will bind it as a variable that is T if RIGHT is
not equal to END, and null otherwise. This can be useful when, in
processing a sequence, you want to replace existing delimiters, but do
nothing at the end.

In general `do-splits' will be found useful in situations where you
want to iterate over subsequences in the manner of `split-sequence',
but don't actually need to realize the sequences."
  (let ((c (or not-at-end? (gensym))))
    (with-unique-names (fn)
      `(flet ((,fn (,left ,right &optional ,c)
                (declare
                 (type array-index ,left)
                 (type array-length ,right)
                 (type boolean ,c)
                 ,@(unsplice (unless not-at-end? `(ignorable ,c))))
                ,@body))
         (declare (dynamic-extent #',fn))
         (map-splits #',fn ,split-fn ,seq ,start ,end ,from-end)))))

(defun list-collapse-duplicates (test key list)
  (declare (list list))
  (with-two-arg-test (test)
    (with-item-key-function (key)
      (nlet rec ((list list)
                 (acc '())
                 (last-result nil))
        (if (endp list) (nreverse acc)
            (if (null acc)
                (rec (cdr list)
                     (cons (car list) acc)
                     (key (car list)))
                (let ((new-result (key (car list))))
                  (if (test last-result new-result)
                      (rec (cdr list) acc last-result)
                      (rec (cdr list)
                           (cons (car list) acc)
                           new-result)))))))))

(defun collapse-duplicates (seq &key (key #'identity) (test #'eql))
  "Remove adjacent duplicates in SEQ.

Repetitions that are not adjacent are left alone.

    (remove-duplicates '(1 1 2 2 1 1)) => '(1 2)
    (collapse-duplicates  '(1 1 2 2 1 1)) => '(1 2 1)"
  (if (listp seq)
      (list-collapse-duplicates test key seq)
      (with-two-arg-test (test)
        (with-item-key-function (key)
          (with-specialized-buckets (seq)
            (let ((bucket (make-bucket seq))
                  (len (length seq)))
              (nlet rec ((i 0)
                         (last-key nil))
                (cond ((= i len)
                       (bucket-seq seq bucket))
                      ((= i 0)
                       (let ((elt (elt seq 0)))
                         (bucket-push seq elt bucket)
                         (rec 1 (key elt))))
                      (t
                       (let* ((elt (elt seq i))
                              (new-key (key elt)))
                         (if (test new-key last-key)
                             (rec (1+ i) last-key)
                             (progn
                               (bucket-push seq elt bucket)
                               (rec (1+ i) new-key)))))))))))))

(defun same (key-fn seq &key (test #'eql) (start 0) end)
  "Return true if KEY-FN returns the same value for any/all members of LIST."
  (fbind (key-fn)
    (with-two-arg-test (test)
      (let (init val)
        (do-subseq (item seq t :start start :end end)
          (if (null init)
              (setf val (key-fn item) init t)
              (unless (test val (key-fn item))
                (return-from same nil))))))))

(declaim (inline copy-enough-list nsplice-list splice-list))

(defun copy-firstn (list n)
  "Like COPY-LIST, but copies at most the first N conses of LIST. Handles cyclic
lists gracefully."
  (declare (optimize speed))
  (declare (type list list))
  (declare (type fixnum n))
  (cond ((= n 0) list)
        ((null list) list)
        (t (let ((result '())
                 (cons list))
             (dotimes (i n)
               ;; Push at most N new conses onto the stack.
               (push (car cons) result)
               (setf cons (cdr cons))
               ;; If at the end of a proper/dotted list, finish.
               (when (atom cons) (return)))
             ;; Nreverse the stack...
             (prog1 (nreverse result)
               ;; ...but set the last CDR to the shared tail.
               (setf (cdr result) cons))))))

(defun nsplice-list (list new start end)
  (declare (optimize speed))
  (declare (type list list))
  (declare (type (or null list) new))
  (declare (type alexandria:array-index start end))
  (let* ((temp-list (cons nil list))
         (cut-start (nthcdr start temp-list))
         (cut-end (nthcdr (- end start) (cdr cut-start))))
    (if (null new)
        (setf (cdr cut-start) cut-end)
        (setf (cdr cut-start) new
              (cdr (last new)) cut-end))
    (cdr temp-list)))

(defun splice-list (list new start end)
  (declare (optimize speed))
  (declare (type list list))
  (declare (type sequence new))
  (declare (type alexandria:array-index start end))
  (let ((queue (queue)))
    (declare (dynamic-extent queue))
    (loop repeat start do
      (enq (pop list) queue))
    (do-each (new-elt new)
      (enq new-elt queue))
    (qconc queue (nthcdr (- end start) list))
    (qlist queue)))

(declaim (inline nsplice-vector splice-vector))

(deftype array-size-difference ()
  `(integer ,(- array-dimension-limit) ,array-dimension-limit))

(defun nsplice-vector (vector new start end)
  (declare (type alexandria:array-index start end))
  (declare (type vector vector))
  (declare (type sequence new))
  (let ((diff-removed (- end start))
        (diff-added (length new)))
    (if (= diff-removed diff-added)
        (replace vector new :start1 start)
        (let* ((diff (- diff-added diff-removed))
               (length (length vector)))
          (declare (type array-size-difference diff))
          (cond ((not (adjustable-array-p vector))
                 ;; We must copy and overwrite the whole array.
                 (let ((result (make-array (+ length diff) :element-type
                                           (array-element-type vector))))
                   ;; Copy the beginning.
                   (replace result vector
                            :start1 0 :end1 start
                            :start2 0 :end2 start)
                   ;; Copy the new part.
                   (when new (replace result new :start1 start))
                   ;; Copy the end.
                   (replace result vector
                            :start1 (+ end diff) :end1 (length result)
                            :start2 end :end2 length)
                   result))
                ((plusp diff)
                 ;; The array is adjustable and we are making it longer.
                 ;; Adjust it and only then move the tail.
                 (adjust-array vector (+ length diff))
                 (replace vector vector
                          :start1 (+ end diff) :end1 (+ length diff)
                          :start2 end :end2 length)
                 (when new (replace vector new :start1 start))
                 vector)
                (t
                 ;; The array is adjustable and we are making it shorter.
                 ;; Move the tail and only then adjust it.
                 (replace vector vector
                          :start1 (+ end diff) :end1 (+ length diff)
                          :start2 end :end2 length)
                 (let ((new-len (+ length diff)))
                   (when (array-has-fill-pointer-p vector)
                     ;; The fill pointer has to be less than the new length.
                     (setf (fill-pointer vector)
                           (min (fill-pointer vector) new-len)))
                   (adjust-array vector new-len))
                 (when new (replace vector new :start1 start))
                 vector))))))

(defun splice-vector (vector new start end)
  (declare (type vector vector))
  (declare (type sequence new))
  (declare (type alexandria:array-index start end))
  (let ((diff-removed (- end start))
        (diff-added (length new))
        (vector (copy-seq vector)))
    (if (= diff-removed diff-added)
        (replace vector new :start1 start)
        (let* ((diff (- diff-added diff-removed))
               (length (length vector)))
          (declare (type array-size-difference diff))
          ;; We must copy and overwrite the whole array.
          (let ((result (make-array (+ length diff) :element-type
                                    (array-element-type vector))))
            ;; Copy the beginning.
            (replace result vector
                     :start1 0 :end1 start
                     :start2 0 :end2 start)
            ;; Copy the new part.
            (when new (replace result new :start1 start))
            ;; Copy the end.
            (replace result vector
                     :start1 (+ end diff) :end1 (length result)
                     :start2 end :end2 length)
            result)))))

;;; TODO: there is no extensible sequence support in SPLICE/NSPLICE right now.
;;;       Should there be? It doesn't seem like Serapeum is tested with
;;;       extensible sequences at all.

(declaim (inline splice-seq nsplice-seq))

(-> splice-seq (sequence &key
                         (:new sequence)
                         (:start alexandria:array-index)
                         (:end alexandria:array-index))
    (values sequence &optional))
(defun splice-seq (sequence &key new (start 0) (end (length sequence)))
  "Removes a part of SEQUENCE between START and END and replaces it with
contents of NEW (if provided). Does not modify SEQUENCE or NEW, but the result
is allowed to share structure with the original if SEQUENCE is a list.

    (splice-seq '(1 2 3 4 5) :new '(:a :b :c) :start 1 :end 1)
    => (1 :A :B :C 2 3 4 5)

    (splice-seq '(1 2 3 4 5) :new '(:a :b :c) :start 1 :end 4)
    => (1 :A :B :C 5)

Omitting NEW removes elements from SEQUENCE:

    (splice-seq '(1 2 3 4 5) :start 1 :end 3)
    => '(1 4 5)"
  (declare (type sequence sequence new))
  (if (and (= start end) (emptyp new))
      sequence
      (etypecase sequence
        (list (splice-list sequence new start end))
        (vector (splice-vector sequence new start end)))))

(-> nsplice-seq (sequence &key
                          (:new sequence)
                          (:start alexandria:array-index)
                          (:end alexandria:array-index))
    (values sequence &optional))
(defun nsplice-seq (sequence &key new (start 0) (end (length sequence)))
  "Removes a part of SEQUENCE between START and END and replaces it with
contents of NEW (if provided). SEQUENCE and NEW may be destroyed in the process
and the result is allowed to share structure with the original if SEQUENCE is a
list.

    (nsplice-seq (list 1 2 3 4 5) :new (list :a :b :c) :start 1 :end 1)
    => (1 :A :B :C 2 3 4 5)

    (nsplice-seq (list 1 2 3 4 5) :new (list :a :b :c) :start 1 :end 4)
    => (1 :A :B :C 5)

Omitting NEW removes elements from SEQUENCE:

    (nsplice-seq (list 1 2 3 4 5) :start 1 :end 3)
    => '(1 4 5)"
  (declare (type sequence sequence new))
  (if (and (= start end) (emptyp new))
      sequence
      (etypecase sequence
        (list (nsplice-list sequence (coerce new 'list) start end))
        (vector (nsplice-vector sequence new start end)))))

(define-modify-macro splice-seqf (&rest keyword-args) splice-seq
  "Modify macro for SPLICE-SEQ.")

(define-modify-macro nsplice-seqf (&rest keyword-args) nsplice-seq
  "Modify macro for NSPLICE-seq.")
