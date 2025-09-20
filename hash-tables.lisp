(in-package :serapeum)

(define-do-macro do-hash-table ((key value table &optional return)
                                &body body)
  "Iterate over hash table TABLE, in no particular order.

At each iteration, a key from TABLE is bound to KEY, and the value of
that key in TABLE is bound to VALUE."
  (with-unique-names (iterator loop found?)
    (multiple-value-bind (body decls) (parse-body body)
      `(with-hash-table-iterator (,iterator ,table)
         ;; Don't shadow the outer block.
         (loop :named ,loop :do
           (multiple-value-bind (,found? ,key ,value)
               (,iterator)
             ,@decls
             (unless ,found?
               (return-from ,loop))
             ,@body))))))

(defun hash-table-test-p (test)
  "Is TEST a valid hash table test?"
  (or (member test '(eq eql equal equalp) :test #'eq)
      (member (ensure-function test)
              (load-time-value (list #'eq #'eql #'equal #'equalp) t)
              :test #'eq)
      (values
       (ignore-errors
        (make-hash-table :test test)))))

(defconstant +hash-table-default-size+
  (hash-table-size (make-hash-table)))

(defun same-test? (ht1 ht2)
  "Do two hash tables have the same test?"
  (declare (hash-table ht1 ht2))
  (let ((test1 (hash-table-test ht1))
        (test2 (hash-table-test ht2)))
    (or (eql test1 test2)
        (eql (ensure-function test1)
             (ensure-function test2)))))

(defun check-same-test (ht1 ht2)
  "Check that HT1 and HT2, two hash tables, have the same value for
`hash-table-test'.

This should work even for \"custom\" hash tables with user-defined
tests."
  (unless (same-test? ht1 ht2)
    (error "Hash tables ~a and ~a have different tests."
           ht1 ht2)))

(defun copy-hash-table/empty
    (table
     &key (test (hash-table-test table))
          (size (hash-table-size table))
          (rehash-size (hash-table-rehash-size table))
          (rehash-threshold (hash-table-rehash-threshold table)))
  "Make a hash table with the same parameters (test, size, &c.) as
TABLE, but empty."
  (make-hash-table :test test :size size
                   :rehash-size rehash-size
                   :rehash-threshold rehash-threshold))

;; People rarely provide a :size argument to a hash table unless it’s
;; because they anticipate a very large hash table. This means that
;; code that uses hash tables often wastes memory. But most hash
;; tables made with `dict' are never going to in size, so we size them
;; to their contents.

(-> dict (&rest t) hash-table)
(defun dict (&rest keys-and-values)
  "A concise constructor for hash tables.

    (gethash :c (dict :a 1 :b 2 :c 3)) => 3, T

By default, return an 'equal hash table containing each successive
pair of keys and values from KEYS-AND-VALUES.

If the number of KEYS-AND-VALUES is odd, then the first argument is
understood as the test.

     (gethash \"string\" (dict \"string\" t)) => t
     (gethash \"string\" (dict 'eq \"string\" t)) => nil

Note that `dict' can also be used for destructuring (with Trivia).

    (match (dict :x 1)
      ((dict :x x) x))
    => 1"
  (let* ((length (length keys-and-values))
         (test (if (oddp length)
                   (pop keys-and-values)
                   'equal)))
    (values
     (plist-hash-table keys-and-values
                       :test test
                       :size (truncate length 2)))))

(define-compiler-macro dict (&rest keys-and-values)
  (let ((test (if (oddp (length keys-and-values))
                  (pop keys-and-values)
                  ''equal)))
    (with-gensyms (ht)
      `(let ((,ht (make-hash-table
                   :test ,test
                   :size ,(truncate (length keys-and-values) 2))))
         ,@(unsplice
            (when keys-and-values
              `(setf ,@(loop for (key value . nil) on keys-and-values by #'cddr
                             append `((gethash ,key ,ht) ,value)))))
         ,ht))))

(defpattern dict (&rest keys-and-values)
  "A destructuring pattern for hash tables."
  (multiple-value-bind (test keys-and-values)
      (if (oddp (length keys-and-values))
          (values (first keys-and-values)
                  (rest keys-and-values))
          (values nil keys-and-values))
    (with-unique-names (it)
      `(trivia:guard1 (,it :type hash-table)
                      (hash-table-p ,it)
                      ,@(and test
                             `((trivia:guard1 ,it
                                              (eql ',test (hash-table-test ,it)))))
                      ,@(loop for (k pat . nil) on keys-and-values by #'cddr
                              collect `(gethash ,k ,it)
                              collect pat)))))

(-> dict* (hash-table &rest t) hash-table)
(defun dict* (dict &rest args)
  "Merge new bindings into DICT.
Roughly equivalent to `(merge-tables* DICT (dict args...))'

DICT argument is modified and returned as the result."
  (doplist (k v args)
    (setf (gethash k dict) v))
  dict)

(defmacro dictq (&rest keys-and-values)
  "A literal hash table.
Like `dict', but the keys and values are implicitly quoted, and the
hash table is inlined as a literal object."
  (apply #'dict keys-and-values))

(-> href (hash-table &rest t) (values t boolean &optional))
(defloop href (table &rest keys)
  "A concise way of doing lookups in (potentially nested) hash tables.

    (href (dict :x 1) :x) => 1
    (href (dict :x (dict :y 2)) :x :y)  => 2"
  (match keys
    (() (values table t))
    ((list _) (gethash (car keys) table))
    (otherwise
     (apply #'href (gethash (car keys) table) (cdr keys)))))


(-> href-default (t hash-table &rest t) (values t boolean &optional))
(defun href-default (default table &rest keys)
  "Like `href', with a default.
As soon as one of KEYS fails to match, DEFAULT is returned."
  (nlet href (table keys)
    (match keys
      (()
       (values default nil))
      ((list _)
       (multiple-value-bind (value ok?)
           (gethash (car keys) table)
         (if ok?
             (values value t)
             (values default nil))))
      (otherwise
       (multiple-value-bind (value ok?)
           (gethash (car keys) table)
         (if ok?
             (href value (cdr keys))
             (values default nil)))))))

(defun (setf href) (value table &rest keys)
  (nlet hset ((table table)
              (keys keys))
    (match keys
      (() value)
      ((list _) (setf (gethash (car keys) table) value))
      (otherwise
       (hset (gethash (car keys) table) (cdr keys))))))

(defun expand-href (table keys)
  (reduce
   (lambda (table key)
     `(gethash ,key ,table))
   keys
   :initial-value table))

(define-compiler-macro href (table key &rest keys)
  (let* ((keys (cons key keys))
         (table-tmp (gensym (string 'table)))
         (key-tmps
           (make-gensym-list (length keys)
                             (string 'key))))
    `(let ((,table-tmp ,table)
           ,@(mapcar #'list key-tmps keys))
       ,(expand-href table-tmp key-tmps))))

(define-compiler-macro (setf href) (value table &rest keys)
  `(setf ,(expand-href table keys) ,value))

(defun @ (table &rest keys)
  "A concise way of doing lookups in (potentially nested) hash tables.

    (@ (dict :x 1) :x) => 1
    (@ (dict :x (dict :y 2)) :x :y)  => 2"
  (apply #'href table keys))

(defun (setf @) (value table key &rest keys)
  (nlet rec ((table table)
             (keys (cons key keys)))
    (if (rest keys)
        (rec (gethash (car keys) table) (cdr keys))
        (setf (gethash (car keys) table) value))))

(define-compiler-macro @ (table key &rest keys)
    `(href ,table ,key ,@keys))

(define-compiler-macro (setf @) (value table key &rest keys)
  `(setf (href ,table ,key ,@keys) ,value))

(-> pophash (t hash-table) (values t boolean &optional))
(defun pophash (key hash-table)
  "Lookup KEY in HASH-TABLE, return its value, and remove it.

This is only a shorthand. It is not in itself thread-safe.

From Zetalisp."
  (multiple-value-bind (value present?)
      (gethash key hash-table)
    (when present? (remhash key hash-table))
    (values value present?)))

(-> swaphash (t t hash-table) (values t boolean &optional))
(defun swaphash (key value hash-table)
  "Set KEY and VALUE in HASH-TABLE, returning the old values of KEY.

This is only a shorthand. It is not in itself thread-safe.

From Zetalisp."
  (multiple-value-prog1 (gethash key hash-table)
    (setf (gethash key hash-table) value)))

(declaim (inline hash-fold))
(defun hash-fold (fn init hash-table)
  "Reduce TABLE by calling FN with three values: a key from the hash
table, its value, and the return value of the last call to FN. On the
first call, INIT is supplied in place of the previous value.

From Guile."
  (let ((fn (ensure-function fn)))
    (with-hash-table-iterator (next hash-table)
      (let ((prior init))
        (loop (multiple-value-bind (more k v) (next)
                (if more
                    (setf prior (funcall fn k v prior))
                    (return prior))))))))

(-> maphash-return (function hash-table) t)
(defun maphash-return (fn hash-table)
  "Like MAPHASH, but collect and return the values from FN.
From Zetalisp."
  (let ((fn (ensure-function fn)))
    ;; You were expecting nreverse? Hash tables are unordered.
    (hash-fold (lambda (k v prior)
                 (cons (funcall fn k v) prior))
               '()
               hash-table)))

(-> maphash-new ((-> (t t) (values t t &optional))
                 hash-table &key &allow-other-keys)
  hash-table)
(defun maphash-new (fn hash-table &rest hash-table-args &key &allow-other-keys)
  "Like MAPHASH, but builds and returns a new hash table.

FN is a function of two arguments, like the function argument to
`maphash'. It is required, however, to return two values, a new key
and a new value.

If `copy-hash-table' did not exist, you could define it as:

    (maphash-new #'values hash-table)

Note it is not necessarily the case that the new hash table will have
the same number of entries as the old hash table, since FN might
evaluate to the same key more than once.

By default, the new hash table has the same hash table
properties (test, size) as HASH-TABLE, but these can be overridden
with HASH-TABLE-ARGS."
  (let ((fn (ensure-function fn)))
    (hash-fold (lambda (k v new)
                 (receive (new-k new-v)
                     (funcall fn k v)
                   (dict* new new-k new-v))
                 new)
               (apply #'copy-hash-table/empty hash-table hash-table-args)
               hash-table)))

(-> maphash-into (hash-table function &rest sequence)
    hash-table)
(defun maphash-into (hash-table fn &rest seqs)
  "Map FN over SEQS, updating HASH-TABLE with the results. Return HASH-TABLE.

FN is required to return two values, and key and a value."
  (prog1 hash-table
    ;; Is this worth having a compiler macro for, so `map' is called
    ;; with a fixed number of values?
    (let ((fn (ensure-function fn)))
      (apply #'map nil
             (lambda (&rest args)
               (receive (k v) (apply fn args)
                 (setf (gethash k hash-table) v)))
             seqs))))

;; Clojure
(defun merge-tables* (table &rest tables)
  "Modifies the first TABLE by merging all keys from other TABLES into it.

Returns the first TABLE."
  (reduce (lambda (ht1 ht2)
            (check-same-test ht1 ht2)
            (do-hash-table (k v ht2)
              (setf (gethash k ht1) v))
            ht1)
          tables
          :initial-value table))

(defun merge-tables (&rest tables)
  "Merge TABLES, working from left to right.
The resulting hash table has the same parameters as the first table.

If no tables are given, an new, empty hash table is returned.

If a single table is given, a copy of it is returned.

If the same key is present in two tables, the value from the rightmost
table is used.

All of the tables being merged must have the same value for
`hash-table-test'.

Clojure's `merge'.
"
  (match tables
    ((list) (make-hash-table))
    ((list table) (copy-hash-table table))
    ((list* table tables)
     (let ((size (max +hash-table-default-size+
                      (reduce #'+ tables
                              :key #'hash-table-count
                              :initial-value (hash-table-count table)))))
       (values
        (apply #'merge-tables*
               (copy-hash-table table :size size)
               tables))))))

(defun flip-hash-table (table &rest hash-table-args
                        &key (filter (constantly t)) (key #'identity)
                          test size rehash-size rehash-threshold)
  "Return a table like TABLE, but with keys and values flipped.

     (gethash :y (flip-hash-table (dict :x :y)))
     => :x, t

TEST allows you to filter which keys to set.

     (def number-names (dictq 1 one 2 two 3 three))

     (def name-numbers (flip-hash-table number-names))
     (def name-odd-numbers (flip-hash-table number-names :filter #'oddp))

     (gethash 'two name-numbers) => 2, t
     (gethash 'two name-odd-numbers) => nil, nil

KEY allows you to transform the keys in the old hash table.

     (def negative-number-names (flip-hash-table number-names :key #'-))
     (gethash 'one negative-number-names) => -1, t

KEY defaults to `identity'."
  (declare (ignore test size rehash-size rehash-threshold))
  (let ((table2 (apply #'copy-hash-table/empty table
                       (remove-from-plist hash-table-args
                                          :filter :key))))
    (ensuring-functions (key filter)
      (do-hash-table (k v table)
        (let ((key (funcall key k)))
          (when (funcall filter key)
            (setf (gethash v table2) key)))))
    table2))

(defun set-hash-table (set &rest hash-table-args &key (test #'eql)
                                                   (key #'identity)
                                                   (strict t)
                       &allow-other-keys)
  "Return SET, a list considered as a set, as a hash table.
This is the equivalent of Alexandria's `alist-hash-table' and
`plist-hash-table' for a list that denotes a set.

STRICT determines whether to check that the list actually is a set.

The resulting hash table has the elements of SET for both its keys and
values. That is, each element of SET is stored as if by
     (setf (gethash (key element) table) element)"
  (let* ((hash-table-args
           (remove-from-plist hash-table-args
                              :key :test :strict))
         ;; Use multiple-value-call so the provided arguments override
         ;; the defaults.
         (table (multiple-value-call #'make-hash-table
                  (values-list hash-table-args)
                  :test test
                  :size (length set))))
    (with-item-key-function (key)
      (if (not strict)
          (dolist (item set)
            (setf (gethash (key item) table) item))
          ;; We can check for set-ness while building the hash table.
          (dolist (item set)
            (when (nth-value 1 (swaphash (key item) item table))
              (error "Not a set: ~a" set)))))
    table))

(defun hash-table-set (table &key (strict t) (test #'eql) (key #'identity))
  "Return the set denoted by TABLE.
Given STRICT, check that the table actually denotes a set.

Without STRICT, equivalent to `hash-table-values'."
  (let ((set (hash-table-values table)))
    (when strict
      (unless (setp set :test test :key key)
        (error "Not a set: ~a" set)))
    set))

(defun hash-table-predicate (hash-table)
  "Return a predicate for membership in HASH-TABLE.
The predicate returns the same two values as `gethash', but in the
opposite order."
  (check-type hash-table hash-table)
  (lambda (x)
    (multiple-value-bind (val val?)
        (gethash x hash-table)
      (values val? val))))

(defun hash-table-function (hash-table &key read-only strict (key-type 't) (value-type 't)
                                         strict-types default)
  "Return a function for accessing HASH-TABLE.

Calling the function with a single argument is equivalent to `gethash'
against a copy of HASH-TABLE at the time HASH-TABLE-FUNCTION was
called.

    (def x (make-hash-table))

    (funcall (hash-table-function x) y)
    ≡ (gethash y x)

If READ-ONLY is nil, then calling the function with two arguments is
equivalent to `(setf (gethash ...))' against HASH-TABLE.

If STRICT is non-nil, then the function signals an error if it is
called with a key that is not present in HASH-TABLE. This applies to
setting keys, as well as looking them up. Pass `:strict :read` if you
only want strict checking for lookups.

DEFAULT is the default value to return from `gethash'.

The function is able to restrict what types are permitted as keys and
values. If KEY-TYPE is specified, an error will be signaled if an
attempt is made to get or set a key that does not satisfy KEY-TYPE. If
VALUE-TYPE is specified, an error will be signaled if an attempt is
made to set a value that does not satisfy VALUE-TYPE. However, the
hash table provided is *not* checked to ensure that the existing
pairings satisfy KEY-TYPE and VALUE-TYPE -- not unless STRICT-TYPES is
also specified."
  (labels ((no-such-key (key)
             (error "Hash table ~a has no key ~a" hash-table key))
           (check-type* (datum type)
             (unless (typep datum type)
               (error 'type-error :expected-type type :datum type)))
           (wrap-hash-table (ht)
             (if read-only
                 (lambda (key)
                   (gethash key ht default))
                 (lambda (key &optional (value nil value?))
                   (if value?
                       (setf (gethash key ht) value)
                       (gethash key ht default)))))
           (wrap-strict (fun)
             (symbol-macrolet
                 ((strict-reader
                    (lambda (key)
                      (strict-lookup fun key))))
               (case strict
                 ((nil) fun)
                 (:read
                  (if read-only
                      strict-reader
                      (lambda (key &optional (value nil value?))
                        (if value?
                            (funcall fun key value)
                            (strict-lookup fun key)))))
                 (otherwise
                  (if read-only
                      strict-reader
                      (lambda (key &optional (value nil value?))
                        (if value?
                            (setf (strict-lookup fun key) value)
                            (strict-lookup fun key))))))))
           (strict-lookup (fun key)
             (multiple-value-bind (value present?)
                 (funcall fun key)
               (if present?
                   (values value t)
                   (no-such-key key))))
           ((setf strict-lookup) (value fun key)
             (multiple-value-bind (old-val present?)
                 (funcall fun key)
               (declare (ignore old-val))
               (if present?
                   (funcall fun key value)
                   (no-such-key key))))
           (wrap-key-type (fun type)
             (if (type= type t) fun
                 (lambda (key &rest args)
                   (check-type* key type)
                   (apply fun key args))))
           (wrap-value-type (fun type)
             (if (type= type t) fun
                 (if read-only fun
                     (lambda (key &optional (value nil value?))
                       (if (not value?)
                           (funcall fun key)
                           (progn
                             (check-type* value type)
                             (funcall fun key value))))))))
    (when strict-types
      (unless (and (type= key-type t)
                   (type= value-type t))
        (do-hash-table (k v hash-table)
          (check-type* k key-type)
          (check-type* v value-type))))
    (assure function
      (~> hash-table
          copy-hash-table
          wrap-hash-table
          wrap-strict
          (wrap-key-type key-type)
          (wrap-value-type value-type)))))

(defun make-hash-table-function (&rest args &key &allow-other-keys)
  "Call `hash-table-function' on a fresh hash table.
ARGS can be args to `hash-table-function' or args to
`make-hash-table', as they are disjoint."
  (apply #'hash-table-function
         (apply #'make-hash-table :allow-other-keys t args)
         :allow-other-keys t args))

(defun delete-from-hash-table (table &rest keys)
  "Return TABLE with KEYS removed (as with `remhash').
Cf. `delete-from-plist' in Alexandria."
  (prog1 table
    (dolist (key keys)
      (remhash key table))))

(-> pairhash (sequence sequence &optional (or null hash-table))
  hash-table)
(defun pairhash (keys data &optional hash-table)
  "Like `pairlis', but for a hash table.

Unlike `pairlis', KEYS and DATA are only required to be sequences (of
the same length), not lists.

By default, the hash table returned uses `eql' as its tests. If you
want a different test, make the table yourself and pass it as the
HASH-TABLE argument."
  (let ((hash-table
          (or hash-table
              (make-hash-table :size (max (length keys)
                                          +hash-table-default-size+)))))
    (declare (hash-table hash-table))
    (unless (length= keys data)
      (error "Arguments to pairhash must be of the same length."))
    (map nil
         (lambda (key datum)
           (setf (gethash key hash-table) datum))
         keys data)
    hash-table))

(defun pretty-print-hash-table (ht &optional (stream *standard-output*))
  "Pretty print the hash-table HT to STREAM.

```
\(pretty-print-hash-table (dict :a 1 :b 2 :c 3))
;; =>
\(dict
  :A 1
  :B 2
  :C 3
 )
```

If you want to always pretty print hash tables, you can set this in your init file:

``` lisp
\(toggle-pretty-print-hash-table)
```

  Ported from RUTILS."
  (let ((*print-pretty* t)
        (i 0))
    (pprint-logical-block (stream nil)
      (pprint-newline :fill stream)
      (format stream "(~s " 'dict)
      (unless (eq (hash-table-test ht) 'equal)
        (princ #\' stream)
        (princ (hash-table-test ht) stream))
      (pprint-indent :block 2 stream)
      (block nil
        (maphash (lambda (k v)
                   (pprint-newline :mandatory stream)
                   (when (and *print-length* (> (incf i) *print-length*))
                     (princ "..." stream)
                     (return))
                   (when (and k (listp k)) (princ #\' stream))
                   (if (typep k 'hash-table)
                       (pretty-print-hash-table k stream)
                       (prin1 k stream))
                   (princ " " stream)
                   (when (and v (listp v)) (princ #\' stream))
                   (if (typep v 'hash-table)
                       (pretty-print-hash-table v stream)
                       (prin1 v stream)))
                 ht))
      (pprint-indent :block 1 stream)
      (pprint-newline :mandatory stream)
      (princ ") " stream)))
  ht)

(let (toggled)
  (defun toggle-pretty-print-hash-table (&optional (on nil explicit))
    "Toggles printing hash-tables with PRETTY-PRINT-HASH-TABLE or with the default method.
    If ON is set explicitly, turn on literal printing (T), otherwise use the default (NIL).

    Ported from RUTILS."
    (declare (notinline flip))          ;phasing
    (let ((off (if explicit on (not toggled))))
      (if off
          (progn
            (set-pprint-dispatch 'hash-table (flip #'pretty-print-hash-table))
            (setf toggled t))
          (progn (set-pprint-dispatch 'hash-table nil)
                 (setf toggled nil))))))
