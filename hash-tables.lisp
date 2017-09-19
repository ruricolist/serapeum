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
         (loop named ,loop do
           (multiple-value-bind (,found? ,key ,value)
               (,iterator)
             ,@decls
             (unless ,found?
               (return-from ,loop))
             ,@body))))))

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

(defun dict (&rest keys-and-values)
  "A concise constructor for hash tables.

    (gethash :c (dict :a 1 :b 2 :c 3)) => 3, T

By default, return an 'equal hash table containing each successive
pair of keys and values from KEYS-AND-VALUES.

If the number of KEYS-AND-VALUES is odd, then the first argument is
understood as the test.

     (gethash \"string\" (dict \"string\" t)) => t
     (gethash \"string\" (dict 'eq \"string\" t)) => nil"
  (let ((test (if (oddp (length keys-and-values))
                  (pop keys-and-values)
                  'equal)))
    (values (plist-hash-table keys-and-values :test test))))

(define-compiler-macro dict (&rest keys-and-values)
  (let ((test (if (oddp (length keys-and-values))
                  (pop keys-and-values)
                  ''equal)))
    (with-gensyms (ht)
      `(let ((,ht (make-hash-table
                   :test ,test
                   :size ,(max +hash-table-default-size+
                               (truncate (length keys-and-values)
                                         2)))))
         ,@(unsplice
            (when keys-and-values
              `(setf ,@(loop for (key value . nil) on keys-and-values by #'cddr
                             append `((gethash ,key ,ht) ,value)))))
         ,ht))))

(defun dict* (dict &rest args)
  "Merge new bindings into DICT.
Roughly equivalent to `(merge-tables DICT (dict args...))'."
  (doplist (k v args)
    (setf (gethash k dict) v))
  dict)

(defmacro dictq (&rest keys-and-values)
  "A literal hash table.
Like `dict', but the keys and values are implicitly quoted."
  (apply #'dict keys-and-values))

(defloop href (table &rest keys)
  "A concise way of doings lookups in (potentially nested) hash tables.

    (href (dict :x 1) :x) => x
    (href (dict :x (dict :y 2)) :x :y)  => y"
  (cond ((endp keys) table)
        ((single keys) (gethash (car keys) table))
        (t (apply #'href (gethash (car keys) table) (cdr keys)))))

(defun href-default (default table &rest keys)
  "Like `href', with a default.
As soon as one of KEYS fails to match, DEFAULT is returned."
  (nlet href (table keys)
    (cond ((endp keys)
           (values default nil))
          ((single keys)
           (multiple-value-bind (value ok?)
               (gethash (car keys) table)
             (if ok?
                 (values value t)
                 (values default nil))))
          (t (multiple-value-bind (value ok?)
                 (gethash (car keys) table)
               (if ok?
                   (href value (cdr keys))
                   (values default nil)))))))

(defun (setf href) (value table &rest keys)
  (nlet hset ((table table)
              (keys keys))
    (cond ((endp keys) value)
          ((single keys) (setf (gethash (car keys) table) value))
          (t (hset (gethash (car keys) table) (cdr keys))))))

(defun expand-href (table keys)
  (reduce
   (lambda (table key)
     `(gethash ,key ,table))
   keys
   :initial-value table))

(define-compiler-macro href (table &rest keys)
    (expand-href table keys))

(define-compiler-macro (setf href) (value table &rest keys)
  `(setf ,(expand-href table keys) ,value))

(defun @ (table &rest keys)
  "A concise way of doings lookups in (potentially nested) hash tables.

    (@ (dict :x 1) :x) => x
    (@ (dict :x (dict :y 2)) :x :y)  => y "
  (apply #'href table keys))

(defun (setf @) (value table key &rest keys)
  (nlet rec ((table table)
             (keys (cons key keys)))
    (if (single keys)
        (setf (gethash (car keys) table) value)
        (rec (gethash (car keys) table) (cdr keys)))))

(flet ((expand-@ (table keys)
         (reduce
          (lambda (table key)
            `(gethash ,key ,table))
          keys
          :initial-value table)))
  (define-compiler-macro @ (table key &rest keys)
      (expand-@ table (cons key keys)))
  (define-compiler-macro (setf @) (value table key &rest keys)
    `(setf ,(expand-@ table (cons key keys)) ,value)))

(defun pophash (key hash-table)
  "Lookup KEY in HASH-TABLE, return its value, and remove it.

This is only a shorthand. It is not in itself thread-safe.

From Zetalisp."
  (multiple-value-bind (value present?)
      (gethash key hash-table)
    (when present? (remhash key hash-table))
    (values value present?)))

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

(defun maphash-return (fn hash-table)
  "Like MAPHASH, but collect and return the values from FN.
From Zetalisp."
  (let ((fn (ensure-function fn)))
    ;; You were expecting nreverse? Hash tables are unordered.
    (hash-fold (lambda (k v prior)
                 (cons (funcall fn k v) prior))
               '()
               hash-table)))

;; Clojure
(defun merge-tables (table &rest tables)
  "Merge TABLE and TABLES, working from left to right.
The resulting hash table has the same parameters as TABLE.

If the same key is present in two tables, the value from the rightmost
table is used.

All of the tables being merged must have the same value for
`hash-table-test'.

Clojure's `merge'.
"
  (let ((size (max +hash-table-default-size+
                   (reduce #'+ tables
                           :key #'hash-table-count
                           :initial-value (hash-table-count table)))))
    (values
     (apply #'merge-tables!
            (copy-hash-table table :size size)
            tables))))

(defun merge-tables! (table &rest tables)
  (reduce (lambda (ht1 ht2)
            (check-same-test ht1 ht2)
            (do-hash-table (k v ht2)
              (setf (gethash k ht1) v))
            ht1)
          tables
          :initial-value table))

(defun flip-hash-table (table &key (test (constantly t)) (key #'identity))
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
     (gethash 'one negative-number-names) => -1, nil

KEY defaults to `identity'."
  (let ((table2 (copy-hash-table/empty table)))
    (ensuring-functions (key test)
      (do-hash-table (k v table)
        (let ((key (funcall key k)))
          (when (funcall test key)
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
    (with-key-fn (key)
      (if (not strict)
          (dolist (item set)
            (setf (gethash (funcall key item) table) item))
          ;; We can check for set-ness while building the hash table.
          (dolist (item set)
            (when (nth-value 1 (swaphash (funcall key item) item table))
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
                                            strict-types)
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
setting keys, as well as looking them up.

The function is able to restrict what types are permitted as keys and
values. If KEY-TYPE is specified, an error will be signaled if an
attempt is made to get or set a key that does not satisfy KEY-TYPE. If
VALUE-TYPE is specified, an error will be signaled if an attempt is
made to set a value that does not satisfy VALUE-TYPE. However, the
hash table provided is *not* checked to ensure that the existing
pairings KEY-TYPE and VALUE-TYPE -- not unless STRICT-TYPES is also
specified."
  (labels ((no-such-key (key)
             (error "Hash table ~a has no key ~a" hash-table key))
           (check-type* (datum type)
             (unless (typep datum type)
               (error 'type-error :expected-type type :datum type)))
           (wrap-hash-table (ht)
             (if read-only
                 (lambda (key)
                   (gethash key ht))
                 (lambda (key &optional (value nil value?))
                   (if value?
                       (setf (gethash key ht) value)
                       (gethash key ht)))))
           (wrap-strict (fun)
             (if (not strict) fun
                 (if read-only
                     (lambda (key)
                       (strict-lookup fun key))
                     (lambda (key &optional (value nil value?))
                       (if value?
                           (setf (strict-lookup fun key) value)
                           (strict-lookup fun key))))))
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
