(in-package :serapeum)

(export '(in
          filter-map
          memq delq
          append1
          assocdr assocadr rassocar
          firstn
          toposort
          inconsistent-graph
          inconsistent-graph-constraints
          powerset
          efface
          pop-assoc
          mapply
          car-safe cdr-safe
          mapcar-into
          nthrest
          deltas))

(-> riffle (&rest list) list)
(-> firstn ((integer 0 *) list) list)
(-> intersperse (t list) list)
(-> powerset (list) list)
(-> efface (t list) list)
(-> mapcar-into ((or function symbol) list) list)
(-> nthrest ((integer 0 *) list) list)

(defun filter-map (fn list &rest lists)
  "Map FN over LISTS like `mapcar', but omit empty results.

     (filter-map fn ...)
     ≅ (remove nil (mapcar fn ...))"
  (let ((fn (ensure-function fn)))
    (apply #'mapcan
           (lambda (&rest args)
             (unsplice (apply fn args)))
           list lists)))

(define-compiler-macro filter-map (fn list &rest lists)
  (let* ((lists (cons list lists))
         (gs (loop for nil in lists collect (gensym))))
    (with-gensyms (f result)
      `(let ((,f (ensure-function ,fn)))
         (loop ,@(loop for g in gs
                       for list in lists
                       append `(for ,g in ,list))
               for ,result = (funcall ,f ,@gs)
               if ,result collect ,result)))))

(defun car-safe (x)
  "The car of X, or nil if X is not a cons.

This is different from Alexandria’s `ensure-car`, which returns the atom.

    (ensure-car '(1 . 2)) => 1
    (car-safe '(1 . 2)) => 1
    (ensure-car 1) => 1
    (car-safe 1) => nil

From Emacs Lisp."
  (if (consp x) (car x) nil))

(defun cdr-safe (x)
  "The cdr of X, or nil if X is not a cons.
From Emacs Lisp."
  (if (consp x) (cdr x) nil))

;; From Q.
(defsubst enlist (x)
  (if (listp x) x (list x)))

(defsubst mklist (x)
  "If X is a list, return it; otherwise, return (list x)."
  (if (listp x) x (list x)))

(defsubst append1 (list item)
  "Append an atom to a list.

    (append1 list item)
    ≡ (append list (list item))"
  (append list (list item)))

(defun in (x &rest items)
  "Is X equal to any of ITEMS?

`(in x xs...)` is always equivalent to `(member x xs :test equal)`,
but `in` can sometimes compile to more efficient code when the
candidate matches are constant.

From Arc."
  (declare (inline member)
           (dynamic-extent items))
  (member x items :test #'equal))

(define-compiler-macro in (x &rest items)
  (once-only (x)
    (cond ((every (disjoin #'keywordp #'numberp #'characterp)
                  items)
           `(case ,x ((,@items) t)))
          ((every #'stringp items)
           `(string-case ,x
              ((,@items) t)))
          (t `(or ,@(loop for item in items
                          collect `(equal ,x ,item)))))))

(defun memq (item list)
  "Like (member ... :test #'eq).
Should only be used for symbols."
  (declare (list list) (optimize speed))
  (loop for tail on list
        if (eq item (car tail))
          return tail))

(defun delq (item list)
  "Like (delete ... :test #'eq), but only for lists.

Almost always used as (delq nil ...)."
  (declare (list list) (optimize speed))
  #+ () (cond ((endp list) nil)
              ((eq (car list) item) (cdr list))
              (t (rplacd list (delq item (cdr list)))))
  (let ((splice '()))
    (loop for x = list then (cdr x) do
      (cond ((endp x) (return list))
            ((eq (car x) item)
             (if (null splice)
                 (setf list (cdr x))
                 (setf (cdr splice) (cdr x))))
            (t (setf splice x))))))

(let* ((list1 (list 'x 'y nil 'z))
       (list2 (delq nil list1)))
  (assert (not (memq nil list2)))
  (assert (eq list1 list2)))

(defun mapply (fn &rest lists)
  "`mapply` is a cousin of `mapcar`.

If you think of `mapcar` as using `funcall`:

    (mapcar #'- '(1 2 3))
    ≡ (loop for item in '(1 2 3)
             collect (funcall #'- item))

Then `mapply` does the same thing, but using `apply`.

    (mapply #'+ '((1 2 3) (4 5 6)))
    => (6 15)

In variadic use, `mapply` acts as if `mapcar #'append` had first been
used:

    (mapply #'+ xs ys)
    ≡ (mapply #'+ (mapcar #'append xs ys))

But the actual implementation is more efficient."
  (apply #'mapcar
         (lambda (&rest args)
           (apply fn (apply #'append args)))
         lists))

(define-compiler-macro mapply (fn &rest lists)
  "Translate from apply to multiple-value-call."
  (let ((vars (loop for nil in lists collect (string-gensym 'arg))))
    (with-gensyms (gfn)
      `(let ((,gfn (ensure-function ,fn)))
         (mapcar
          (lambda (,@vars)
            (multiple-value-call ,gfn
              ,@(loop for var in vars
                      collect `(values-list ,var))))
          ,@lists)))))

(defsubst assocdr (item alist &rest args)
  "Like (cdr (assoc ...))"
  (let ((found (apply #'assoc item alist args)))
    (values (cdr found) found)))

(defsubst assocadr (item alist &rest args)
  "Like `assocdr' for alists of proper lists.

     (assocdr 'x '((x 1))) => '(1)
     (assocadr 'x '((x 1))) => 1"
  (let ((found (apply #'assoc item alist args)))
    (values (cadr found) found)))

(defsubst rassocar (item alist &rest args)
  "Like (car (rassoc ...))"
  (let ((found (apply #'rassoc item alist args)))
    (values (car found) found)))

(defun firstn (n list)
  "The first N elements of LIST, as a fresh list:

    (firstn 4 (iota 10))
    => (0 1 2 4)

\(I do not why this extremely useful function did not make it into
Common Lisp, unless it was deliberately left out as an exercise for
Maclisp users.)"
  ;; NB This is faster than the DO version in the Pitmanual.
  (loop repeat n for x in list collect x))

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

(defun toposort (elts constraints
                 &key (key #'identity)
                      (tie-breaker (constantly nil) tie-breaker-supplied-p))
  "The topographical sort routine from AMOP.

Takes a list of elements to sort, and a list of constraints, where each
constraint is a two-element list.

    (def dem-bones '((toe foot)
                     (foot heel)
                     (heel ankle)
                     (ankle shin)
                     (shin knee)
                     (knee back)
                     (back shoulder)
                     (shoulder neck)
                     (neck head)))
    (toposort (shuffle (mapcar #'car dem-bones))
              dem-bones)
    => (TOE FOOT HEEL ANKLE SHIN KNEE BACK SHOULDER NECK)

If the graph is inconsistent, signals an error of type
`inconsistent-graph`:

    (toposort '(chicken egg) '((chicken egg) (egg chicken)))
    => Inconsistent graph: ((CHICKEN EGG) (EGG CHICKEN))"
  ;; Adapted from AMOP.
  (fbind ((key key) (tie-breaker tie-breaker))
    (nlet tsort ((elts elts)
                 (constraints constraints)
                 (result '()))
      (if elts
          (let ((min-elts (remove-if
                           (lambda (x)
                             (member (key x) constraints
                                     :key #'second))
                           elts)))
            (if (not min-elts)
                (error 'inconsistent-graph
                       :constraints constraints)
                (let ((choice (if (not (rest min-elts))
                                  (first min-elts)
                                  (if tie-breaker-supplied-p
                                      (tie-breaker min-elts (reverse result))
                                      (first min-elts)))))
                  (tsort (remove choice elts)
                         (remove (key choice)
                                 constraints
                                 :test #'member)
                         (cons choice result)))))
          (nreverse result)))))

(defun intersperse (new-elt list)
  "Insert NEW-ELT between each element of LIST."
  (cons (car list)
        (mapcan (lambda (x)
                  (list x new-elt))
                (cdr list))))

(defun intercalate (new-list list)
  "Splice the elements of NEW-LIST between each element of LIST."
  (append (car list)
          (mapcan
           (curry #'append new-list)
           (cdr list))))

(defun delete-dups (list)
  "Destructively remove duplicates from a list, starting from the end,
testing with #'equal.

Equivalent to Emacs's `delete-dups`."
  (declare (list list) (inline delete-duplicates))
  (delete-duplicates list :test #'equal :from-end t))

(defun powerset (set)
  "Return the powerset of SET.
Uses a non-recursive algorithm."
  (unless (setp set)
    (error "Not a set: ~a" set))
  (loop for i below (expt (length set) 2)
        collect (loop for j from 0
                      for x in set
                      when (logbitp j i)
                        collect x)))

(defmacro drain (expr &optional (eof nil))
  "Collect values for EXPR until one equals EOF."
  (with-gensyms (res)
    `(loop for ,res = ,expr
           until (eql ,res ,eof)
           collect ,res)))

(defmacro draining ((var expr &optional eof) &body body)
  (with-gensyms (res)
     `(loop for ,res = ,expr
            until (eql ,res ,eof)
            collect (let ((,var ,res))
                      ,@body))))

(defun efface (item list)
  "Destructively remove only the first occurence of ITEM in LIST.

From Lisp 1.5."
  (let ((splice '()))
    (loop for x = list then (cdr x) do
      (cond ((endp x) (return list))
            ((eql (car x) item)
             (if (null splice)
                 (return (cdr x))
                 (progn
                   (setf (cdr splice) (cdr x))
                   (return list))))
            (t (setf splice x))))))

(defmacro pop-assoc (key alist &rest args &environment env)
  "Like `assoc' but, if there was a match, delete it from ALIST.

From Newlisp."
  (multiple-value-bind (vars vals new setter getter)
      (get-setf-expansion alist env)
    (with-gensyms (match)
      `(let* (,@(mapcar #'list vars vals)
              (,(car new) ,getter))
         (let ((,match (assoc ,key ,(car new) ,@args)))
           (declare (list ,match))
           (prog1 ,match
             (when ,match
               (setf ,(car new) (efface ,match ,(car new))))
             ,setter))))))

(defsubst mapcar-into (fn list)
  "Like (map-into list fn list).

From PAIP."
  (let ((fn (ensure-function fn)))
    (loop for tail on list
          do (rplaca tail (funcall fn (car tail)))
          finally (return list))))

(defsubst nthrest (n list)
  "Alias for `nthcdr'."
  (nthcdr n list))

(defun deltas (list &optional (op #'-))
  "Return the successive differences in LIST.

     (deltas '(4 9 -5 1 2))
     => '(4 5 -14 6 1)

From Q."
  (cons (car list)
        (mapcar op (cdr list) list)))

(assert
 (equal '(4 5 -14 6 1)
        (deltas '(4 9 -5 1 2))))

(defun cons-if (x xs)
  (if x
      (cons x xs)
      xs))
