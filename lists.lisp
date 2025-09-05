(in-package :serapeum)

(-> firstn ((integer 0 *) list) list)
(-> powerset (list) list)
(-> efface (t list) list)
(-> mapcar-into ((or function symbol) list) list)
(-> nthrest ((integer 0 *) list) list)

(defun filter-map (fn list &rest lists)
  "Map FN over (LIST . LISTS) like `mapcar', but omit empty results.

     (filter-map fn ...)
     ≅ (remove nil (mapcar fn ...))"
  (let ((fn (ensure-function fn)))
    (if lists
        (apply #'mapcan
               (lambda (&rest args)
                 (unsplice (apply fn args)))
               list lists)
        (loop for each in list
              for x = (funcall fn each)
              if x collect x))))

(define-compiler-macro filter-map (fn list &rest lists)
  (let* ((lists (cons list lists))
         (gs (make-gensym-list (length lists))))
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

(defsubst append1 (list item)
  "Append an atom to a list.

    (append1 list item)
    ≡ (append list (list item))"
  (append list (list item)))

(defsubst nconc1 (list item)
  "Like `append1', but destructive."
  (nconc list (list item)))

(defsubst prepend (&rest lists)
  "Construct and return a list by concatenating LISTS in reverse order.

    (prepend list-1 list-2)
    ≡ (append list-2 list-1)"
  (apply #'append (reverse lists)))

(define-modify-macro prependf (&rest lists) prepend
  "Modify-macro for prepend. Prepends LISTS to the PLACE designated by the first argument.")

(defmacro push-end (item place &environment env)
  "Destructively push ITEM to the end of PLACE.
Like `push', but affects the last item rather than the first.

You may want to use `enq' on a `queue' instead.

From LispWorks."
  (multiple-value-bind (dummies vals temps setter getter)
      (get-setf-expansion place env)
    (when (rest temps) (error "Invalid place for push-end"))
    (let ((temp (car temps)))
      `(let* (,@(mapcar #'list dummies vals)
              (,temp ,getter))
         (setf ,temp (nconc1 ,temp ,item))
         ,setter))))

(defmacro push-end-new (item place
                        &rest kwargs
                        &key key test test-not
                        &environment env)
  "Pushes ITEM to the end of place (like `push-end') but only if it not already a member of PLACE (like `pushnew').

For the use of KEY, TEST, and TEST-NOT, see `pushnew'."
  (declare (ignore key test test-not))
  (once-only (item)
    (multiple-value-bind (dummies vals temps setter getter)
        (get-setf-expansion place env)
      (when (rest temps) (error "Invalid place for push-end"))
      (let ((temp (car temps)))
        `(let* (,@(mapcar #'list dummies vals)
                (,temp ,getter))
           (unless (member ,item ,temp ,@kwargs)
             (setf ,temp (nconc1 ,temp ,item)))
           ,setter)))))

(defun in (x &rest items)
  "Is X equal to any of ITEMS?

`(in x xs...)` is always equivalent to `(and (member x xs :test equal) t)`,
but `in` can sometimes compile to more efficient code when the
candidate matches are constant.

From Arc."
  (declare (optimize (speed 3) (safety 1))
           (dynamic-extent items))
  (loop for item in items
        when (equal x item)
          return t))

(define-compiler-macro in (x &rest items)
  (once-only (x)
    (cond ((every (disjoin #'keywordp #'numberp #'characterp)
                  items)
           `(case ,x ((,@items) t)))
          ((every #'stringp items)
           `(and (stringp ,x)
                 (string-case ,x
                   ((,@items) t))))
          (t `(or ,@(loop for item in items
                          collect `(equal ,x ,item)))))))

(-> memq (t list) list)
(declaim-maybe-inline memq)
(defun memq (item list)
  "Like (member ITEM LIST :test #'eq).
Should only be used for symbols."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (list list))
  ;; Cf. Richard Fateman, "Code ‘Bumming’ for testing membership.".
  (tagbody loop
     (when list
       (unless (eq item (first list))
         (setf list (rest list))
         (go loop)))
     (return-from memq list)))

(define-compiler-macro memq (&whole call
                                    item list
                                    &environment env)
  (multiple-value-bind (list constant?)
      (eval-if-constant list env)
    (if (not constant?) call
        (if (not (every #'symbolp list)) call
            `(case ,item
               ,@(loop for tail on list
                       ;; NB. The symbol might be nil.
                       collect `((,(car tail))
                                 ',tail)))))))

(-> delq (t list) list)
(defun delq (item list)
  "Like (delete ... :test #'eq), but only for lists.

Almost always used as (delq nil ...)."
  (declare (list list) (optimize speed))
  #+(or) (cond ((endp list) nil)
               ((eq (car list) item) (cdr list))
               (t (rplacd list (delq item (cdr list)))))
  (let ((splice '()))
    (loop for l = list then (cdr l) do
      (cond ((endp l) (return list))
            ((eq (car l) item)
             (if (null splice)
                 (setf list (cdr l))
                 (setf (cdr splice) (cdr l))))
            (t (setf splice l))))))

(defun mapply (fn list &rest lists)
  "`mapply' is a cousin of `mapcar'.

If you think of `mapcar' as using `funcall':

    (mapcar #'- '(1 2 3))
    ≅ (loop for item in '(1 2 3)
            collect (funcall #'- item))

Then `mapply' does the same thing, but with `apply' instead.

    (loop for item in '((1 2 3) (4 5 6))
            collect (apply #'+ item))
    => (6 15)

    (mapply #'+ '((1 2 3) (4 5 6)))
    => (6 15)

In variadic use, `mapply' acts as if `append' had first been used:

    (mapply #'+ xs ys)
    ≡ (mapply #'+ (mapcar #'append xs ys))

But the actual implementation is more efficient.

`mapply' can convert a list of two-element lists into an alist:

    (mapply #'cons '((x 1) (y 2))
    => '((x . 1) (y . 2))"
  (let ((fn (ensure-function fn)))
    (if lists
        (apply #'mapcar
               (lambda (&rest args)
                 (apply fn (apply #'append args)))
               list lists)
        (mapcar (lambda (args)
                  (apply fn args))
                list))))

(define-compiler-macro mapply (fn list &rest lists)
  (let* ((lists (cons list lists))
         (vars (loop for nil in lists collect (string-gensym 'arg))))
    (with-gensyms (gfn)
      `(let ((,gfn (ensure-function ,fn)))
         (mapcar
          (lambda ,vars
            ,(if (null (cdr vars))
                 `(apply ,gfn ,@vars)
                 ;; Use multiple-value-call to avoid consing.
                 `(multiple-value-call ,gfn
                    ,@(loop for var in vars
                            collect `(values-list ,var)))))
          ,@lists)))))

(defsubst assocdr (item alist &rest args &key &allow-other-keys)
  "Like (cdr (assoc ...))"
  (let ((found (apply #'assoc item alist args)))
    (values (cdr found) found)))

(defsubst assocar (item alist &rest args &key &allow-other-keys)
  "Like (car (assoc ...))"
  (let ((found (apply #'assoc item alist args)))
    (values (car found) found)))

(defsubst assocadr (item alist &rest args &key &allow-other-keys)
  "Like `assocdr' for alists of proper lists.

     (assocdr 'x '((x 1))) => '(1)
     (assocadr 'x '((x 1))) => 1"
  (let ((found (apply #'assoc item alist args)))
    (values (cadr found) found)))

(defsubst rassocar (item alist &rest args &key &allow-other-keys)
  "Like (car (rassoc ...))"
  (let ((found (apply #'rassoc item alist args)))
    (values (car found) found)))

(defsubst rassocdr (item alist &rest args &key &allow-other-keys)
  "Like (cdr (rassoc ...))"
  (let ((found (apply #'rassoc item alist args)))
    (values (cdr found) found)))

(defsubst firstn (n list)
  "The first N elements of LIST, as a fresh list:

    (firstn 4 (iota 10))
    => (0 1 2 4)

\(I do not know why this extremely useful function did not make it
into Common Lisp, unless it was deliberately left out as an exercise
for Maclisp users.)"
  ;; NB This is faster than the DO version in the Pitmanual.
  (loop repeat n for x in list collect x))

(defun powerset (set)
  "Return the powerset of SET.
Uses a non-recursive algorithm."
  (unless (setp set)
    (error "Not a set: ~a" set))
  (loop for i below (expt 2 (length set))
        collect (loop for j from 0
                      for x in set
                      when (logbitp j i)
                        collect x)))

(defun efface (item list)
  "Destructively remove only the first occurence of ITEM in LIST.

From Lisp 1.5."
  ;; Cf. `delq'.
  ;; (cond ((null list) nil)
  ;;       ((eql (car x) item) (cdr list))
  ;;       (t (rplacd list (efface item (cdr list)))))
  (let ((splice '()))
    (loop for l = list then (cdr l) do
      (cond ((endp l) (return list))
            ((eql (car l) item)
             (if (null splice)
                 (return (cdr l))
                 (progn
                   (setf (cdr splice) (cdr l))
                   (return list))))
            (t (setf splice l))))))

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

(defun plist-keys (plist)
  "Return the keys of a plist."
  (collecting*
    (doplist (k v plist)
      (collect k))))

(defun plist-values (plist)
  "Return the values of a plist."
  (collecting*
    (doplist (k v plist)
      (collect v))))

(defun list+length (list start end)
  (when (and start end)
    (assert (<= start end)))
  (let* ((list
           (if start
               (nthcdr start list)
               list))
         (length
           (if end
               (- (length list) end)
               (length list))))
    (values list length)))

(defun list-map-from-end/bordeaux (fun list &key start end)
  "Traverse LIST from the end, calling FUN on each item.

This uses the technique described by Durand and Strandh in their paper
presented at ECLS 2015, “Processing List Elements in Reverse Order.”"
  (declare (optimize (speed 3)
                     (safety 1)
                     (compilation-speed 0)))
  #.+merge-tail-calls+
  (symbol-macrolet ((small 10000)
                    (big 100000000))
    (labels ((aux1 (fun list length)
               (declare (fixnum length) (function fun))
               (unless (zerop length)
                 (aux1 fun (cdr list) (1- length))
                 (funcall fun (car list))))
             (aux2 (fun list length)
               (declare (fixnum length))
               (if (<= length small)
                   (aux1 fun list length)
                   (progn
                     (aux2 fun (nthcdr small list) (- length small))
                     (aux1 fun list small))))
             (aux3 (fun list length)
               (declare (fixnum length))
               (if (< length big)
                   (aux2 fun list length)
                   (let* ((n (ash length -1))
                          (middle (nthcdr n list)))
                     (progn
                       (aux3 fun middle (- length n))
                       (aux2 fun list n))))))
      (multiple-value-bind (list length)
          (list+length list start end)
        (declare (fixnum length) (list list))
        (aux3 fun list length)))))

(defun intersectionp (list1 list2 &key key test test-not)
  "Return T if LIST1 and LIST2 intersect.
Equivalent to `(and (intersection list1 list2) t)`, without
intermediate consing.

Two empty lists are not considered to intersect."

  (declare (list list1 list2))
  (when (and list1 list2)
    (with-member-test (mem :key key :test test :test-not test-not)
      (dolist (elt list1)
        (when (mem (key elt) list2)
          (return t))))))

(defun stable-set-difference (list1 list2 &key key test test-not)
  "Like `set-difference', but preserve the order of LIST1's elements."
  (declare (list list1 list2))
  (with-member-test (mem :key key :test test :test-not test-not)
    (if list2
        (collecting
          (dolist (elt list1)
            (unless (mem (key elt) list2)
              (collect elt))))
        list1)))

(defun append-longest (&rest lists)
  "Like `append', but without guarantees as to order.
The longest list in LISTS is put last, to maximize structure sharing.

This also ignores nil, so `(append-longest list nil)' will return its
first argument unchanged."
  (let ((lists (remove nil lists)))
    (cond ((no lists) nil)
          ((single lists) (car lists))
          (t (let ((longest (longest lists)))
               (multiple-value-call #'append
                 (values-list (remove longest lists))
                 longest))))))

(defun mappend-longest (fn &rest lists)
  "Like `mappend', but using `append-longest'."
  (apply #'append-longest (mapcar fn lists)))
