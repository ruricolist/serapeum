(in-package :serapeum.tests)

(def-suite lists :in serapeum)
(in-suite lists)

(test memq
  (with-notinline (memq)
    (let ((list1 (list 'x 'y)))
      (is (eq (memq 'x list1) list1))
      (is (equal (memq 'x '(x y)) '(x y)))
      (is (equal (memq 'y '(x y)) '(y)))
      (is (equal (memq 'z '(x y)) nil))
      (is (equal (memq 'a '(1 a 2)) '(a 2)))
      (let* ((list1 (list 'x 'y nil 'z))
             (list2 (delq nil list1)))
        (is (not (memq nil list2)))
        (is (eq list1 list2)))
      (let* ((list1 (list 'x 'y nil 'z))
             (list2 (delq 'x list1)))
        (is (not (memq 'x list2)))
        (is (eql (cdr list1) list2))))))

(test mapply
  (with-notinline (mapply)
    (is (equal
         (locally (declare (optimize (speed 0) (safety 3) (debug 3)))
           (mapply #'cons '((x 1) (y 2))))
         '((x . 1) (y . 2))))
    (is (equal (mapply #'list '((a 1) (b 2)) '((c 3) (d 4)))
               '((a 1 c 3) (b 2 d 4))))))

(test filter-map
  (with-notinline (filter-map)
    (is (equal '(a b c) (filter-map #'identity '(a nil b nil nil c))))
    (is (equal '(1 2 3) (filter-map (lambda (s) (find-if #'numberp s))
                                    '((a 1 2) (b) (c d 2 e) (3) (h g)))))
    (is (equal '((a b) (c) (d e)) (filter-map #'append
                                              '(() (a) () () (d e) ())
                                              '(() (b) (c) () () ()))))))

(test car-safe
  (is (eql (car-safe '(a)) 'a))
  (is (eql (car-safe 'a) nil)))

(test cdr-safe
  (is (eql (cdr-safe '(a . b)) 'b))
  (is (eql (cdr-safe 'a) nil)))

(test append1
  (with-notinline (append1)
    (is (equal (append1 nil 'a) '(a)))
    (is (equal (append1 nil nil) '(nil)))
    (is (equal (append1 '(a b c) '(d e)) '(a b c (d e))))))

(test in
  (with-notinline (in)
    (is (eql (in 'a 'b 'c) nil))
    (is (eql (in 'a 'a) t))
    (is (eql (in nil nil) t))
    (is (eql (in '(a) '(a)) t))
    (is (eql (in "a" "x" "y" "z") nil))
    (is (eql (in t) nil))
    (is (eql (in "y" "x" "y" "z") t))))

(test assocdr
  (with-notinline (assocdr)
    (is (equal (multiple-value-list (assocdr 'a nil))
               '(nil nil)))
    (is (equal (multiple-value-list (assocdr 'a '((a . b))))
               '(b (a . b))))
    (is (equal (multiple-value-list (assocdr 'a '((b . a) (a . b) (c . d))))
               '(b (a . b))))))

(test assocdr
  (with-notinline (assocadr)
    (is (equal (multiple-value-list (assocadr 'a nil))
               '(nil nil)))
    (is (equal (multiple-value-list (assocadr 'a '((a b))))
               '(b (a b))))
    (is (equal (multiple-value-list (assocadr 'a '((b a) (a b) (c d))))
               '(b (a b))))))

(test rassocar
  (with-notinline (rassocar)
    (is (equal (multiple-value-list (rassocar 'a nil))
               '(nil nil)))
    (is (equal (multiple-value-list (rassocar 'a '((b . a))))
               '(b (b . a))))
    (is (equal (multiple-value-list (rassocar 'a '((c . b) (b . a) (d . a))))
               '(b (b . a))))))

(test firstn
  (with-notinline (firstn)
    (let ((list1 (copy-list '(a b c d e))))
      (is (equal (firstn 0 list1) nil))
      (is (equal (firstn 1 list1) '(a)))
      (is (equal (firstn 2 list1) '(a b)))
      (is (equal (firstn 5 list1) '(a b c d e)))
      (is (not (eq (firstn 5 list1) list1)))
      (is (equal (firstn 6 list1) '(a b c d e)))
      (is (not (eq (firstn 6 list1) list1))))))

(test powerset
  (signals error
    (powerset '(a a)))
  (is (equal (powerset '()) '(())))
  (is (equal (powerset '(a)) '(() (a))))
  (is (equal (powerset '(a b)) '(() (a) (b) (a b)))))

(test efface
  (let* ((list1 (list 'a 'b 'c 'a 'b 'd))
         (list2 (efface 'b list1)))
    (is (eql list1 list2))
    (is (equal list2 '(a c a b d))))
  (let* ((list1 (list 'a 'b 'a 'c 'a 'd))
         (list2 (efface 'a list1)))
    (is (eql list2 (cdr list1)))
    (is (equal list2 '(b a c a d))))
  (is (eql (efface 'a nil) nil)))

(test mapcar-into
  (with-notinline (mapcar-into)
    (is (eql (mapcar-into #'identity nil) nil))
    (let* ((list1 (list 1 4 10))
           (list2 (mapcar-into #'1+ list1)))
      (is (eql list1 list2))
      (is (equal list2 '(2 5 11))))))

(test nthrest
  (with-notinline (nthrest)
    (let ((list1 (list 1 2 3 4)))
      (is (eql (nthrest 0 list1) list1))
      (is (eql (nthrest 1 list1) (cdr list1)))
      (is (eql (nthrest 2 list1) (cddr list1)))
      (is (eql (nthrest 3 list1) (cdddr list1)))
      (is (eql (nthrest 4 list1) nil))
      (is (equal list1 '(1 2 3 4))))))

(test plist-keys
  (is (null (plist-keys nil)))
  (is (equal (plist-keys '(:a 1 :b 2)) '(:a :b))))

(test plist-values
  (is (null (plist-values nil)))
  (is (equal (plist-values '(:a 14 :b 5)) '(14 5))))

;; The :end tests fail here
#+nil
(test list-map-from-end/bordeaux
  (let (acc)
    (flet ((%a (x) (push x acc))
           (%f (&rest args)
             (apply #'serapeum::list-map-from-end/bordeaux args)))
      (%f #'%a '(a b c d e))
      (is (equal acc '(a b c d e)))
      (setf acc nil)
      (%f #'%a '(a b c d e) :start 0)
      (is (equal acc '(a b c d e)))
      (setf acc nil)
      (%f #'%a '(a b c d e) :start 1)
      (is (equal acc '(b c d e)))
      (setf acc nil)
      (%f #'%a '(a b c d e) :end 0)
      (is (equal acc nil))
      (setf acc nil)
      (%f #'%a '(a b c d e) :end 1)
      (is (equal acc '(a)))
      (setf acc nil)
      (%f #'%a '(a b c d e) :end 2)
      (is (equal acc '(a b)))
      (setf acc nil)
      (%f #'%a '(a b c d e) :start 1 :end 4)
      (is (equal acc '(b c d))))))
