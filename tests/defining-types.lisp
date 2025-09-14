(in-package :serapeum.tests)

(def-suite defining-types :in serapeum)
(in-suite defining-types)

(test defstruct-read-only
  (let ((opts
          '((:copier nil)
            #+(or sbcl cmucl) (:pure t)
            (:include serapeum/defining-types::%read-only-struct))))
    (is (equal `(defstruct (foo ,@opts)
                  (bar (required-argument 'bar) :read-only t))
               (second
                (macroexpand-1
                 '(defstruct-read-only foo
                   bar)))))

    (is (equal `(defstruct (foo ,@opts)
                  "A struct."
                  (bar (required-argument 'bar) :read-only t))
               (second
                (macroexpand-1
                 '(defstruct-read-only foo
                   "A struct."
                   bar)))))

    (is (equal `(defstruct (foo ,@opts)
                  (bar nil :read-only t))
               (handler-bind ((warning #'muffle-warning))
                 (second
                  (macroexpand-1
                   '(defstruct-read-only foo
                     (bar nil :read-only nil)))))))

    (is (equal `(defstruct (foo ,@opts)
                  "A struct."
                  (bar nil :read-only t))
               (second
                (handler-bind ((warning #'muffle-warning))
                  (macroexpand-1
                   '(defstruct-read-only foo
                     "A struct."
                     (bar nil :read-only nil))))))))

  (signals error
    (macroexpand-1 '(defstruct-read-only (foo (:copier copy-foo))))))

(defstruct-read-only ro-struct-1)
(defstruct-read-only (ro-struct-2 (:include ro-struct-1)))

(defstruct rw-struct-1)

(test read-only-struct-inheritance
  (finishes (make-ro-struct-2))
  (signals error
    (eval `(defstruct-read-only (rw-struct-2 (:include rw-struct-1))))))

(defconstructor person
  (name string)
  (age (integer 0 1000)))

(test defconstructor
  (let ((person (person #1="Common Lisp" #2=33)))
    (is (= 34
           (person-age
            (copy-person person
                         :age (1+ (person-age person))))))
    (is (equal '(person #1# #2#)
               (make-load-form person)))
    (is (equal
         '(#1# #2#)
         (trivia:match (person #1# #2#)
           ((person name age) (list name age)))))
    (is (equal
         #1#
         (trivia:match (person #1# #2#)
           ((person name) name))))

    (is (equal
         "(PERSON \"Common Lisp\" 33)"
         (princ-to-string person)))))

(test defunit
  (is (eq* zero zero (eval 'zero)))
  (is (eq* leaf leaf (eval 'leaf)))
  (is (eq zero (get-zero)))
  (is (eq leaf (get-leaf)))
  (is (equal "#.ZERO"
             (with-standard-io-syntax
               (let ((*package* (find-package :serapeum.tests))
                     (*print-readably* t))
                 (write-to-string zero)))))
  (is (equal "#.LEAF"
             (with-standard-io-syntax
               (let ((*package* (find-package :serapeum.tests))
                     (*print-readably* t))
                 (write-to-string leaf))))))

(defun count-nodes (tree)
  (match-of tree tree
    (leaf 0)
    ((node _ left right)
     (+ 1
        (count-nodes left)
        (count-nodes right)))))

(defgeneric count-nodes-generic (tree)
  (:method ((tree leaf)) 0)
  (:method ((node node))
    (let ((left (node-left node))
          (right (node-right node)))
      (+ 1
         (count-nodes left)
         (count-nodes right)))))

(defgeneric tree? (x)
  (:method ((tree <tree>)) t)
  (:method ((x t)) nil))

(test union/tree
  (let ((tree
          (node 5
                (node 1 leaf leaf)
                (node 3 leaf
                      (node 4 leaf leaf)))))
    (is-true (tree? tree))
    (is (= 4 (count-nodes tree)))
    (is (= 4 (count-nodes-generic tree)))))

(defunion maybe
  (just (value t))
  nothing)

(test union/maybe
  (is (subtypep (find-class 'just) (find-class '<maybe>)))
  (is (subtypep (find-class 'nothing) (find-class '<maybe>)))
  (is (subtypep 'maybe '<maybe>))
  (is (subtypep '(or just nothing) 'maybe))
  (is (= 5 (just-value (just 5))))
  (is (eq nothing nothing))
  (is (eq nothing (eval 'nothing))))

(defunion liszt
  (kons (kar t) (kdr liszt))
  knil)

(defun kar (l)
  (match-of liszt l
    ((kons a _) a)
    (knil knil)))

(defun kdr (l)
  (match-of liszt l
    ((kons _ b) b)
    (knil knil)))

(test union/liszt
  (let ((liszt (kons 1 (kons 2 knil))))
    (is (eql (kar liszt) 1))
    (is (eql (kar (kdr liszt)) 2))
    (is (eql (kdr (kdr liszt)) knil))))

(defunion point
  (rectangular (x float) (y float))
  (polar (x float) (y float)))

(test union/point
  (is (= 3.0
         (match-of point (rectangular 1.0 2.0)
           ((rectangular x y) (+ x y))
           ((polar _ _) nil))))

  (is (= 3.0
         (match-of point (rectangular 1.0 2.0)
           ((rectangular x y) (+ x y))
           (_ nil)))))

(deftype unions ()
  '(or tree liszt maybe))

(test union/exhaustiveness?
  (finishes
    (compile nil '(lambda (e)
                   (etypecase-of unions e
                     (liszt) (tree) (just) (nothing)))))
  (signals warning
    (compile nil '(lambda (e)
                   (etypecase-of unions e
                     ((or liszt tree)) (just)))))
  (finishes
    (compile nil '(lambda (e)
                   (etypecase-of (or unions list) e
                     (liszt) (tree) ((or maybe cons)) (null)))))
  (finishes
    (compile nil '(lambda (e)
                   (etypecase-of (or maybe kons knil) e
                     (just) (nothing) (liszt)))))
  (finishes
    (compile nil '(lambda (e)
                   (match-of (or maybe (member 42)) e
                     ((nothing) nil) ((just v) v) (42 'life)))))
  (finishes
    (compile nil '(lambda (e)
                   (match-of (or maybe (member :x)) e
                     ((nothing) nil) ((just v) v) (:x 'life)))))
  (finishes
    (compile nil '(lambda (e)
                   (match-of (or maybe (eql x)) e
                     ((nothing) nil) ((just v) v) ('x 'life)))))
  (signals warning
    (compile nil '(lambda (e)
                   (match-of (or point liszt) e
                     ((or (polar) (rectangular))) ((kons)))))))

(defconstructor dummy-constructor
  (symbol symbol)
  (list list))

(test reread-constructors
  (let ((value
          (read-from-string
           (write-to-string
            (dummy-constructor 'sym (list 1 2 3))
            :readably t))))
    (is (eql 'sym (dummy-constructor-symbol value)))
    (is (equal '(1 2 3) (dummy-constructor-list value)))))

(test constructor-load-forms
  (let ((obj (dummy-constructor 'sym nil)))
    (is (equal '(dummy-constructor 'sym nil)
               (make-load-form obj)))))
