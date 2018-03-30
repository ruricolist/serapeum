(in-package :serapeum.tests)

(def-suite defining-types :in serapeum)
(in-suite defining-types)

(test defstruct-read-only
  (let ((opts
          '((:copier nil)
            #+(or sbcl cmucl) (:pure t)
            (:include serapeum::%read-only-struct))))
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
    (macroexpand-1 '(defstruct-read-only (foo (:include bar)))))

  (signals error
    (macroexpand-1 '(defstruct-read-only (foo (:copier copy-foo))))))

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

(defunit zero)

(test defunit
  (is (eq zero zero))
  (is (equal "#.ZERO"
             (with-standard-io-syntax
               (let ((*package* (find-package :serapeum.tests))
                     (*print-readably* t))
                 (write-to-string zero)))))
  (is (subtypep 'zero 'serapeum::unit-object)))

(defunion tree
  leaf
  (node (value integer)
        (left tree)
        (right tree)))

(defun count-nodes (tree)
  (match-of tree tree
    (leaf 0)
    ((node _ left right)
     (+ 1
        (count-nodes left)
        (count-nodes right)))))

(test union/tree
  (is (= 4 (count-nodes
            (node 5
                  (node 1 leaf leaf)
                  (node 3 leaf
                        (node 4 leaf leaf)))))))

(defunion maybe
  (just (value t))
  nothing)

(test union/maybe
  (is (= 5 (just-value (just 5))))
  (is (eq nothing nothing)))

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
