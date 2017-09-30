(in-package :serapeum.tests)

(def-suite definitions :in serapeum)
(in-suite definitions)

;;; Note that the defalias forms come *after* the tests. The real test
;;; here is not the tests themselves, but that we don't get warnings
;;; about x1, x2, &c. being undefined.

(test defalias
  (is (fboundp 'x1))
  (is (eql 'x (x1))))

(defalias x1 (constantly 'x))

(test let-over-defalias
  (is (fboundp 'x2))
  (is (eql 'x (x2))))

(let ((x 'x))
  (defalias x2 (lambda () x)))

(test flet-over-defalias
  (is (fboundp 'x3))
  (is (eql 'x (x3))))

(flet ((x () 'x))
  (defalias x3 #'x))

(test labels-over-defalias
  (is (fboundp 'x4))
  (is (eql 'x (x4))))

(labels ((x () 'x))
  (defalias x4 #'x))

(test defstruct-read-only
  (let ((opts
          '((:copier nil) (:include serapeum::%read-only-struct))))
    (is (equal `(defstruct (foo ,@opts)
                  (bar (required-argument 'bar) :read-only t))
               (macroexpand-1
                '(defstruct-read-only foo
                  bar))))

    (is (equal `(defstruct (foo ,@opts)
                  "A struct."
                  (bar (required-argument 'bar) :read-only t))
               (macroexpand-1
                '(defstruct-read-only foo
                  "A struct."
                  bar))))

    (is (equal `(defstruct (foo ,@opts)
                  (bar nil :read-only t))
               (handler-bind ((warning #'muffle-warning))
                 (macroexpand-1
                  '(defstruct-read-only foo
                    (bar nil :read-only nil))))))

    (is (equal `(defstruct (foo ,@opts)
                  "A struct."
                  (bar nil :read-only t))
               (handler-bind ((warning #'muffle-warning))
                 (macroexpand-1
                  '(defstruct-read-only foo
                    "A struct."
                    (bar nil :read-only nil)))))))

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
           ((person name) name))))))
