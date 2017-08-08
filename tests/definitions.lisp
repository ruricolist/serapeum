(in-package :serapeum.tests)

(def-suite definitions :in serapeum)
(in-suite definitions)

(test defstruct-read-only
  (is (equal '(defstruct (foo (:copier nil))
               (bar (required-argument 'bar) :read-only t))
             (macroexpand-1
              '(defstruct-read-only foo
                bar))))

  (is (equal '(defstruct (foo (:copier nil))
               "A struct."
               (bar (required-argument 'bar) :read-only t))
             (macroexpand-1
              '(defstruct-read-only foo
                "A struct."
                bar))))

  (is (equal '(defstruct (foo (:copier nil))
               (bar nil :read-only t))
             (handler-bind ((warning #'muffle-warning))
               (macroexpand-1
                '(defstruct-read-only foo
                  (bar nil :read-only nil))))))

  (is (equal '(defstruct (foo (:copier nil))
               "A struct."
               (bar nil :read-only t))
             (handler-bind ((warning #'muffle-warning))
               (macroexpand-1
                '(defstruct-read-only foo
                  "A struct."
                  (bar nil :read-only nil))))))

  (signals error
    (macroexpand-1 '(defstruct-read-only (foo (:include bar)))))

  (signals error
    (macroexpand-1 '(defstruct-read-only (foo (:copier copy-foo))))))
