(defpackage :serapeum/dispatch-case
  (:use :cl :alexandria :serapeum)
  (:import-from :trivia :match :ematch)
  #+sb-package-locks
  (:implement :serapeum :serapeum/dispatch-case))

(in-package :serapeum/dispatch-case)

(define-condition dispatch-case-error (type-error)
  ((matched-types-slot :initarg :matched-types
                       :reader dispatch-case-error-matched-types))
  (:default-initargs :matched-types nil)
  (:report (lambda (c s)
             (with-slots ((matched-types matched-types-slot)) c
               (format s "Dispatch case failed after ~a step~:p~@[ (~{~a~^, ~})~]:~%"
                       (length matched-types)
                       matched-types)
               (write (make-condition 'type-error
                                      :datum (type-error-datum c)
                                      :expected-type (type-error-expected-type c))
                      :stream s)))))

(define-symbol-macro matched-types ())

(defmacro with-matched-type (type &body body &environment env)
  (let ((%matched-types (macroexpand-1 'matched-types env)))
    `(symbol-macrolet ((matched-types ,(cons type %matched-types)))
       ,@body)))

(defmacro dispatch-case-error (&key type datum &environment env)
  (let ((%matched-types (macroexpand-1 'matched-types env)))
    `(error 'dispatch-case-error
            :expected-type ,type
            :datum ,datum
            :matched-types ',(butlast %matched-types))))

(defun clause-leading-type (clause)
  (caar clause))

(defun branch-type (branch)
  (clause-leading-type (first branch)))

(defun ensure-var (var)
  (check-type var symbol)
  ;; Is this really a good idea?
  (assure symbol
    (if (string= var '_) (gensym) var)))

(defun sort-clauses (clauses &optional env)
  "Given a list of typecase clauses, order them so no preceding clause
shadows later ones, if possible."
  ;; NB Is using a toposort really necessary?
  (let* ((types (mapcar #'clause-leading-type clauses))
         (constraints
           (loop for type1 in types
                 append (loop for type2 in types
                              when (proper-subtype-p type1 type2 env)
                                collect (list type1 type2))))
         (constraints (nub constraints))
         (ordering (toposort constraints :test #'equal)))
    (stable-sort (copy-list clauses)
                 ordering
                 :key #'clause-leading-type)))

(defun remove-shadowed-clauses (clauses &optional env)
  "Given a list of typecase clauses, remove any clauses that are
shadowed by previous clauses."
  (nlet rec ((clauses clauses)
             (types-seen '())
             (acc '()))
    (match clauses
      ((list) (nreverse acc))
      ((list*
        (and clause (list* clause-type _))
        clauses)
       (if (subtypep clause-type `(or ,@types-seen) env)
           ;; This clause is shadowed.
           (rec clauses types-seen acc)
           (rec clauses
                (cons clause-type types-seen)
                (cons clause acc)))))))

(defmacro etypecase-of/no-shadows (type expr &body clauses
                                   &environment env)
  "Like `etypecase-of', but filter shadowed clauses."
  (once-only (expr)
    `(with-matched-type ,type
       (typecase-of ,type ,expr
         ,@(remove-shadowed-clauses clauses env)
         (otherwise
          (dispatch-case-error :type ',type :datum ,expr))))))

(defun collect-fallthrough-clauses (branch more-branches &optional env)
  "Collect fallthrough clauses from MORE-BRANCHES into BRANCH."
  (let ((branch-type (branch-type branch)))
    (append branch
            (nreverse
             (reduce (lambda (more-clauses branch)
                       (if (subtypep branch-type
                                     (branch-type branch)
                                     env)
                           (revappend branch more-clauses)
                           more-clauses))
                     more-branches
                     :initial-value nil)))))

(defun hoist-clause-bodies (block clauses env)
  "Hoist the bodies of the clauses into tags.
This is needed because the same clause may be spliced into the
dispatch-case form in several places (in order to implement
fallthrough).

Returns two values: a list of alternating tags and forms, and a list
of clauses rewritten to jump to the tags."
  (with-collectors (tags-out clauses-out)
    (dolist (clause clauses)
      (destructuring-bind (types . body) clause
        (if (constantp `(progn ,@body) env)
            ;; The body is constant, no need to hoist.
            (clauses-out
             `(,types
               (return-from ,block
                 (progn ,@body))))
            (let ((tag (string-gensym 'tag)))
              (tags-out tag `(return-from ,block (progn ,@body)))
              (clauses-out `(,types (go ,tag)))))))))

(defmacro dispatch-case ((&rest exprs-and-types) &body clauses)
  "Dispatch on the types of multiple expressions, exhaustively.

Say you are working on a project where you need to handle timestamps
represented both as universal times, and as instances of
`local-time:timestamp'. You start by defining the appropriate types:

    (defpackage :dispatch-case-example
      (:use :cl :alexandria :serapeum :local-time)
      (:shadow :time))
    \(in-package :dispatch-case-example)

    (deftype universal-time ()
      '(integer 0 *))

    (deftype time ()
      '(or universal-time timestamp))

Now you want to write a `time=' function that works on universal
times, timestamps, and any combination thereof.

You can do this using `etypecase-of':

    (defun time= (t1 t2)
      (etypecase-of time t1
        (universal-time
         (etypecase-of time t2
           (universal-time
            (= t1 t2))
           (timestamp
            (= t1 (timestamp-to-universal t2)))))
        (timestamp
         (etypecase-of time t2
           (universal-time
            (time= t2 t1))
           (timestamp
            (timestamp= t1 t2))))))

This has the advantage of efficiency and exhaustiveness checking, but
the serious disadvantage of being hard to read: to understand what
each branch matches, you have to backtrack to the enclosing branch.
This is bad enough when the nesting is only two layers deep.

Alternately, you could do it with `defgeneric':

    (defgeneric time= (t1 t2)
      (:method ((t1 integer) (t2 integer))
        (= t1 t2))
      (:method ((t1 timestamp) (t2 timestamp))
        (timestamp= t1 t2))
      (:method ((t1 integer) (t2 timestamp))
        (= t1 (timestamp-to-universal t2)))
      (:method ((t1 timestamp) (t2 integer))
        (time= t2 t1)))

This is easy to read, but it has three potential disadvantages. \(1)
There is no exhaustiveness checking. If, at some point in the future,
you want to add another representation of time to your project, the
compiler will not warn you if you forget to update `time='. \(This is
bad enough with only two objects to dispatch on, but with three or
more it gets rapidly easier to miss a case.) \(2) You cannot use the
`universal-time' type you just defined; it is a type, not a class, so
you cannot specialize methods on it. \(3) You are paying a run-time
price for extensibility -- the inherent overhead of a generic function
-- when extensibility is not what you want.

Using `dispatch-case' instead gives you the readability of
`defgeneric' with the efficiency and safety of `etypecase-of'.

    (defun time= (t1 t2)
      (dispatch-case ((t1 time)
                      (t2 time))
        ((universal-time universal-time)
         (= t1 t2))
        ((timestamp timestamp)
         (timestamp= t1 t2))
        ((universal-time timestamp)
         (= t1 (timestamp-to-universal t2)))
        ((timestamp universal-time)
         (time= t2 t1))))

The syntax of `dispatch-case' is much closer to `defgeneric' than it
is to `etypecase'. The order in which clauses are defined does not
matter, and you can define fallthrough clauses in the same way you
would define fallthrough methods in `defgeneric'.

Suppose you wanted to write a `time=' function like the one above, but
always convert times to timestamps before comparing them. You could
write that using `dispatch-case' like so:

    (defun time= (x y)
      (dispatch-case ((x time)
                      (y time))
        ((* universal-time)
         (time= x (universal-to-timestamp y)))
        ((universal-time *)
         (time= (universal-to-timestamp x) y))
        ((timestamp timestamp)
         (timestamp= x y))))

\(In the list of types, you can use as asterisk as a shorthand for the
type of the corresponding argument to `dispatch-case'; in that above,
`time'.)

Note that this requires only three clauses, where writing it out using
nested `etypecase-of' forms would require four clauses. This is a
small gain; but with more subtypes to dispatch on, or more objects,
such fallthrough clauses become more useful."
  (let ((*gensym-counter* 0))
    `(dispatch-case-let
         ,(loop for (expr type) in exprs-and-types
                for i from 0
                for var = (string-gensym 'arg)
                collect `((,var ,type) ,expr))
       ,@clauses)))

(defun expand-dispatch-caseql-clauses (clauses)
  (expect-form-list
   (loop for (keys . body) in clauses
         collect (cons (loop for key in keys
                             collect `(eql ,key))
                       body))))

(defmacro dispatch-caseql ((&rest exprs-and-types) &body clauses)
  "Like `dispatch-case', but types in clauses are implicitly wrapped in `eql'.
The syntax of `dispatch-caseql' is tohus closer to `case' than to
`typecase'."
  (expect-form-list
   `(dispatch-case ,exprs-and-types
      ,@(expand-dispatch-caseql-clauses clauses))))

(defmacro dispatch-case-let ((&rest bindings) &body clauses &environment env)
  "Like `dispatch-case', but establish new bindings for each expression.

For example,

    (dispatch-case-let (((x string) (expr1))
                        ((y string) (expr2)))
      ...)

is equivalent to

    (let ((x (expr1))
          (y (expr2)))
      (dispatch-case ((x string)
                      (y string))
        ...))

It may be helpful to think of this as a cross between
`defmethod' (where the (variable type) notation is used in the lambda
list) and `let' (which has an obvious macro-expansion in terms of
`lambda')."
  (multiple-value-bind (vars types exprs)
      ;; Split the bindings and types.
      (with-collectors (vars-out types-out exprs-out)
        (flet ((vars-out (var) (vars-out (ensure-var var))))
          (dolist (binding bindings)
            (ematch binding
              ((list (and var (type symbol)) expr)
               (vars-out var)
               (types-out t)
               (exprs-out expr))
              ((list (list (and var (type symbol)) type) expr)
               (vars-out var)
               (types-out type)
               (exprs-out expr))))))
    (let ((normalized-clauses
            ;; Normalize asterisks into the types of the corresponding
            ;; variables.
            (loop for (clause-types . clause-body) in clauses
                  collect (cons
                           (loop for clause-type in clause-types
                                 for type in types
                                 if (eql clause-type '*)
                                   collect type
                                 else collect clause-type)
                           clause-body))))
      (with-unique-names (block)
        (multiple-value-bind (tags final-clauses)
            (hoist-clause-bodies block normalized-clauses env)
          `(let ,(mapcar #'list vars exprs)
             (with-read-only-vars ,vars
               (block ,block
                 (tagbody
                    (dispatch-case/nobindings ,(mapcar #'list vars types)
                      ,@final-clauses)
                    ,@tags)))))))))

(defmacro dispatch-caseql-let ((&rest bindings) &body clauses)
  "Like `dispatch-case-let', but using the clause syntax of `dispatch-caseql'."
  `(dispatch-case-let ,bindings
     ,@(expand-dispatch-caseql-clauses clauses)))

;;; TODO The problem with this expansion is that each branch only sees
;;; one value, which yields error messages that are hard to
;;; understand.

(defmacro dispatch-case/nobindings (vars-and-types &body clauses
                                    &environment env)
  (setf clauses (sort-clauses clauses env))
  (multiple-value-bind (vars types)
      ;; (values (mapcar #'first vars-and-types)
      ;;         (mapcar #'second vars-and-types))
      ;; Doing it this way checks that each form has the right shape.
      (loop for (var type) in vars-and-types
            collect var into vars
            collect type into types
            finally (return (values vars types)))
    (declare (ignore types))
    ;; Avoid multiple evaluation.
    (assert (every #'symbolp vars))
    ;; Check that the clauses are valid.
    (loop for (types . nil) in clauses
          do (assert (length= types vars)))
    (ematch vars-and-types
      ((list) nil)
      ((list (list var type))
       `(etypecase-of/no-shadows ,type ,var
          ,@(loop for (types . body) in clauses
                  collect `(,@types ,@body))))
      ((list* (list var type) vars-and-types)
       `(etypecase-of/no-shadows ,type ,var
          ,@(let* ((branches
                     (assort clauses
                             :key #'clause-leading-type
                             :test #'type=))
                   (branches
                     (maplist (lambda (branches)
                                (collect-fallthrough-clauses
                                 (first branches)
                                 (rest branches)
                                 env))
                              branches)))
              (loop for branch in branches
                    for type = (branch-type branch)
                    collect `(,type
                              (dispatch-case/nobindings ,vars-and-types
                                ,@(loop for ((nil . types) . body) in branch
                                        collect `(,types ,@body)))))))))))
