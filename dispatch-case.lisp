(defpackage :serapeum/dispatch-case
  (:use :cl :alexandria :serapeum)
  (:import-from :trivia :match :ematch)
  #+sb-package-locks (:implement :serapeum :serapeum/dispatch-case))

(in-package :serapeum/dispatch-case)

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
         (ordering (toposort constraints)))
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
  `(etypecase-of ,type ,expr
     ,@(remove-shadowed-clauses clauses env)))

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

(defun hoist-clause-bodies (clauses env)
  "Hoist the bodies of the clauses into separate functions.
This is needed because the same clause may be spliced into the
dispatch-case form in several places (in order to implement
fallthrough).

Returns two values: a list of function definitions (suitable as the
first argument to `flet') and a list of clauses."
  ;; TODO Should this be a tagbody instead?
  (with-collectors (fns-out clauses-out)
    (dolist (clause clauses)
      (destructuring-bind (types . body) clause
        (if (constantp `(progn ,@body) env)
            ;; The body is constant, no need to hoist.
            (clauses-out clause)
            (let ((sym (string-gensym 'clause-body)))
              (fns-out `(,sym () ,@body))
              (clauses-out `(,types (,sym)))))))))

(defmacro dispatch-case ((&rest exprs-and-types) &body clauses)
  "Dispatch on the types of multiple expressions, exhaustively.

Say you are working on a project where you need to handle timestamps
represented both as universal times, and as instances of
`local-time:timestamp'. You start by defining the appropriate types:

    (defpackage :dispatch-case-example
      (:use :cl :alexandria :serapeum :local-time)
      (:shadow :time))
    (in-package :dispatch-case-example)

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
the serious disadvantage of being hard to read.

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

This is easy to read, but it has three disadvantages. (1) There is no
exhaustiveness checking. If, at some point in the future, you want to
add another representation of time to your project, the compiler will
not object if you forget to update `time='. (2) You cannot use the
`universal-time' type you just defined; it is a type, not a class, so
you cannot specialize methods on it. (3) You are paying a run-time
price for extensibility -- the inherent overhead of a generic function
-- when extensibility is not what you want.

Using `dispatch-case' instead gives you the readability of
`defgeneric' with the efficiency and safety of `etypecase-of'.

    (defun time= (t1 t2)
      (dispatch-case ((time t1)
                      (time t2))
        ((universal-time universal-time)
         (= t1 t2))
        ((timestamp timestamp)
         (timestamp= t1 t2))
        ((universal-time timestamp)
         (= t1 (timestamp-to-universal t2)))
        ((timestamp universal-time)
         (time= t2 t1))))

Note that -- unlike `etypecase', but like `defgeneric' -- the order in
which the clauses are defined does not matter."
  `(dispatch-case-let
       ,(loop for (expr type) in exprs-and-types
              for var = (string-gensym 'temp)
              collect `((,var ,type) ,expr))
     ,@clauses))

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

It may be helpful to think of this as analogous to both
`defmethod' (where the `(variable type)' notation is used in the
lambda list) and `let' (which has an obvious macro-expansion in terms
of `lambda')."
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
    (multiple-value-bind (fns clauses)
        (hoist-clause-bodies clauses env)
      (let ((function-names (mapcar #'first fns)))
        `(let ,(mapcar #'list vars exprs)
           (flet ,fns
             (declare
              (dynamic-extent
               ,@(mapcar (op `#',_) function-names)))
             (dispatch-case/nobindings ,(mapcar #'list vars types)
               ,@clauses)))))))

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
    (loop for (types . body) in clauses
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
