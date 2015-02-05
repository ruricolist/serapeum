(in-package :serapeum)

(export '(letrec letrec*
          mvlet mvlet*
          lret lret*
          and-let*))

;;;# `lret'

(defmacro lret ((&rest bindings) &body body)
  "Return the initial value of the last binding in BINDINGS. The idea
is to create something, initialize it, and then return it.

    (lret ((x 1)
           (y (make-array 1)))
      (setf (aref y 0) x))
    => #(1)

`lret' may seem trivial, but it fufills the highest purpose a macro
can: it eliminates a whole class of bugs (initializing an object, but
forgetting to return it).

Cf. `aprog1' in Anaphora."
  (multiple-value-bind (body decls)
      (parse-body body)
    `(let ,bindings
       ,@decls
       (prog1 ,(ensure-car (lastcar bindings))
         ,@body))))

(defmacro lret* ((&rest bindings) &body body)
  "Cf. `lret'."
  (multiple-value-bind (body decls)
      (parse-body body)
    `(let* ,bindings
       ,@decls
       (prog1 ,(ensure-car (lastcar bindings))
         ,@body))))

;;;# `letrec'

;;; Obviously `letrec' is less useful than in Scheme (where it is the
;;; way to construct recursive functions) but still sometimes useful;
;;; say, when initializing a timer whose function needs to refer to
;;; the timer itself.

(defmacro letrec ((&rest bindings) &body body)
  "Recursive LET.
The idea is that functions created in BINDINGS can close over one
another, and themselves.

Note that `letrec' only binds variables: it can define recursive
functions, but can't bind them as functions. (But see `fbindrec'.)"
  `(let (,@(mapcar #'car bindings))
     (psetq ,@(apply #'append bindings))
     (locally ,@body)))

(defmacro letrec* ((&rest bindings) &body body)
  "Like LETREC, but the bindings are evaluated in order.
See Waddell et al., *Fixing Letrec* for motivation.

Cf. `fbindrec*'."
  `(let (,@(mapcar #'car bindings))
     (setq ,@(apply #'append bindings))
     (locally ,@body)))

(defun simple-binding-p (binding)
  (= (length binding) 2))

;;;# `mvlet'

(defmacro mvlet* ((&rest bindings) &body body &environment env)
  "Expand a series of nested `multiple-value-bind' forms.

`mvlet*' is similar in intent to Scheme’s `let-values', but with a
different and less parenthesis-intensive syntax. Each binding is a
list of

    (var var*... expr)

A simple example should suffice to show both the implementation and
the motivation:

    (defun uptime (seconds)
      (mvlet* ((minutes seconds (truncate seconds 60))
               (hours minutes (truncate minutes 60))
               (days hours (truncate hours 24)))
        (declare ((integer 0 *) days hours minutes seconds))
        (fmt \"~d day~:p, ~d hour~:p, ~d minute~:p, ~d second~:p\"
             days hours minutes seconds)))

Note that declarations work just like `let*'."
  (cond ((null bindings)
         `(locally ,@body))
        ((every #'simple-binding-p bindings)
         `(let* ,bindings ,@body))
        (t (multiple-value-bind (body decls)
               (parse-body body)
             (let* ((mvbinds (member-if-not #'simple-binding-p bindings))
                    (simple-binds (ldiff bindings mvbinds)))
               (if simple-binds
                   (multiple-value-bind (local other)
                       (partition-declarations (mapcar #'car simple-binds) decls env)
                     `(let* ,simple-binds
                        ,@local
                        (mvlet* ,mvbinds
                          ,@other
                          ,@body)))
                   (let* ((vars (butlast (car bindings)))
                          (expr (lastcar (car bindings))))
                     (multiple-value-bind (local other)
                         (partition-declarations vars decls env)
                       `(multiple-value-bind ,vars
                            ,expr
                          ,@local
                          (mvlet* ,(cdr bindings)
                            ,@other
                            ,@body))))))))))

(defmacro firstn-values (n expr)
  (cond ((= n 0)
         `(values))
        ((= n 1)
         `(values ,expr))
        (t (let ((temps (loop for i below n collect (string-gensym 'temp))))
             `(multiple-value-bind ,temps
                  ,expr
                (values ,@temps))))))

(defmacro mvlet ((&rest bindings) &body body)
  "Parallel (`let'-like) version of `mvlet*'."
  (cond ((null bindings)
         `(locally ,@body))
        ((null (rest bindings))
         (let ((b (first bindings)))
           `(multiple-value-bind ,(butlast b) ,(lastcar b)
              ,@body)))
        ((every #'simple-binding-p bindings)
         `(let ,bindings
            ,@body))
        (t `(multiple-value-call
                (lambda ,(mappend #'butlast bindings)
                  ,@body)
              ,@(loop for binding in bindings
                      for n = (length (butlast binding))
                      for expr = (lastcar binding)
                      collect `(firstn-values ,n ,expr))))))

;;;# `and-let*'

(defmacro and-let* ((&rest clauses) &body body &environment env)
  "Scheme's guarded LET* (SRFI-2).

Each clause should have one of the following forms:

- `identifier', in which case IDENTIFIER's value is tested.

- `(expression)', in which case the value of EXPRESSION is tested.

- `(identifier expression)' in which case EXPRESSION is evaluated,
    and, if its value is not false, IDENTIFIER is bound to that value
    for the remainder of the clauses and the optional body.

Note that, of course, the semantics are slightly different in Common
Lisp than in Scheme, because our AND short-circuits on null, not
false."
  (multiple-value-bind (body decls)
      (parse-body body)
    (labels ((expand (clauses body)
               (unsplice
                (ematch clauses
                  (() (if body `(progn ,@body) nil))
                  ((list* (and var (type symbol)) clauses)
                   `(and ,var ,@(expand clauses body)))
                  ((list* (list var expr) clauses)
                   (multiple-value-bind (local other)
                       (partition-declarations (list var) decls env)
                     `(let ((,var ,expr))
                        ,@local
                        (and ,var ,@(expand clauses
                                            (append other body))))))
                  ((list* (list expr) clauses)
                   `(and ,expr ,@(expand clauses body)))))))
      (car (expand clauses body)))))

;;;# Etc

;;; These might be worth exporting if we can teach Slime to indent
;;; them.

(defmacro flet* (bindings &body body)
  (if (null bindings)
      `(locally ,@body)
      `(flet (,(car bindings))
         (flet* ,(cdr bindings)
           ,@body))))

(defmacro stack-flet (bindings &body body)
  `(flet ,bindings
     (declare (dynamic-extent ,@(mapcar (lambda (binding)
                                          `(function ,(car binding)))
                                        bindings)))
     ,@body))
