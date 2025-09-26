(defpackage :serapeum/binding
  (:documentation "Binding macros.")
  #+sb-package-locks (:lock t)
  (:use
   :cl
   :alexandria
   :serapeum/macro-tools
   :trivia)
  (:export
   #:and-let*
   #:if-not
   #:if-not-let
   #:letrec
   #:letrec*
   #:lret
   #:lret*
   #:mvlet
   #:mvlet*
   #:receive))
(in-package :serapeum/binding)

;;; Helpers.

(defun simple-binding-p (binding)
  (or (atom binding)
      (<= (length binding) 2)))

(defun canonicalize-bindings (bindings)
  (loop for binding in bindings
        if (atom binding)
          collect (list binding nil)
        else if (null (cdr binding))
               collect (list (car binding) nil)
        else collect binding))

;;; `let1'

(defmacro let1 (var expr &body body)
  "Bind VAR, immutably, to EXPR and evaluate BODY.

This may be pronounced with equal propriety as \"let-one\" or
\"let-once\"."
  `(let ((,var ,expr))
     (with-read-only-vars (,var)
       ,@body)))

(defpattern let1 (var expr &rest body)
  `(and (trivia:<> ,var ,expr) ,@body))

;;; `lret'

(defmacro lret-aux (let (&rest bindings) &body body)
  (if (null bindings)
      `(,let ()
         ,@body)
      (multiple-value-bind (body decls)
          (parse-body body)
        (let ((last-binding (ensure-car (lastcar bindings))))
          `(,let ,bindings
             ,@decls
             (with-read-only-vars (,last-binding)
               (prog1 ,last-binding
                 ,@body)))))))

(defmacro lret ((&rest bindings) &body body)
  "Return the initial value of the last binding in BINDINGS. The idea
is to create something, initialize it, and then return it.

    (lret ((x 1)
           (y (make-array 1)))
      (setf (aref y 0) x))
    => #(1)

Note that the value returned is the value initially bound. Subsequent
assignments are ignored.

    (lret ((x 1))
      (setf x 2))
    => 1

Furthermore, on Lisps that support it, the variable may be made
read-only, making assignment a compiler-time error.

`lret' may seem trivial, but it fufills the highest purpose a macro
can: it eliminates a whole class of bugs (initializing an object, but
forgetting to return it).

Cf. `aprog1' in Anaphora."
  `(lret-aux let ,bindings
     ,@body))

(defmacro lret* ((&rest bindings) &body body)
  "Cf. `lret'."
  `(lret-aux let* ,bindings
     ,@body))

;;; `letrec'

;; Obviously `letrec' is less useful than in Scheme (where it is the
;; way to construct recursive functions) but still sometimes useful;
;; say, when initializing a timer whose function needs to refer to
;; the timer itself.

(defmacro letrec-with (setq (&rest bindings) &body body
                       &environment env)
  (setf bindings (canonicalize-bindings bindings))
  `(let (,@(loop for (var init) in bindings
                 if (constantp init env)
                   collect `(,var ,init)
                 else collect var))
     (,setq
      ,@(loop for (var init) in bindings
              unless (constantp init env)
                append `(,var ,init)))
     (locally ,@body)))

(defmacro letrec ((&rest bindings) &body body)
  "Recursive LET.
The idea is that functions created in BINDINGS can close over one
another, and themselves.

Note that `letrec' only binds variables: it can define recursive
functions, but can't bind them as functions. (But see `fbindrec'.)"
  `(letrec-with psetq ,bindings
     ,@body))

(defmacro letrec* ((&rest bindings) &body body)
  "Like LETREC, but the bindings are evaluated in order.
See Waddell et al., *Fixing Letrec* for motivation.

Cf. `fbindrec*'."
  `(letrec-with setq ,bindings
     ,@body))

(defmacro receive (formals expr &body body)
  "Stricter version of `multiple-value-bind'.

Use `receive' when you want to enforce that EXPR should return a
certain number of values, or a minimum number of values.

If FORMALS is a proper list, then EXPR must return exactly as many
values -- no more and no less -- as there are variables in FORMALS.

If FORMALS is an improper list (VARS . REST), then EXPR must return at
least as many values as there are VARS, and any further values are
bound, as a list, to REST.

Lastly, if FORMALS is a symbol, bind that symbol to all the values
returned by EXPR, as if by `multiple-value-list'.

From Scheme (SRFI-8)."
  ;; It's probably not worth stack-allocating the thunk.
  (cond ((null formals)
         `(multiple-value-call
              (lambda ()
                ,@body)
            ,expr))
        ((atom formals)
         ;; This could also be written:
         #+(or) `(let ((,formals (multiple-value-list ,expr)))
                   ,@body)
         ;; But I want it to be possible to declare FORMALS to have
         ;; dynamic extent, and most Lisps support that for &rest
         ;; lists.
         `(multiple-value-call
              (lambda (&rest ,formals)
                ,@body)
            ,expr))
        ((proper-list-p formals)
         (when (intersection formals lambda-list-keywords)
           (error "Lambda list keywords in formals: ~a" formals))
         `(multiple-value-call
              (lambda ,formals
                ,@body)
            ,expr))
        (t (let* ((last (last formals))
                  (vars (append (butlast formals)
                                (list (car last))))
                  (rest (cdr last)))
             (when (intersection (cons rest vars) lambda-list-keywords)
               (error "Lambda list keywords in formals: ~a" formals))
             `(multiple-value-call
                  (lambda (,@vars &rest ,rest)
                    ,@body)
                ,expr)))))

;;; `mvlet'

;; TODO Should mvlet* allow bindings to be repeated in a single
;; binding form? It would be more consistent with let*.

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
             (let* ((bindings (canonicalize-bindings bindings))
                    (mvbinds (member-if-not #'simple-binding-p bindings))
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
         `(progn ,expr (values)))
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
         (let* ((bindings (canonicalize-bindings bindings))
                (b (first bindings)))
           `(multiple-value-bind ,(butlast b) ,(lastcar b)
              ,@body)))
        ((every #'simple-binding-p bindings)
         `(let ,bindings
            ,@body))
        (t (let* ((bindings (canonicalize-bindings bindings))
                  (binds
                    (mapcar #'butlast bindings))
                  (exprs
                    (mapcar #'lastcar bindings))
                  (temp-binds
                    (loop for vars in binds
                          collect (mapcar #'string-gensym vars)))
                  (temp-bindings
                    (loop for temp-bind in temp-binds
                          for expr in exprs
                          collect (append temp-bind (list expr))))
                  (rebindings
                    (loop for vars in binds
                          for temp-vars in temp-binds
                          append (loop for var in vars
                                       for temp-var in temp-vars
                                       collect (list var temp-var)))))
             `(mvlet* ,temp-bindings
                (let ,rebindings
                  ,@body))))))

;;; `and-let*'

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
false.

Also, this version makes the bindings immutable."
  (multiple-value-bind (body decls)
      (parse-body body)
    (labels ((expand (clauses body)
               (unsplice
                (ematch clauses
                  (() (if body `(progn ,@body) nil))
                  ((list* (and var (type symbol)) clauses)
                   `(and ,var ,@(expand clauses body)))
                  ((list* (list var expr) clauses)
                   (let ((temp (gensym (string var))))
                     (multiple-value-bind (local-decls other-decls)
                         (partition-declarations (list var) decls env)
                       ;; The use of the temporary here is so we still
                       ;; get style warnings if the variable is
                       ;; unused.
                       `(let* ((,temp ,expr)
                               (,var ,temp))
                          ,@local-decls
                          (and ,temp
                               ,@(expand clauses
                                         (append other-decls body)))))))
                  ((list* (list expr) clauses)
                   `(and ,expr ,@(expand clauses body)))))))
      (car (expand clauses body)))))

;;; Etc

;; These might be worth exporting if we can teach Slime to indent
;; them.

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

(defmacro if-not (test then &optional else)
  "If TEST evaluates to NIL, evaluate THEN and return its values,
otherwise evaluate ELSE and return its values. ELSE defaults to NIL."
  `(if (not ,test)
       ,then
       ,else))

(defmacro if-not-let (bindings &body (then-form &optional else-form))
  "Creates new variable bindings, and conditionally executes either
THEN-FORM or ELSE-FORM. ELSE-FORM defaults to NIL.
BINDINGS must be either single binding of the form:
 (variable initial-form)
or a list of bindings of the form:
 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))
All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.
If one of the variables was bound to NIL, the THEN-FORM is executed with the
bindings in effect, otherwise the ELSE-FORM is executed with the bindings in
effect.
Adapted from Alexandria if-let."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (if-not (and ,@variables)
	       ,then-form
	       ,else-form))))
