(defpackage :serapeum/macro-tools
  (:documentation "Tools for writing macros.")
  #+sb-package-locks (:lock t)
  (:use :cl :alexandria)
  (:import-from
   :introspect-environment
   :compiler-macroexpand
   :compiler-macroexpand-1
   :constant-form-value)
  (:import-from
   :tcr.parse-declarations-1.0
   :build-declarations
   :filter-declaration-env
   :parse-declarations)
  (:import-from
   :trivia
   :ematch
   :match)
  (:import-from
   :trivial-cltl2
   :variable-information)
  (:export
   :+merge-tail-calls+
   :callf
   :callf2
   :case-failure
   :define-case-macro
   :define-do-macro
   :define-post-modify-macro
   :eval-if-constant
   :expand-macro
   :expand-macro-recursively
   :expect-form-list
   :expect-single-form
   :make-unique-name-list
   :parse-defmethod-args
   :parse-leading-keywords
   :partition-declarations
   :string-gensym
   :unique-name
   :unparse-ordinary-lambda-list
   :unsplice
   :with-read-only-vars
   :with-thunk))

(in-package :serapeum/macro-tools)

;;;# Basics

(defun extract-function-name (x)
  "If possible, extract the name from X, a function designator."
  ;;; Borrowed from the internals of Alexandria.
  (match x
    ((list 'function name) name)
    ((list 'quote name) name)
    (otherwise nil)))

(defmacro rebinding-functions (bindings &body body)
  "Like `rebinding', but specifically for functions.
The value being rebound will be wrapped in `ensure-function'."
  (loop for var in bindings
        for name = (gensym)
        collect `(,name ,var) into renames
        collect ``(,,var (ensure-function ,,name)) into temps
        finally (return `(let* ,renames
                           (with-unique-names ,bindings
                             `(let (,,@temps)
                                ,,@body))))))

(defun normalize-cases (cases &key (allow-default t))
  "Normalize CASES, clauses for a case-like macro.
Return each non-default clause normalized to `(keys . body)', where
keys is a *list* of keys.

Return the default clause as a second value.

If there is more than one default clause, signal an error."
  (loop with default
        for (keys . body) in cases
        if (or (eql keys t)
               (eql keys 'otherwise))
          do (if default
                 (error "More than one default case in ~a" cases)
                 (if allow-default
                     (setf default body)
                     (error "Default disallowed in ~a" cases)))
        else collect (cons (ensure-list keys) body) into cases-out
        finally (return (values cases-out default))))

;;;## `string-gensym'
;;; I got sick of writing `(mapcar (compose #'gensym #'string) ...)'
;;; in every other macro.

(defun string-gensym (x)
  "Equivalent to (gensym (string x)).

Generally preferable to calling GENSYM with a string, because it
respects the current read table.

The alternative to writing `(mapcar (compose #'gensym #'string) ...)'
in every other macro."
  (gensym (string x)))

;;; These are more consistent with `with-unique-names'.

(defun unique-name (x)
  "Alias for `string-gensym'."
  (string-gensym x))

(defun make-unique-name-list (length &optional (x 'g))
  "Alias for `alexandria:make-gensym-list'."
  (make-gensym-list length (string x)))

;;;## `unsplice'
;;; Found this gem in the code for Lparallel.

(declaim (inline unsplice))
(defun unsplice (form)
  "If FORM is non-nil, wrap it in a list.

This is useful with ,@ in macros, and with `mapcan'.

E.g., instead of writing:

    `(.... ,@(when flag '((code))))

You can write:

    `(.... ,@(unsplice (when flag '(code))))

It may be especially helpful when splicing in variables. Instead of
writing:

    `(.... ,@(and docstring `(,docstring)))

You can simply write:

   `(.... ,@(unsplice docstring))

From Lparallel."
  (if form
      (list form)
      nil))

;;;## `with-thunk'
;;; This is useful, but the name could and should be improved.

(defmacro with-thunk ((spec &rest args) &body body)
  "A macro-writing macro for the `call-with-' style.

In the `call-with-' style of writing macros, the macro is simply a
syntactic convenience that wraps its body in a thunk and a call to the
function that does the actual work.

    (defmacro with-foo (&body body)
      `(call-with-foo (lambda () ,@body)))

The `call-with-' style has many advantages. Functions are easier to
write than macros; you can change the behavior of a function without
having to recompile all its callers; functions can be traced, appear
in backtraces, etc.

But meanwhile, all those thunks are being allocated on the heap. Can
we avoid this? Yes, but at a high cost in boilerplate: the closure has
to be given a name (using `flet') so it can be declared
`dynamic-extent'.

    (defmacro with-foo (&body body)
      (with-gensyms (thunk)
        `(flet ((,thunk () ,@body))
           (declare (dynamic-extent #',thunk))
           (call-with-foo #',thunk))))

`with-thunk' avoids the boilerplate:

    (defmacro with-foo (&body body)
      (with-thunk (body)
        `(call-with-foo ,body)))

You can give the thunk a name for easier debugging.

    (with-thunk ((body :name foo)) ...)

It is also possible to construct a \"thunk\" with arguments.

    (with-thunk (body foo)
      `(call-with-foo ,body))
    ≡ `(flet ((,thunk (,foo)
          ,@body))
        (declare (dynamic-extent #',thunk))
        (call-with-foo #',thunk))

Someday this may have a better name."
  ;; TODO Derive default name from &environment. Cf. log4cl.
  (destructuring-bind (var &key name) (ensure-list spec)
    (declare (type (and symbol (not null)) var)
             (type symbol name))
    (let* ((stack-fn-prefix (string 'stack-fn-))
           (stack-fn-name
             (or (concatenate 'string
                              stack-fn-prefix
                              (string (or name var)))))
           (stack-fn
             (gensym stack-fn-name)))
      (with-gensyms (b gargs)
        `(let ((,b ,var)
               (,var ',stack-fn)
               (,gargs (list ,@args)))
           `(flet ((,',stack-fn ,,gargs
                     ,@,b))
              (declare (dynamic-extent (function ,',stack-fn)))
              (symbol-macrolet ((,',stack-fn (function ,',stack-fn)))
                ,,@body)))))))

;;;# Expanding macros
;;; Expanding macros, Swank-style. We use `labels' in these
;;; definitions because `nlet' hasn't been defined yet.

(defun expand-macro (form &optional env)
  "Like `macroexpand-1', but also expand compiler macros.
From Swank."
  (multiple-value-bind (expansion expanded?)
      (macroexpand-1 form env)
    (if expanded?
        (values expansion t)
        (compiler-macroexpand-1 form))))

(defun expand-macro-recursively (form &optional env)
  "Like `macroexpand', but also expand compiler macros.
From Swank."
  (labels ((expand (form count)
             (multiple-value-bind (form expanded?)
                 (expand-macro form env)
               (if (not expanded?)
                   (values form (> count 0))
                   (expand form (1+ count))))))
    (expand form 0)))

;;;# Picking apart declarations

(defun partition-declarations (xs declarations &optional env)
  "Split DECLARATIONS into those that do and do not apply to XS.
Return two values, one with each set.

Both sets of declarations are returned in a form that can be spliced
directly into Lisp code:

     (locally ,@(partition-declarations vars decls) ...)"
  (let ((env2 (parse-declarations declarations env)))
    (flet ((build (env)
             (build-declarations 'declare env)))
      (if (null xs)
          (values nil (build env2))
          (values
           (build (filter-declaration-env env2 :affecting xs))
           (build (filter-declaration-env env2 :not-affecting xs)))))))

;;; `callf' and `callf2' are inspired by macros used in the
;;; implementation of Emacs Lisp's `cl' package.

(defmacro callf (function place &rest args &environment env)
  "Set PLACE to the value of calling FUNCTION on PLACE, with ARGS."
  (multiple-value-bind (vars vals stores setter getter)
      (get-setf-expansion place env)
    `(let* ,(mapcar #'list vars vals)
       (multiple-value-bind ,stores
           (funcall ,function ,getter ,@args)
         ,setter))))

(defmacro callf2 (function arg1 place &rest args)
  "Like CALLF, but with the place as the second argument."
  `(callf (curry ,function ,arg1) ,place ,@args))

(defmacro ensuring-functions  (vars &body body)
  `(let ,(loop for var in vars
               collect `(,var (ensure-function ,var)))
     ,@body))

(defmacro define-do-macro (name binds &body body)
  "Define an iteration macro like `dolist'.

Writing a macro like `dolist' is more complicated than it looks. For
consistency with the rest of CL, you have to do all of the following:

- The entire loop must be surrounded with an implicit `nil' block.
- The body of the loop must be an implicit `tagbody'.
- There must be an optional `return' form which, if given, supplies
  the values to return from the loop.
- While this return form is being evaluated, the iteration variables
  must be bound to `nil'.

Say you wanted to define a `do-hash' macro that iterates over hash
tables. A full implementation would look like this:

     (defmacro do-hash ((key value hash-table &optional return) &body body)
       (multiple-value-bind (body decls) (parse-body body)
         `(block nil
            (maphash (lambda (,key ,value)
                       ,@decls
                       (tagbody
                          ,@body))
                     ,hash-table)
            ,(when return
               `(let (,key ,value)
                  ,return)))))

Using `define-do-macro' takes care of all of this for you.

     (define-do-macro do-hash ((key value hash-table &optional return) &body body)
       `(maphash (lambda (,key ,value)
                   ,@body)
                 ,hash-table))"
  (let* ((opts (member '&optional (car binds)))
         (ret-var (cadr opts))
         ;; Handle both (key value table) and ((key value) table) as
         ;; well as ((key &optional value) table).
         (iter-vars (mappend (compose #'lambda-list-vars
                                      #'ensure-list)
                             (butlast (ldiff (car binds) opts))))
         (body-var (cadr (member '&body (cdr binds)))))
    (unless ret-var
      (error "No binding for return form in ~s" (car binds)))
    (unless body-var
      (error "No binding for body in ~s" binds))
    (unless iter-vars
      (error "No iteration vars in ~s" binds))
    (multiple-value-bind (body decls doc) (parse-body body :documentation t)
      `(defmacro ,name ,binds
         ,@(unsplice doc)
         ,@decls
         (multiple-value-bind (,body-var decls)
             (parse-body ,body-var)
           (let ((,body-var
                   `(,@decls
                     (tagbody ,@,body-var))))
             `(block nil
                ,,@body
                ,(when ,ret-var
                   `(let (,,@iter-vars)
                      (declare (ignorable ,,@iter-vars))
                      ,,ret-var)))))))))

(defmacro define-post-modify-macro (name lambda-list function &optional documentation)
  "Like `define-modify-macro', but arranges to return the original value."
  (labels ((parse (ll) (parse-ordinary-lambda-list ll))
           (pmm-lambda-list (ll)
             (multiple-value-bind (req opt rest key aok? aux key?) (parse ll)
               (declare (ignore key))
               (when (or key? aok?) (error "&key arguments not allowed."))
               (when aux (error "&aux arguments not allowed."))
               (values (append req (mapcar #'car opt))
                       rest)))
           (expand-pmm (args rest?)
             (with-gensyms (ref env)
               `(defmacro ,name (,ref ,@lambda-list &environment ,env)
                  ,@(unsplice documentation)
                  (let ((fn ',function) (rest? ',rest?)
                        (args (list ,@args)))
                    (multiple-value-bind (vars vals stores setter getter)
                        (get-setf-expansion ,ref ,env)
                      (with-gensyms (temp)
                        `(let* ,`(,@(mapcar #'list vars vals)
                                  (,temp ,getter)
                                  (,(car stores) (,fn ,temp ,@args ,@(unsplice rest?))))
                           ,setter
                           ,temp))))))))
    (multiple-value-bind (args rest?)
        (pmm-lambda-list lambda-list)
      (expand-pmm args rest?))))

(defun parse-leading-keywords (body)
  "Given BODY, return two values: a list of the leading inline keyword
arguments, and the rest of the body.

Inline keywords are like the keyword arguments to individual cases in
`restart-case'."
  (labels ((rec (keywords body)
             (match body
               ((list* (and kw (type keyword)) val body)
                (rec (list* val kw keywords)
                     body))
               ((list (and _ (type keyword)))
                (error "Invalid leading keywords in ~s" body))
               (otherwise
                (values (nreverse keywords) body)))))
    (rec nil body)))

(defmacro read-only-var (real-var &optional (name real-var))
  (declare (ignore name))
  `,real-var)

(defun (setf %read-only-var) (value var)
  (declare (ignore value))
  (error "~a is read-only in this environment"
         var))

(define-setf-expander read-only-var (real-var &optional (name real-var) &environment env)
  (warn "~a is read-only in this environment" name)
  (get-setf-expansion `(%read-only-var ',real-var) env))

(defun variable-special? (var &optional env)
  (if (fboundp 'trivial-cltl2:variable-information)
      (eql (funcall 'trivial-cltl2:variable-information var env) :special)
      nil))

(defun policy-quality (quality &optional env)
  "Query ENV for optimization declaration information.
Returns 1 when the environment cannot be accessed."
  (if (fboundp 'trivial-cltl2:declaration-information)
      (let ((alist (funcall 'trivial-cltl2:declaration-information 'optimize env)))
        (or (second (assoc quality alist))
            (error "Unknown policy quality ~s" quality)))
      (if (member quality '(speed safety space debug compilation-speed))
          1
          (error "Unknown policy quality ~s" quality))))

(defun policy> (env policy1 policy2)
  (> (policy-quality policy1 env)
     (policy-quality policy2 env)))

(defun speed-matters? (env)
  "Return T if ENV says we should prefer space to speed."
  (not (or (policy> env 'space 'speed)
           (policy> env 'compilation-speed 'speed))))

(defun variable-type (var &optional env)
  (if (fboundp 'trivial-cltl2:variable-information)
      (let ((alist (nth-value 2 (funcall 'trivial-cltl2:variable-information var env))))
        (or (cdr (assoc 'type alist))
            t))
      t))

(defmacro with-read-only-vars ((&rest vars) &body body &environment env)
  "Make VARS read-only within BODY.

That is, within BODY, each var in VARS is bound as a symbol macro,
which expands into a macro whose setf expander, in turn, is defined to
signal a warning at compile time, and an error at run time.

Depending on your Lisp implementation this may or may not do anything,
and may or may not have an effect when used on special variables."
  (declare (ignorable env))
  (case uiop:*implementation-type*
    ((:ccl :sbcl :cmu :acl)
     ;; The use of temps here, while it is ugly and annoying when
     ;; debugging, is necessary to prevent symbol-macrolet from going
     ;; into an infinite loop.
     (let* ((vars (loop for var in vars
                        unless (variable-special? var env)
                          collect var))
            (temps
              (loop for var in vars
                    collect (gensym (string var)))))
       `(let ,(mapcar #'list temps vars)
          (declare (ignorable ,@temps))
          (symbol-macrolet ,(loop for var in vars
                                  for temp in temps
                                  collect `(,var (read-only-var ,temp ,var)))
            ,@body))))
    (t
     `(locally ,@body))))

(defun expand-read-only-var (var env)
  (ematch var
    ((and var (type symbol))
     (let ((exp (macroexpand-1 var env)))
       (ematch exp
         ((list 'read-only-var (and storage (type symbol)) name)
          (assert (eql name var))
          storage))))))

;;; Macro-writing macro for writing macros like `case'.

;;; TODO Would it be worthwhile to look for clause bodies that are
;;; "the same", and merge them together? Or should we expect that any
;;; reasonable Common Lisp compiler will already do that? SBCL
;;; doesn't. But what would be the right predicate?

(defmacro define-case-macro (name macro-args params &body macro-body)
  "Define a macro like `case'.

A case-like macro is one that supports the following syntax:

- A list of keys is treated as matching any key in the list.
- An empty list matches nothing.
- The atoms T or `otherwise' introduce a default clause.
- There can only be one default clause.
- The default clause must come last.
- Any atom besides the empty list, T, or `otherwise' matches itself.

As a consequence of the above, to match against the empty list, T, or
`otherwise', they must be wrapped in a list.

    (case x
      ((nil) \"Matched nil.\")
      ((t) \"Matched t.\")
      ((otherwise) \"Matched `otherwise'.\")
      (otherwise \"Didn't match anything.\"))

A macro defined using `define-case-macro' can ignore all of the above.
It receives three arguments: the expression, already protected against
multiple evaluation; a normalized list of clauses; and, optionally, a
default clause.

The clauses are normalized as a list of `(key . body)', where each key
is an atom. (That includes nil, T, and `otherwise'.) Nonetheless, each
body passed to the macro will only appear once in the expansion; there
will be no duplicated code.

The body of the default clause is passed separately,
bound to the value of the `:default' keyword in PARAMS.

    (define-case-macro my-case (expr &body clauses)
        (:default default)
      ....)

Note that in this case, `default' will be bound to the clause's body
-- a list of forms -- and not to the whole clause. The key of the
default clause is discarded.

If no binding is specified for the default clause, then no default
clause is allowed.

One thing you do still have to consider is the handling of duplicated
keys. The macro defined by `define-case-macro' will reject case sets
that contains duplicate keys under `eql', but depending on the
semantics of your macro, you may need to check for duplicates under a
looser definition of equality.

As a final example, if the `case' macro did not already exist, you
could define it almost trivially using `define-case-macro':

    (define-case-macro my-case (expr &body clause)
        (:default default)
      `(cond
         ,@(loop for (key . body) in clauses
                 collect `((eql ,expr ,key) ,@body))
         (t ,@body)))"
  (multiple-value-bind (expr other-args clauses)
      (ematch macro-args
        ((list expr '&body clauses)
         (values expr nil clauses))
        ((list expr other-arg '&body clauses)
         (values expr (list other-arg) clauses)))
    (destructuring-bind (&key
                           error
                           (default (and error (gensym)))
                           (default-keys '(t otherwise)))
        params
      (let ((default-sym (or default (gensym)))
            (docstring (and (stringp (first macro-body))
                            (pop macro-body))))
        `(defmacro ,name (,expr ,@other-args &body ,clauses)
           ,@(unsplice docstring)
           (expand-case-macro
            (lambda (,expr ,default-sym ,clauses)
              (declare (ignorable ,default-sym))
              ;; If `default' is defined as `nil', then no default
              ;; clause is allowed.
              ,(when (null default)
                 `(when ,default-sym
                    (error "Default disallowed in ~a" ,clauses)))
              ,@macro-body)
            ,expr ,clauses
            :default-keys ',default-keys
            :error ',error
            :macro-name ',name))))))

(defun clauses+default (clauses &key (default-keys '(t otherwise)))
  (let ((default-clause-tails
          (loop for tail on clauses
                for clause = (first tail)
                for key = (first clause)
                when (member key default-keys :test #'eq)
                  collect tail)))
    (cond ((null default-clause-tails)
           (values clauses nil))
          ((rest default-clause-tails)
           (error "Multiple default clauses in ~a" clauses))
          (t
           (let ((default-tail (first default-clause-tails)))
             (if (rest default-tail)
                 (error "Default clause not last in ~a" clauses)
                 (let ((default (first default-tail)))
                   (values (remove default clauses)
                           (rest default)))))))))

(defun simplify-keylists (clauses)
  "Simplify the keylists in CLAUSES.

If the keylist is an empty list, omit the clause.

If the keylist is a list with one element, unwrap that element.

Otherwise, leave the keylist alone."
  (loop for clause in clauses
        for (keylist . body) = clause
        if (null keylist)
          do (progn)
        else if (and (listp keylist)
                     (null (rest keylist))
                     ;; Protect the key if the key is itself a list.
                     (atom (first keylist)))
               collect (cons (first keylist) body)
        else
          collect clause))

(defparameter *case-macro-target*
  (case uiop:*implementation-type*
    ((:sbcl :cmu) 'flet)
    (t 'tagbody))
  "Implementation-appropriate target syntax clause deduplication.
How should repeated clauses in a case macro be deduplicated? With flet
or a tagbody?")

(defun expand-case-macro (cont expr clauses
                          &key (default-keys '(t otherwise)) error
                               (macro-name 'custom-case))
  (check-type clauses list)
  (when (eql error t)
    (setf error 'case-failure))
  (let ((cont
          (lambda (expr-temp default clauses)
            (assert (symbolp expr-temp))
            (assert (listp default))
            (assert (listp clauses))
            (funcall cont expr-temp default clauses)))
        (expr-temp (gensym (format nil "~a-~a"
                                   macro-name 'key))))
    ;; Rebind expr.
    `(let ((,expr-temp ,expr))
       ,(multiple-value-bind (clauses default)
            (clauses+default clauses :default-keys default-keys)
          (let* ((clauses (simplify-keylists clauses))
                 (keys (mapcar #'first clauses))
                 (flat-keys (mappend #'ensure-list keys))
                 (clauses
                   (or (and error
                            (or default-keys
                                (error "Cannot add an error clause without a default key."))
                            (append clauses
                                    (list `(,(random-elt default-keys)
                                             (,error ,expr-temp ',flat-keys)))))
                       clauses)))
            (when (< (length (remove-duplicates flat-keys))
                     (length flat-keys))
              (error "Duplicated keys in ~s" keys))
            (if (every #'atom keys)     ;NB Nil could be a key.
                ;; Easy case. No lists of keys; do nothing special.
                (funcall cont expr-temp default clauses)
                ;; This could be done two ways: with flet or with
                ;; tagbody. Switching is straightforward: just swap
                ;; `expand-case-macro/flet' for
                ;; `expand-case-macro/tagbody', or vice versa. (It
                ;; might even be worth using different expansions on
                ;; different Lisps.)
                (let ((expander
                        (ecase *case-macro-target*
                          ((flet) #'expand-case-macro/flet)
                          ((tagbody) #'expand-case-macro/tagbody))))
                  (funcall expander
                           cont expr-temp clauses default
                           :macro-name macro-name))))))))

(defun expand-case-macro/common (clauses &key jump macro-name)
  (check-type jump function)
  (check-type macro-name symbol)
  (labels ((gen-fn-sym ()
             (gensym (concatenate 'string (string macro-name) "-" #.(string 'fn))))
           (rec (clauses dest-acc clauses-acc)
             (if (null clauses)
                 (values (reverse dest-acc)
                         (reverse clauses-acc))
                 (destructuring-bind ((keys . body) . rest-clauses) clauses
                   (if (atom keys)      ;Remember nil could be a key.
                       (rec rest-clauses
                            dest-acc
                            (cons (first clauses) clauses-acc))
                       (let* ((sym (gen-fn-sym))
                              (dest (cons sym body))
                              (body (list (funcall jump sym))))
                         (rec rest-clauses
                              (cons dest dest-acc)
                              (revappend (loop for key in keys
                                               collect (cons key body))
                                         clauses-acc))))))))
    (rec clauses nil nil)))

(defun expand-case-macro/flet (cont expr-temp normal-clauses default &key macro-name)
  (multiple-value-bind (dests clauses)
      (expand-case-macro/common normal-clauses
                                :jump (lambda (sym)
                                        `(,sym))
                                :macro-name macro-name)
    (let ((fns (loop for (sym . body) in dests
                     collect `(,sym () ,@body))))
      `(flet ,fns
         ,(funcall cont expr-temp default clauses)))))

(defun expand-case-macro/tagbody (cont expr-temp normal-clauses default &key macro-name)
  (let ((case-block (gensym (format nil "~a-~a" macro-name 'block))))
    (multiple-value-bind (dests clauses)
        (expand-case-macro/common normal-clauses
                                  :jump (lambda (sym)
                                          `(go ,sym)))
      `(block ,case-block
         (tagbody
            (return-from ,case-block
              ,(funcall cont expr-temp default clauses))
            ,@(loop for (sym . body) in dests
                    append `(,sym (return-from ,case-block
                                    (progn ,@body)))))))))

(define-condition case-failure (type-error)
  ()
  (:documentation "A subtype of type-error specifically for case failures."))

(defun case-failure (expr keys)
  "Signal an error of type `case-failure'."
  (error 'case-failure
         :datum expr
         :expected-type `(member ,@keys)))

(defun lambda-list-vars (lambda-list)
  "Return a list of the variables bound in LAMBDA-LIST, an ordinary
lambda list."
  (multiple-value-bind (req opt rest keys allow-other-keys? aux keyp)
      (parse-ordinary-lambda-list lambda-list)
    (declare (ignore allow-other-keys? keyp))
    (remove nil
            (append req
                    (mapcar #'first opt)
                    (mapcar #'third opt)
                    (list rest)
                    (mapcar (compose #'first #'rest #'first) keys)
                    (mapcar #'third keys)
                    (mapcar #'first aux)))))

(defun eval-if-constant (form &optional env)
  "Try to reduce FORM to a constant, using ENV.
If FORM cannot be reduced, return it unaltered.

Also return a second value, T if the form could be reduced to a
constant, or nil otherwise. \(Note that the second value may be T if
FORM was already a constant; think of it as a \"green light\" to treat
the value as a constant.)

This is equivalent to testing if FORM is constant, then evaluating it,
except that FORM is macro-expanded in ENV (taking compiler macros into
account) before doing the test.

Note that this function may treat a form as constant which would not
be recognized as such by `constantp', because we also expand compiler
macros."
  (labels ((eval-if-constant (form env)
             (cond ((constantp form)
                    (values (eval form) t))
                   ((and env (constantp form env))
                    ;; Use the implementation's expander via introspect-environment.
                    (let ((value (constant-form-value form env)))
                      (if (constantp value)
                          (values value t)
                          ;; Not every Lisp has a functioning
                          ;; `constant-form-value', so if it failed,
                          ;; it's still worth trying macroexpansion
                          ;; (compiler macros too).
                          (expand-and-retry form env))))
                   (t (expand-and-retry form env))))
           (expand-and-retry (form env)
             (multiple-value-bind (exp exp?)
                 (expand-macro-recursively form env)
               (if (not exp?)
                   (values form nil)
                   (eval-if-constant exp env)))))
    (eval-if-constant form env)))

(defmacro declaim-maybe-inline-1 (fn)
  (declare (ignorable fn))
  #+sbcl `(declaim (sb-ext:maybe-inline ,fn))
  #+cmucl `(declaim (ext:maybe-inline ,fn)))

(defmacro declaim-maybe-inline (&rest fns)
  `(progn
     ,@(loop for fn in fns
             collect `(declaim-maybe-inline-1 ,fn))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +merge-tail-calls+
      ;; Cf. https://0branch.com/notes/tco-cl.html#sec-2-5.

      ;; On SBCL debug=0 is sufficient to deactivate insert-debug-catch,
      ;; and to trigger recognize-self-calls (as long as one of speed or
      ;; space is greater than 0).

      ;; CCL does TCO as long as debug<3.

      ;; LispWorks merges tail calls as long as debug<3.

      ;; Allegro will only optimize non-self tail calls if debug<3 and
      ;; speed>2.
      '(declare (optimize (debug 0)
                 #+sbcl (space 1)
                 #+allegro (speed 3)))
    :test 'equal
    :documentation "Try to ensure that tail calls will be merged.

If you just want portable self-calls, for writing loops using
recursion, use `nlet' or `defloop' instead.

This may not work at all on some Lisps."))

(defparameter *forbidden-heads*
  '(progn locally prog1 prog2 prog prog* declare tagbody
    block tagbody progv
    when unless cond if or and
    case ecase ccase
    typecase ctypecase etypecase
    let let* multiple-value-bind)
  "Symbols that should not occur in the head of a list of forms.
E.g. `progn', `locally'.")

(defun expect-form-list (exp)
  "Sanity-check EXP, a macro expansion, assuming it is supposed to be
  a series of forms suitable for splicing into a progn (implicit or
  explicit.)"
  (if (or (not (listp exp))
          (member (car exp) *forbidden-heads*))
      (error "A list of forms was expected, but this appears to be a single form:~%~s"
             exp)
      exp))

(defun expect-single-form (exp)
  "Sanity-check EXP, a macro expansion, assuming it is supposed to be
  a single form suitable for inserting intact."
  (if (match exp
        ((and _ (type atom)) t)
        ((list* (list* 'lambda _) _) t)
        ((list* (and _ (type symbol)) _) t)
        (otherwise nil))
      exp
      (error "A single form was expected, but this appears to be a list of forms:~%~s"
             exp)))

(defun unparse-ordinary-lambda-list (&optional required optional rest keywords aok? aux key?)
  "Put together an ordinary lambda list from its constituent parts.

This is the inverse of `alexandria:parse-ordinary-lambda-list'.

    lambda-list
    ≡ (multiple-value-call #'unparse-ordinary-lambda-list
        (parse-ordinary-lambda-list lambda-list)"
  (let ((optional
          (mapcar (lambda (spec)
                    (match spec
                      ((list var init nil)
                       (list var init))
                      (otherwise spec)))
                  optional))
        (keywords
          (mapcar (lambda (spec)
                    (match spec
                      ((list (list keyword-name name) init nil)
                       (list (list keyword-name name) init))
                      (otherwise spec)))
                  keywords)))
    `(,@required
      ,@(and optional `(&optional ,@optional))
      ,@(and rest `(&rest ,rest))
      ,@(and (or key? keywords)
             `(&key ,@keywords))
      ,@(and aok? '(&allow-other-keys))
      ,@(and aux `(&aux ,@aux)))))

(defun parse-defmethod-args (args)
  "Parse the args to defmethod (everything except the name).
Returns three values: a list of qualifiers, the specialized
lambda-list, and the forms that make up the body."
  (let* ((lambda-list.body (member-if (of-type 'list) args))
         (qualifiers (ldiff args lambda-list.body))
         (lambda-list (car lambda-list.body))
         (body (cdr lambda-list.body)))
    (values qualifiers
            lambda-list
            body)))

(defun gensym? (x)
  "Is X a gensym'd (uninterned) symbol?"
  (and (symbolp x) (not (symbol-package x))))

(defun simple-lambda-list? (lambda-list)
  "A lambda list with no inits."
  (multiple-value-bind (req opt rest keys other-keys aux keyp)
      (parse-ordinary-lambda-list lambda-list)
    (declare (ignore req rest other-keys keyp))
    (notany (disjoin #'second #'third)
            (append opt keys aux))))

;;; TODO Handle let*, mvbind?
(defun let-over-lambda (form lexenv)
  "Expand form, using `expand-macro'. If the result is a simple let-over-lambda,
analyze it into an environment, declarations, and a lambda."
  #.+merge-tail-calls+
  (match form
    ;; Special cases for `complement` and `constantly`.
    ((list 'complement fn)
     (with-gensyms (temp)
       (values `((,temp (ensure-function ,fn)))
               `((function ,temp))
               `(lambda (&rest args)
                  (declare (dynamic-extent args))
                  (not (apply ,temp args))))))
    ((list 'constantly x)
     (with-gensyms (temp)
       (values `((,temp ,x))
               nil
               `(lambda (&rest args)
                  (declare (ignore args))
                  ,temp))))
    ;; NB Disjoin, conjoin, and rcurry don't have compiler macros (why
    ;; not?).
    ((list* (and fun (or 'conjoin 'disjoin)) pred preds)
     (let* ((preds (cons pred preds))
            (temps (loop for nil in preds collect (gensym))))
       (values (mapcar (lambda (temp pred)
                         `(,temp (ensure-function ,pred)))
                       temps preds)
               nil
               `(lambda (&rest args)
                  (,(locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
                      (case fun
                        (conjoin 'and)
                        (disjoin 'or)))
                   ,@(loop for temp in temps
                           collect `(apply ,temp args)))))))
    ((list* 'rcurry fun args)
     (let ((tempfn (string-gensym 'fn))
           (temps (loop for nil in args collect (gensym))))
       (values `((,tempfn (ensure-function ,fun))
                 ,@(loop for temp in temps
                         for arg in args
                         collect `(,temp ,arg)))
               nil
               `(lambda (&rest more)
                  (declare (dynamic-extent more))
                  (multiple-value-call ,tempfn (values-list more) ,@temps)))))
    ;; TODO Special-case partial. We should be smart enough to see through this.
    ;; ((list* 'partial fn args)
    ;;  (let-over-lambda `(curry ,fn ,@args) lexenv))
    ;; A plain lambda.
    ((or (list* 'lambda args body)
         (list 'function (list* 'lambda args body)))
     (values nil nil `(lambda ,args ,@body)))
    ((list 'ensure-function fn)
     (let-over-lambda fn lexenv))
    ((cons 'locally body)
     (multiple-value-bind (forms decls)
         (parse-body body)
       (let-over-lambda
        `(let ()
           ,@decls
           ,@forms)
        lexenv)))
    ;; A literal lambda as the function.
    ((list* (list* 'lambda lambda-list body) arguments)
     ;; Just rewrite it as a let, if possible.
     (if (intersection lambda-list lambda-list-keywords)
         (trivia.fail:fail)
         (let-over-lambda
          `(let ,(mapcar #'list lambda-list arguments)
             ,@body)
          lexenv)))
    ;; let* with single binding. Note that Clozure, at least, expands
    ;; let with only one binding into let*.
    ((list* 'let* (list binding) body)
     (let-over-lambda `(let ,binding ,@body) lexenv))
    ;; let-over-lambda.
    ((list* 'let (and bindings (type list)) body)
     (multiple-value-bind (forms decls)
         (parse-body body)
       (match forms
         ((list
           (or (list* 'lambda args body)
               (list 'function (list* 'lambda args body))))
          (if (every #'gensym? (mapcar #'ensure-car bindings))
              ;; If all the bindings are gensyms, don't worry about
              ;; shadowing or duplicates.
              (values bindings
                      (remove 'optimize
                              (mappend #'cdr decls)
                              :key #'car)
                      `(lambda ,args ,@body))
              ;; Otherwise, we have to rebind some variables.
              (multiple-value-bind (bindings rebindings)
                  (let ((binds '())
                        (rebinds '()))
                    (loop for binding in bindings
                          for (var init) = (if (listp binding)
                                               binding
                                               (list binding nil))
                          if (gensym? var)
                            do (push (list var init) binds)
                          else do (let ((temp (string-gensym var)))
                                    (push `(,temp ,init) binds)
                                    (push `(,var ,temp) rebinds)))
                    (values (nreverse binds)
                            (nreverse rebinds)))
                (values bindings
                        ;; TODO
                        nil
                        (if (simple-lambda-list? args)
                            ;; The lambda list has no inits, so we don't
                            ;; have to worry about refs to the vars
                            ;; being rebound.
                            `(lambda ,args
                               (symbol-macrolet ,rebindings
                                 ,@body))
                            (with-gensyms (temp-args)
                              ;; The lambda list might refer to the
                              ;; rebindings, so leave it to the
                              ;; compiler.
                              `(lambda (&rest ,temp-args)
                                 (declare (dynamic-extent ,temp-args))
                                 (symbol-macrolet ,rebindings
                                   (apply (lambda ,args
                                            ,@body)
                                          ,temp-args))))))))))))
    (otherwise (let ((exp (expand-macro form lexenv)))
                 (if (eq exp form)
                     nil
                     (let-over-lambda exp lexenv))))))
