(in-package :serapeum)
(in-readtable :fare-quasiquote)

;;;# Basics

;;; Borrowed from the internals of Alexandria.

(defun extract-function-name (x)
  "If possible, extract the name from X, a function designator."
  (match x
    (`(function ,name) name)
    (otherwise x)))

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

From Lparallel."
  (if form
      (list form)
      nil))

;;;## `with-thunk'
;;; This is useful, but the name could and should be improved.

(defmacro with-thunk ((var &rest args) &body body)
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

It is also possible to construct a \"thunk\" with arguments.

    (with-thunk (body foo)
      `(call-with-foo ,body))
    ≡ `(flet ((,thunk (,foo)
          ,@body))
        (declare (dynamic-extent #',thunk))
        (call-with-foo #',thunk))

Someday this may have a better name."
  (let* ((stack-thunk-prefix (string 'stack-fn-))
         (stack-thunk-name
           (concatenate 'string
                        stack-thunk-prefix
                        (string var)))
         (stack-thunk
           (gensym stack-thunk-name)))
    (with-gensyms (b gargs)
      `(let ((,b ,var)
             (,var ',stack-thunk)
             (,gargs (list ,@args)))
         `(flet ((,',stack-thunk ,,gargs
                   ,@,b))
            (declare (dynamic-extent (function ,',stack-thunk)))
            (symbol-macrolet ((,',stack-thunk (function ,',stack-thunk)))
              ,,@body))))))

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
  (labels ((expand (form)
             (multiple-value-bind (form expanded?)
                 (expand-macro form env)
               (if (not expanded?)
                   form
                   (expand form)))))
    (expand form)))

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

(defmacro seq-dispatch (seq &body (list-form array-form &optional other-form))
  "Efficiently dispatch on the type of SEQ."
  (declare (ignorable other-form))
  (let* ((list-form
           `(with-read-only-vars (,seq)
              ,list-form))
         (array-form
           `(with-read-only-vars (,seq)
              ,array-form))
         (list-form
           `(let ((,seq (truly-the list ,seq)))
              (declare (ignorable ,seq))
              ,list-form))
         (vector-form
           ;; Create a separate branch for simple vectors.
           `(if (simple-vector-p ,seq)
                (let ((,seq (truly-the simple-vector ,seq)))
                  (declare (ignorable ,seq))
                  (with-vref simple-vector
                    ,array-form))
                (let ((,seq (truly-the vector ,seq)))
                  (declare (ignorable ,seq))
                  ,array-form))))
    #+ccl `(ccl::seq-dispatch ,seq ,list-form ,vector-form)
    ;; Only SBCL and ABCL support extensible sequences right now.
    #+(or sbcl abcl)
    (once-only (seq)
      `(if (listp ,seq)
           ,list-form
           ,(if other-form
                `(if (arrayp ,seq)
                     ,vector-form
                     ,other-form)
                ;; Duplicate the array form so that, hopefully, `elt'
                ;; will be compiled to `aref', &c.
                `(if (arrayp ,seq)
                     ,vector-form
                     ,other-form))))
    #-(or sbcl abcl ccl)
    `(if (listp ,seq) ,list-form ,vector-form)))

(defmacro vector-dispatch (vec &body (bit-vector-form vector-form))
  "Efficiently dispatch on the type of VEC.
The first form provides special handling for bit vectors. The second
form provides generic handling for all types of vectors."
  `(cond ((typep ,vec 'simple-bit-vector)
          (let ((,vec (truly-the simple-bit-vector ,vec)))
            (declare (ignorable ,vec))
            (with-vref simple-bit-vector
              ,bit-vector-form)))
         ((typep ,vec 'bit-vector)
          (let ((,vec (truly-the bit-vector ,vec)))
            (declare (ignorable ,vec))
            (with-vref bit-vector
              ,bit-vector-form)))
         ;; Omitted so we can safely nest within with-vector-dispatch.
         ;; ((typep ,vec 'simple-vector)
         ;;  (let ((,vec (truly-the simple-vector ,vec)))
         ;;    (declare (ignorable ,vec))
         ;;    (with-vref simple-vector
         ;;      ,vector-form)))
         (t
          (let ((,vec (truly-the vector ,vec)))
            (declare (ignorable ,vec))
            ,vector-form))))

;;; `callf' and `callf2' are extracted from the guts of Emacs Lisp's
;;; `cl' package.

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
                        `(let* ,`(,@(mapcar #'list
                                            (mapcar #'car vars)
                                            (mapcar #'car vals))
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
  (or (introspect-environment:specialp var env)
      ;; TODO This should be in introspect-environment proper.
      #+allegro
      (eql (sys:variable-information var env) :special)))

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
     `(progn ,@body))))

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
                ;; tagbody. I'm opting to use the version with flet
                ;; here, as I think it results in a more readable
                ;; expansion, and probably makes the job of type
                ;; inference easier. If it proves too expensive,
                ;; however, switching back to tagbody is
                ;; straightforward: just swap out
                ;; `expand-case-macro/flet' for
                ;; `expand-case-macro/tagbody'. (It might even be
                ;; worth using different expansions on different
                ;; Lisps.)
                (expand-case-macro/flet cont expr-temp clauses default
                                        :macro-name macro-name)))))))

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
            ,@(apply #'append dests))))))

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

Also return a second value, T if the form was reduced, or nil
otherwise.

This is equivalent to testing if FORM is constant, then evaluting it,
except that FORM is macro-expanded in ENV (taking compiler macros into
account) before doing the test.

Note that this function may treat a form as constant which would not
be recognized as such by `constantp', because we also expand compiler
macros."
  (cond ((constantp form)
         (values (eval form) t))
        ((and env (constantp form env))
         ;; Use the implementation's expander via introspect-environment.
         (let ((value (constant-form-value form env)))
           (if (constantp form)
               (values value t)
               ;; It failed, let's try macroexpanding.
               (eval-if-constant form nil))))
        (t
         (let ((exp (expand-macro-recursively form env)))
           (if (constantp exp)
               (values (eval exp) t)
               (values form nil))))))

(defmacro declaim-maybe-inline-1 (fn)
  (declare (ignorable fn))
  #+sbcl `(declaim (sb-ext:maybe-inline ,fn))
  #+cmucl `(declaim (ext:maybe-inline ,fn)))

(defmacro declaim-maybe-inline (&rest fns)
  `(progn
     ,@(loop for fn in fns
             collect `(declaim-maybe-inline-1 ,fn))))
