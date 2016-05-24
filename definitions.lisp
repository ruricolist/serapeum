(in-package :serapeum)

;;;# Lexical globals

;;; `def' and `defconst' are both applications of the same idea: using
;;; symbol macros to get lexical behavior from global variables (via
;;; `defparameter' and `defconstant', respectively).

(defmacro def (var &body (&optional val documentation))
  "The famous \"deflex\".

Define a top level (global) lexical VAR with initial value VAL,
which is assigned unconditionally as with DEFPARAMETER. If a DOC
string is provided, it is attached to both the name |VAR| and the name
*STORAGE-FOR-DEFLEX-VAR-|VAR|* as a documentation string of kind
'VARIABLE. The new VAR will have lexical scope and thus may be
shadowed by LET bindings without affecting its dynamic (global) value.

It is possible for VAL to close over VAR.

In implementations that support it (SBCL and CCL, at the moment) this
version creates a backing variable that is \"global\" or \"static\",
so there is not just a change in semantics, but also a gain in
efficiency.

If VAR is a list that starts with `values`, each element is treated as
a separate variable and initialized as if by `(setf (values VAR...)
VAL)`.

The original `deflex' is due to Rob Warnock."
  (ematch var
    ((list 'values)
     `(progn ,val))
    ((list 'values var)
     `(def ,var ,val ,@(unsplice documentation)))
    ((list* 'values vars)
     `(mvdef ,vars ,val ,@(unsplice documentation)))
    ((and var (type (and symbol (not null))))
     (let* ((s0 (symbol-name '#:*storage-for-deflex-var-))
            (s1 (symbol-name var))
            (s2 (symbol-name '#:*))
            (s3 (symbol-package var))	; BUGFIX [see above]
            (backing-var (intern (concatenate 'string s0 s1 s2) s3)))
       ;; Note: The DEFINE-SYMBOL-MACRO must precede VAL so VAL can close
       ;; over VAR.
       `(progn
          (define-symbol-macro ,var ,backing-var)
          (global-vars:define-global-parameter* ,backing-var ,val
            ,@(unsplice documentation))
          ',var)))))

(defmacro mvdef (vars &body (&optional expr documentation))
  `(progn
     ,@(loop for var in vars
             collect `(def ,var nil ,@(unsplice documentation)))
     (setf (values ,@vars) ,expr)
     ',vars))

(defmacro define-values (values &body (expr))
  "Like `def', but for multiple values.
Each variable in VALUES is bound as with `def', then set all at once
as with `multiple-value-setq'."
  `(progn
     ,@(loop for v in values collect `(def ,v nil))
     (setf (values ,@values) ,expr)
     (values ,@(loop for v in values collect `(quote ,v)))))

(defun same-literal-p (x y)
  "A predicate that compares whether X and Y have the same literal
  representation."
  (or (equal x y)
      ;; Crude, but reliable.
      (handler-case
          ;; "If ‘*read-eval*’ is false and ‘*print-readably*’ is true,
          ;; any such method that would output a reference to the
          ;; "‘#.’" reader macro will either output something else or
          ;; will signal an error (as described above)."
          (let ((*read-eval* t))
            (string= (write-to-string x :readably t)
                     (write-to-string y :readably t)))
        (print-not-readable () nil))))

(defmacro defconst (symbol init &optional docstring)
  "Define a constant, lexically.

`defconst' defines a constant using a strategy similar to `def', so
you don’t have to +cage+ your constants.

The constant is only redefined on re-evaluation if INIT has a
different literal representation than the old value.

The name is from Emacs Lisp."
  (let ((backing-var (symbolicate '#:+storage-for-deflex-var- symbol '+)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (define-constant ,backing-var ,init
         :test 'same-literal-p
         :documentation ,docstring)
       (define-symbol-macro ,symbol ,backing-var))))

;;;# Emacs-alikes

;;;## `defsubst'

;;; This is of course by way of Emacs, although it's actually much
;;; older.

(defmacro defsubst (name params &body body)
  "Define an inline function.

     (defsubst fn ...)
     ≡ (declaim (inline fn))
       (defun fn ...)

The advantage of a separate defining form for inline functions is that
you can't forget to declaim the function inline before defining it –
without which it may not actually end up being inlined.

From Emacs and other ancient Lisps."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,params
       ,@body)))

;;;## `defalias'

;;; In Emacs `defalias' is really just `fset': both the name and the
;;; value are evaluated. Here we only evaluate the value.

(defmacro defalias (alias &body (def &optional docstring))
  "Define a value as a top-level function.

     (defalias string-gensym (compose #'gensym #'string))

Like (setf (fdefinition ALIAS) DEF), but with a place to put
documentation and some niceties to placate the compiler.

Name from Emacs Lisp."
  `(progn
     ;; Give the function a temporary definition at compile time so
     ;; the compiler doesn't complain about its being undefined.
     (declaim (notinline ,alias))
     (eval-when (:compile-toplevel)
       (unless (fboundp ',alias)
         (defun ,alias (&rest args)
           (declare (ignore args)))))
     (eval-when (:load-toplevel :execute)
       (compile ',alias ,def)
       ,@(unsplice
          (when docstring
            `(setf (documentation ',alias 'function) ,docstring))))
     ',alias))

;;;# Etc

(defmacro defplace (name args &body (form &optional docstring))
  "Define NAME and (SETF NAME) in one go.

Note that the body must be a single, setf-able expression."
  (with-gensyms (value)
    `(progn
       (defun ,name ,args
         ,@(unsplice docstring)
         ,form)
       (defun (setf ,name) (,value ,@args)
         (setf ,form ,value)))))

(defmacro defcondition (name supers &body (slots &rest options))
  "Alias for `define-condition'.

Like (define-condition ...), but blissfully conforming to the same
nomenclatural convention as every other definition form in Common
Lisp."
  `(define-condition ,name ,supers
     ,slots
     ,@options))


;; Internal definitions.

(defvar *in-let*)
(defvar *orig-form*)
(defvar *symbol-macros*)

(defmacro nonlocal (&body body)
  `(progn ,@body))

(defmacro local* (&body body)
  "Like `local', but leave the last form in BODY intact.

     (local*
       (defun aux-fn ...)
       (defun entry-point ...))
     =>
     (labels ((aux-fn ...))
       (defun entry-point ...)) "
  `(local
     ,@(butlast body)
     (nonlocal ,@(last body))))

(defun initialize-binds-from-decls (binds-in decls &optional env)
  "Given a list of bindings and a set of declarations, look for any
uninitialized bindings. If there are any, try to extrapolate
reasonable initialization values for them based on the declarations in
DECLS."
  (labels ((var-decls-type (var decls)
             (let ((bind-decls (partition-declarations (list var) decls)))
               (block decls-type
                 (loop (dolist (decl bind-decls)
                         (print decl)
                         (match decl
                           ((list 'declare (list 'type typespec (and _ (eql var))))
                            (return-from decls-type typespec))
                           ((list 'declare (list typespec (and _ (eql var))))
                            (return-from decls-type typespec))))
                       ;; If we can't find a declaration, default to `t`.
                       (return-from decls-type t)))))
           ;; Curiously, alexandria:type= doesn't take an environment arg.
           (type=? (x y)
             (and (subtypep x y env)
                  (subtypep y x env)))
           (initialization-value (var decls)
             (let ((type (var-decls-type var decls)))
               (cond
                 ;; Needless; this is the default behavior.
                 #+ () ((type=? type 'boolean) nil)
                 ((or (type=? type 'bit)
                      (type=? type 'fixnum)
                      (type=? type 'array-index)
                      (subtypep type 'unsigned-byte env))
                  0)
                 ((type=? type '(complex integer)) #C(0 0))
                 ((type=? type 'single-float) 0f0)
                 ((type=? type 'double-float) 0d0)
                 ((type=? type 'float) 0.0)
                 ((type=? type '(complex single-float)) #C(0f0 0f0))
                 ((type=? type '(complex double-float)) #C(0d0 0d0))
                 ((type=? type '(complex float)) #C(0.0 0.0))
                 ;; Strings aren't immutable.
                 #+ () ((type=? type 'string) (make-string 0))
                 ((type=? type 'pathname) 'uiop:*nil-pathname*)
                 ((type=? type 'function) '#'identity)
                 ;; TODO More character types? E.g. base-char on SBCL?
                 ((type=? type 'character) (code-char 0)))))
           (binds-out (binds-in decls binds-out)
             (if (endp binds-in)
                 (nreverse binds-out)
                 (let ((var (first binds-in)))
                   (if (listp var)
                       ;; Already initialized.
                       (let ((binds-in (rest binds-in))
                             (binds-out (cons var binds-out)))
                         (binds-out binds-in decls binds-out))
                       (let* ((init (initialization-value var decls))
                              (bind (if init `(,var ,init) var)))
                         (binds-out (rest binds-in)
                                    decls
                                    (cons bind binds-out))))))))
    ;; Shortcut: no declarations, just return the bindings.
    (if (null decls)
        binds-in
        (binds-out binds-in decls '()))))

(defmacro let-initialized (bindings &body body &environment env)
  "Like LET, but if any of BINDINGS are uninitialized, try to give
them sane initialization values."
  (multiple-value-bind (body decls) (parse-body body)
    `(let ,(initialize-binds-from-decls bindings decls env)
       ,@decls
       ,@body)))

(defmacro local (&body orig-body &environment env)
  "Make internal definitions using top-level definition forms.

Within `local' you can use top-level definition forms and have them
create purely local definitions, like `let', `labels', and `macrolet':

     (fboundp 'plus) ; => nil

     (local
       (defun plus (x y)
         (+ x y))
       (plus 2 2))
     ;; => 4

     (fboundp 'plus) ; => nil

Each form in BODY is subjected to partial expansion (with
`macroexpand-1') until either it expands into a recognized definition
form (like `defun') or it can be expanded no further.

\(This means that you can use macros that expand into top-level
definition forms to create local definitions.)

Just as at the real top level, a form that expands into `progn' (or an
equivalent `eval-when') is descended into, and definitions that occur
within it are treated as top-level definitions.

\(Support for `eval-when' is incomplete: `eval-when' is supported only
when it is equivalent to `progn').

The recognized definition forms are:

- `def', for lexical variables (as with `letrec')
- `define-values', for multiple lexical variables at once.
- `defun', for local functions (as with `labels')
- `defalias', to bind values in the function namespace (like `fbindrec*')
- `declaim', to make declarations (as with `declare')
- `defconstant' and `defconst', which behave exactly like symbol macros
- `define-symbol-macro', to bind symbol macros (as with `symbol-macrolet')

Also, with serious restrictions, you can use:

- `defmacro', for local macros (as with `defmacro')

\(Note that the top-level definition forms defined by Common Lisp
are (necessarily) supplemented by three from Serapeum: `def',
`define-values', and `defalias'.)

The exact order in which the bindings are made depends on how `local'
is implemented at the time you read this. The only guarantees are that
variables are bound sequentially; functions can always close over the
bindings of variables, and over other functions; and macros can be
used once they are defined.

     (local
       (def x 1)
       (def y (1+ x))
       y)
     => 2

     (local
       (defun adder (y)
         (+ x y))
       (def x 2)
       (adder 1))
     => 3

Perhaps surprisingly, `let' forms (as well as `let*' and
`multiple-value-bind') *are* descended into; the only difference is
that `defun' is implicitly translated into `defalias'. This means you
can use the top-level idiom of wrapping `let' around `defun'.

    (local
      (let ((x 2))
        (defun adder (y)
          (+ x y)))
      (adder 2))
    => 4

Support for macros is sharply limited.

1. Macros defined with `defmacro' must precede all other expressions.

2. Macros and symbol macros cannot be defined inside of binding forms
like `let'.

3. `macrolet' is not allowed at the top level of a `local' form.

These restrictions are undesirable, but well justified: it is
impossible to handle the general case both correctly and portably, and
while some special cases could be provided for, the cost in complexity
of implementation and maintenance would be prohibitive.

The value returned by the `local` form is that of the last form in
BODY. Note that definitions have return values in `local' just like
they do at the top level. For example:

     (local
       (plus 2 2)
       (defun plus (x y)
         (+ x y)))

Returns `plus', not 4.

The `local' macro is loosely based on Racket's support for internal
definitions."
  (multiple-value-bind (body decls) (parse-body orig-body)
    (let (vars hoisted-vars labels *in-let* *orig-form* exprs *symbol-macros*)
      (labels ((in-subenv? ()
                 "Are we within a binding form?"
                 (or *in-let* *symbol-macros*))
               (at-beginning? ()
                 "Return non-nil if this is the first form in the `local'."
                 (not (or vars hoisted-vars labels (in-subenv?))))
               (check-beginning (offender)
                 (unless (at-beginning?)
                   (error "Macro definitions in `local' must precede other expressions.~%Offender: ~s" offender)))
               (expand-top (forms)
                 (with-collector (seen)
                   (loop (unless forms
                           (return))
                         (let ((*orig-form* (pop forms)))
                           ;; NB This `restart-case' has no associated
                           ;; signals or handlers: we're using the
                           ;; continuations directly.
                           (restart-case
                               (seen (expand-partially *orig-form*))
                             ;; This is used to (recursively) flatten
                             ;; progns into top-level forms.
                             (splice (spliced-forms)
                               (setf forms (append spliced-forms forms)))
                             ;; This is used to move a macro
                             ;; definition from inside the `local`
                             ;; form to around it. Obviously it only
                             ;; works when the macro precedes other
                             ;; body forms.
                             (eject-macro (name wrapper)
                               (check-beginning name)
                               (let ((body
                                       (append (seen)
                                               forms)))
                                 (return-from local
                                   (append wrapper (list `(local ,@body)))))))))))
               (expand-body (body)
                 "Shorthand for recursing on an implicit `progn'."
                 `(progn ,@(mapcar #'expand-partially body)))
               (symbol-macro (name exp)
                 (push (list name exp) *symbol-macros*))
               (shadow-symbol-macro (name)
                 (removef *symbol-macros* name :key #'car))
               (expansion-done (form)
                 (setf form (wrap-symbol-macros form))
                 (push form exprs)
                 form)
               (expand-in-env-1 (form &optional env)
                 "Like macroexpand-1, but handle local symbol macro bindings."
                 (if (symbolp form)
                     (let ((exp (assoc form *symbol-macros*)))
                       (if exp
                           (progn
                             (when (eql exp form)
                               (error "Recursive symbol macro: ~a" form))
                             (values (second exp) t))
                           (macroexpand-1 form env)))
                     (macroexpand-1 form env)))
               (expand-in-env (form &optional env)
                 (let (exps? exp?)
                   (loop (setf (values form exp?) (expand-in-env-1 form env)
                               exps? (or exps? exp?))
                         (unless exp?
                           (return (values form exps?))))))
               (wrap-symbol-macros (form)
                 (if (null *symbol-macros*) form
                     `(symbol-macrolet ,*symbol-macros*
                        ,form)))
               (step-expansion (form)
                 (multiple-value-bind (exp exp?)
                     (expand-in-env-1 form env)
                   (if exp?
                       ;; Try to make sure that, if the
                       ;; expansion bottoms out, we return the
                       ;; original form instead of the expanded
                       ;; one. It makes no semantic difference,
                       ;; but it does make the expansion easier
                       ;; to read.
                       (let ((next-exp (expand-partially exp)))
                         (if (eq exp next-exp)
                             (expansion-done form)
                             exp))
                       (expansion-done form))))
               (expand-partially (form)
                 "Macro-expand FORM until it becomes a definition form or macro expansion stops."
                 (if (atom form) (step-expansion form)
                     (destructuring-case form
                       ((nonlocal &body _) (declare (ignore _))
                        (expansion-done form))
                       ((defmacro name args &body body)
                        (invoke-restart 'eject-macro
                                        name
                                        `(macrolet ((,name ,args ,@body)))))
                       ;; Surprisingly, define-symbol-macro does not take documentation.
                       ((define-symbol-macro sym exp)
                        (symbol-macro sym exp)
                        `',sym)
                       ((declaim &rest specs)
                        (dolist (spec specs)
                          (push `(declare ,spec) decls)))
                       ((def var &optional expr docstring)
                        (declare (ignore docstring))
                        (if (listp var)
                            ;; That is, (def (values ...) ...).
                            (expand-partially (expand-in-env form env))
                            ;; Remember `def' returns a symbol.
                            (progn
                              (shadow-symbol-macro var)
                              (let ((expr (expand-in-env expr env)))
                                (if (and
                                     (or (constantp expr)
                                         ;; Don't hoist if it could be
                                         ;; altered by a macro or
                                         ;; symbol-macro, or if it's in a
                                         ;; lexical env.
                                         (and (not (in-subenv?))
                                              (constantp expr env)))
                                     ;;Don't hoist if null.
                                     (not (null expr))
                                     ;; Don't hoist unless this is the first
                                     ;; binding for this var.
                                     (not (member var vars)))
                                    (progn
                                      (push (list var expr) hoisted-vars)
                                      `',var)
                                    (progn
                                      ;; Don't duplicate the binding.
                                      (unless (member var hoisted-vars :key #'first)
                                        (pushnew var vars))
                                      `(progn (setf ,var ,expr) ',var)))))))
                       ((defconstant name expr &optional docstring)
                        (declare (ignore docstring))
                        (shadow-symbol-macro name)
                        (let ((expanded (expand-in-env expr env)))
                          (if (and (not (in-subenv?)) (constantp expanded))
                              (expand-partially
                               `(define-symbol-macro ,name ,expr))
                              (push (list name `(load-time-value ,expr t)) hoisted-vars)))
                        `',name)
                       ((defconst name expr &optional docstring)
                        (expand-partially `(defconstant ,name ,expr ,docstring)))
                       ((defun name args &body body)
                        (if *in-let*
                            (expand-partially
                             `(defalias ,name
                                (named-lambda ,name ,args
                                  ,@body)))
                            (progn
                              (push `(,name ,args ,@body) labels)
                              ;; `defun' returns a symbol.
                              `',name)))
                       ((defalias name expr &optional docstring)
                        (declare (ignore docstring))
                        (let ((temp (string-gensym 'fn)))
                          (push `(,temp #'identity) hoisted-vars)
                          (push `(declare (type function ,temp)) decls)
                          (push `(,name (&rest args) (apply ,temp args)) labels)
                          `(progn (setf ,temp (ensure-function ,expr)) ',name)))
                       ((progn &body body)
                        (if (single body)
                            (expand-partially (first body))
                            (if *in-let*
                                `(progn ,@(mapcar #'expand-partially body))
                                (invoke-restart 'splice body))))
                       (((prog1 multiple-value-prog1) f &body body)
                        `(,(car form) ,(expand-partially f)
                          ,(expand-body body)))
                       ((prog2 f1 f2 &body body)
                        `(prog2 ,(expand-partially f1)
                             ,(expand-partially f2)
                           ,(expand-body body)))
                       ((eval-when situations &body body)
                        (if (member :execute situations)
                            (expand-body body)
                            nil))
                       (((let let* flet labels)
                         bindings &body body)
                        (let ((*in-let* t))
                          (multiple-value-bind (body decls) (parse-body body)
                            `(,(car form) ,bindings
                              ,@decls
                              ,(expand-body body)))))
                       (((multiple-value-bind destructuring-bind progv)
                         vars expr &body body)
                        (let ((*in-let* t))
                          (multiple-value-bind (body decls) (parse-body body)
                            `(,(car form) ,vars ,expr
                              ,@decls
                              ,(expand-body body)))))
                       ((symbol-macrolet binds &body body)
                        (let ((*symbol-macros*
                                (append binds *symbol-macros*)))
                          (multiple-value-bind (body decls) (parse-body body)
                            `(locally ,@decls
                               ,(expand-body body)))))
                       ((locally &body body)
                        (multiple-value-bind (body decls) (parse-body body)
                          `(locally ,@decls
                             ,(expand-body body))))
                       ((block name &body body)
                        `(block ,name ,(expand-body body)))
                       ((otherwise &rest rest) (declare (ignore rest))
                        (step-expansion form))))))
        (let* ((body (expand-top body))
               (fn-names (mapcar (lambda (x) `(function ,(car x))) labels))
               (var-names (append (mapcar #'car hoisted-vars) vars)))
          (when (null exprs)
            (simple-style-warning "No expressions in `local' form"))
          (multiple-value-bind (var-decls decls)
              (partition-declarations var-names decls)
            (multiple-value-bind (fn-decls decls)
                (partition-declarations fn-names decls)
              ;; These functions aren't necessary, but they
              ;; make the expansion cleaner.
              (labels ((wrap-decls (body)
                         (if decls
                             `(locally ,@decls
                                ,@body)
                             `(progn ,@body)))
                       (wrap-vars (body)
                         (if (or hoisted-vars vars)
                             ;; As an optimization, hoist constant
                             ;; bindings, e.g. (def x 1), so the
                             ;; compiler can infer their types or
                             ;; make use of declarations. (Ideally we
                             ;; would hoist anything we know for sure
                             ;; is not a closure, but that's
                             ;; complicated.)
                             `((let-initialized (,@hoisted-vars
                                                 ,@vars)
                                 ,@var-decls
                                 ,@body))
                             body))
                       (wrap-labels (body)
                         (if labels
                             `((labels ,labels
                                 ,@fn-decls
                                 ,@body))
                             body)))
                (wrap-decls
                 (wrap-vars
                  (wrap-labels
                   body)))))))))))
