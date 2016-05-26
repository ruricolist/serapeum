(defpackage :serapeum/internal-definitions
  (:use :cl :alexandria :serapeum :optima)
  #+sb-package-locks (:implement :serapeum :serapeum/internal-definitions))

(in-package :serapeum/internal-definitions)

(defvar *in-let*)
(defvar *orig-form*)
(defvar *local-symbol-macros*)

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

(defclass internal-definitions-env ()
  ((vars :initform nil)
   (decls :initarg :decls)
   (hoisted-vars :initform nil)
   (labels :initform nil)
   (exprs :initform nil)
   (global-symbol-macros :initform nil)
   (env :initarg :env))
  (:default-initargs
   :env nil
   :decls nil))

(defmethods internal-definitions-env
    (self vars decls hoisted-vars labels exprs global-symbol-macros env)
  (:method in-subenv? (self)
    "Are we within a binding form?"
    (or *in-let* global-symbol-macros *local-symbol-macros*))
  (:method at-beginning? (self)
    "Return non-nil if this is the first form in the `local'."
    (not (or vars hoisted-vars labels (in-subenv? self))))
  (:method check-beginning (self offender)
    (unless (at-beginning? self)
      (error "Macro definitions in `local' must precede other expressions.~%Offender: ~s" offender)))
  (:method expand-top (self forms)
    (with-collector (seen)
      (loop (unless forms
              (return))
            (let ((*orig-form* (pop forms)))
              ;; NB This `restart-case' has no associated
              ;; signals or handlers: we're using the
              ;; continuations directly.
              (restart-case
                  (seen (expand-partially self *orig-form*))
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
                  (check-beginning self name)
                  (let ((body
                          (append (seen)
                                  forms)))
                    (throw 'local
                      (append wrapper (list `(local ,@body)))))))))))
  (:method expand-body (self body)
    "Shorthand for recursing on an implicit `progn'."
    `(progn ,@(mapcar (op (expand-partially self _)) body)))
  (:method save-symbol-macro (self name exp)
    (push (list name exp) global-symbol-macros))
  (:method shadow-symbol-macro (self name)
    ;; Note that this removes /all/ instances.
    (removef global-symbol-macros name :key #'car))
  (:method expansion-done (self form)
    (setf form (wrap-symbol-macros self form))
    (push form exprs)
    form)
  (:method expand-in-env-1 (self form &optional env)
    "Like macroexpand-1, but handle local symbol macro bindings."
    (if (symbolp form)
        (let ((exp (or (assoc form global-symbol-macros)
                       (assoc form *local-symbol-macros*))))
          (if exp
              (progn
                (when (eq (second exp) form)
                  (error "Recursive symbol macro: ~a" form))
                (values (second exp) t))
              (macroexpand-1 form env)))
        (macroexpand-1 form env)))
  (:method expand-in-env (self form &optional env)
    (let (exps? exp?)
      (loop (setf (values form exp?) (expand-in-env-1 self form env)
                  exps? (or exps? exp?))
            (unless exp?
              (return (values form exps?))))))
  (:method wrap-symbol-macros (self form)
    (let ((symbol-macros
            (append *local-symbol-macros* global-symbol-macros)))
      (if (null symbol-macros) form
          `(symbol-macrolet ,symbol-macros
             ,form))))
  (:method step-expansion (self form)
    (multiple-value-bind (exp exp?)
        (expand-in-env-1 self form env)
      (if exp?
          ;; Try to make sure that, if the
          ;; expansion bottoms out, we return the
          ;; original form instead of the expanded
          ;; one. It makes no semantic difference,
          ;; but it does make the expansion easier
          ;; to read.
          (let ((next-exp (expand-partially self exp)))
            (if (eq exp next-exp)
                (expansion-done self form)
                next-exp))
          (expansion-done self form))))
  (:method expand-partially (self form)
    "Macro-expand FORM until it becomes a definition form or macro expansion stops."
    (if (atom form) (step-expansion self form)
        (destructuring-case form
          ((nonlocal &body _) (declare (ignore _))
           (expansion-done self form))
          ((defmacro name args &body body)
           (invoke-restart 'eject-macro
                           name
                           `(macrolet ((,name ,args ,@body)))))
          ;; Surprisingly, define-symbol-macro does not take documentation.
          ((define-symbol-macro sym exp)
           (save-symbol-macro self sym exp)
           `',sym)
          ((declaim &rest specs)
           (dolist (spec specs)
             (push `(declare ,spec) decls)))
          ((def var &optional expr docstring)
           (declare (ignore docstring))
           (if (listp var)
               ;; That is, (def (values ...) ...).
               (expand-partially self (expand-in-env form env))
               ;; Remember `def' returns a symbol.
               (progn
                 (shadow-symbol-macro self var)
                 (let ((expr (expand-in-env self expr env)))
                   (if (and
                        (or (constantp expr)
                            ;; Don't hoist if it could be
                            ;; altered by a macro or
                            ;; symbol-macro, or if it's in a
                            ;; lexical env.
                            (and (not (in-subenv? self))
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
           (shadow-symbol-macro self name)
           (let ((expanded (expand-in-env self expr env)))
             (if (and (not (in-subenv? self)) (constantp expanded))
                 (expand-partially self
                                   `(define-symbol-macro ,name ,expr))
                 (push (list name `(load-time-value ,expr t)) hoisted-vars)))
           `',name)
          ((defconst name expr &optional docstring)
           (expand-partially self `(defconstant ,name ,expr ,docstring)))
          ((defun name args &body body)
           (if *in-let*
               (expand-partially self
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
               (expand-partially self (first body))
               (if *in-let*
                   `(progn ,@(mapcar (op (expand-partially self _)) body))
                   (invoke-restart 'splice body))))
          (((prog1 multiple-value-prog1) f &body body)
           `(,(car form) ,(expand-partially self f)
             ,(expand-body self body)))
          ((prog2 f1 f2 &body body)
           `(prog2 ,(expand-partially self f1)
                ,(expand-partially self f2)
              ,(expand-body self body)))
          ((eval-when situations &body body)
           (if (member :execute situations)
               (expand-body self body)
               nil))
          (((let let* flet labels)
            bindings &body body)
           (let ((*in-let* t))
             (multiple-value-bind (body decls) (parse-body body)
               `(,(car form) ,bindings
                 ,@decls
                 ,(expand-body self body)))))
          (((multiple-value-bind destructuring-bind progv)
            vars expr &body body)
           (let ((*in-let* t))
             (multiple-value-bind (body decls) (parse-body body)
               `(,(car form) ,vars ,expr
                 ,@decls
                 ,(expand-body self body)))))
          ((symbol-macrolet binds &body body)
           (multiple-value-bind (body decls) (parse-body body)
             `(locally ,@decls
                ,(let ((*local-symbol-macros*
                         (append binds *local-symbol-macros*)))
                   (expand-body self body)))))
          ((locally &body body)
           (multiple-value-bind (body decls) (parse-body body)
             `(locally ,@decls
                ,(expand-body self body))))
          ((block name &body body)
           `(block ,name ,(expand-body self body)))
          ((otherwise &rest rest) (declare (ignore rest))
           (step-expansion self form)))))
  (:method generate-internal-definitions (self body)
    (let* ((body (expand-top self body))
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
               body)))))))))

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
  (catch 'local
    (multiple-value-bind (body decls) (parse-body orig-body)
      (let (*in-let* *orig-form* *local-symbol-macros*)
        (generate-internal-definitions
         (make 'internal-definitions-env
               :decls decls
               :env env)
         body)))))
