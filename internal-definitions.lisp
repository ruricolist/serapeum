(defpackage :serapeum/internal-definitions
  (:use :cl :alexandria :serapeum)
  (:import-from :serapeum :unbound)
  (:import-from :trivia :match :ematch)
  #+sb-package-locks (:implement :serapeum :serapeum/internal-definitions))

(in-package :serapeum/internal-definitions)

(defmacro without-internal-definitions (&body body)
  `(progn ,@body))

(defmacro local* (&body body)
  "Like `local', but leave the last form in BODY intact.

     (local*
       (defun aux-fn ...)
       (defun entry-point ...))
     =>
     (labels ((aux-fn ...))
       (defun entry-point ...))"
  `(local
     ,@(butlast body)
     (without-internal-definitions ,@(last body))))

(defun initialize-binds-from-decls (binds-in decls &optional env)
  "Given a list of bindings and a set of declarations, look for any
uninitialized bindings. If there are any, try to extrapolate
reasonable initialization values for them based on the declarations in
DECLS."
  (labels ((var-decls-type (var decls)
             (let ((bind-decls (partition-declarations (list var) decls)))
               ;; If we can't find a declaration, default to `t`.
               (dolist (decl bind-decls t)
                 (match decl
                   ((list 'declare (list 'type typespec (and _ (eql var))))
                    (return typespec))
                   ((list 'declare (list typespec (and _ (eql var))))
                    (return typespec))))))
           ;; Curiously, alexandria:type= doesn't take an environment arg.
           (type=? (x y)
             (and (subtypep x y env)
                  (subtypep y x env)))
           (initialization-value (var decls)
             (let ((type (var-decls-type var decls)))
               (cond
                 ((type=? type 't)
                  ;; Generate a placeholder, saving the name of the
                  ;; var for debugging purposes. Note that the
                  ;; placeholder is inserted directly into the code,
                  ;; so there is no run-time consing.
                  (unbound var))
                 ((type=? type 'boolean) nil)

                 ;; Why all this attention to numeric types? One of
                 ;; the things internal definitions are useful for is
                 ;; porting numeric code from languages like C or
                 ;; Fortran. By using appropriate initialization
                 ;; values we help SBCL take advantage of those
                 ;; declarations.
                 ((or (type=? type 'bit)
                      (type=? type 'fixnum)
                      (type=? type 'array-index)
                      (type=? type 'array-length)
                      (subtypep type 'unsigned-byte env)
                      (subtypep type 'signed-byte env))
                  0)
                 ((type=? type 'single-float) 0f0)
                 ((type=? type 'double-float) 0d0)
                 ((type=? type 'float) 0.0)
                 ((type=? type '(complex integer)) #C(0 0))
                 ((type=? type '(complex single-float)) #C(0f0 0f0))
                 ((type=? type '(complex double-float)) #C(0d0 0d0))
                 ((type=? type '(complex float)) #C(0.0 0.0))

                 ((type=? type 'string) "")
                 ((type=? type 'pathname) 'uiop:*nil-pathname*)
                 ((type=? type 'function) '#'identity)
                 ((type=? type 'standard-char) #\a)
                 ((type=? type 'character) (code-char 0)))))
           (binds-out (binds-in decls binds-out)
             (if (endp binds-in)
                 (nreverse binds-out)
                 (destructuring-bind (var . binds-in) binds-in
                   (if (listp var)
                       ;; Already initialized.
                       (binds-out binds-in
                                  decls
                                  (cons var binds-out))
                       (let* ((init (initialization-value var decls))
                              (bind `(,var ,init)))
                         (binds-out binds-in
                                    decls
                                    (cons bind binds-out))))))))
    ;; Shortcut: no declarations, just return the bindings.
    (binds-out binds-in decls '())))

(defmacro let-initialized (bindings &body body &environment env)
  "Like LET, but if any of BINDINGS are uninitialized, try to give
them sane initialization values."
  (multiple-value-bind (body decls) (parse-body body)
    `(let ,(initialize-binds-from-decls bindings decls env)
       ,@decls
       ,@body)))

(locally
    ;; Enforce types.
    (declare (optimize safety))
  (defclass internal-definitions-env ()
    ((vars :type list :initform nil)
     (var-aliases :type list :initform nil)
     (decls :type list :initarg :decls)
     (hoisted-vars :type list :initform nil)
     (labels :type list :initform nil)
     (exprs :type list :initform nil)
     (global-symbol-macros :type list :initform nil)
     (env :type t :initarg :env
          :documentation "The Lisp environment."))
    (:default-initargs
     :env nil
     :decls nil)))

(deftype expr () t)
(deftype body () 'list)

(locally (declare (optimize safety))
  (defclass %binding ()
    ((name :reader binding-name
           :initarg :name
           :type symbol)
     (init :reader binding-init
           :initarg :init
           :type expr)))

  (defclass var (%binding) ())
  (defclass symbol-macro (%binding) ())
  (defclass fun (%binding) ())
  (defclass macro (%binding) ()))

(macrolet ((binding-constructor (class)
             `(defun ,class (name  init)
                (make ',class :name name :init init))))
  (binding-constructor var)
  (binding-constructor symbol-macro)
  (binding-constructor fun)
  (binding-constructor macro))

(deftype binding ()
  '(or var symbol-macro fun macro))

(defun expand-binding (bind)
  (if (listp bind)
      (if (rest bind)
          bind
          (append bind '(nil)))
      (list bind nil)))

(defun expand-bindings (bindings)
  (mapcar #'expand-binding bindings))

(defconstructor subenv
  "Minimal lexical environment."
  (vars list)
  (funs list)
  (blocks list)
  (tags list))

(defvar *subenv*
  (subenv nil nil nil nil))

(defun subenv-empty? (&aux (s *subenv*))
  (nor (subenv-vars s)
       (subenv-funs s)
       (subenv-blocks s)
       (subenv-tags s)))

(defun augment/vars (binds &optional (subenv *subenv*))
  (let ((vars
          (~>> binds
               (mapcar #'expand-binding)
               (mapply #'var))))
    (copy-subenv subenv
                 :vars (append vars (subenv-vars subenv)))))

(defun augment/symbol-macros (symbol-macros &optional (subenv *subenv*))
  (copy-subenv subenv
               :vars (append (mapply #'symbol-macro symbol-macros)
                             (subenv-vars subenv))))

(defun augment/block (block &optional (subenv *subenv*))
  (copy-subenv subenv
               :blocks (cons block (subenv-blocks subenv))))

(defun augment/tags (tags &optional (subenv *subenv*))
  (copy-subenv subenv
               :tags (append tags (subenv-tags subenv))))

(defun tagbody-tag? (form)
  (typep form '(or symbol integer)))

(defun extract-tagbody-tags (body)
  (declare (notinline filter))          ;phasing
  ;; It's not documented in CLHS, but SBCL and CCL agree that a tag
  ;; must be a symbol or an integer.
  (filter #'tagbody-tag? body))

(defun augment/funs (funs &optional (subenv *subenv*))
  (copy-subenv subenv
               :funs (append (mapcar (lambda (spec)
                                       (fun (first spec) (rest spec)))
                                     funs)
                             (subenv-funs subenv))))

;;; Of course this isn't used at the moment.
(defun augment/macros (macros &optional (subenv *subenv*))
  (copy-subenv subenv
               :funs (append (mapply #'macro macros)
                             (subenv-funs subenv))))

(defun visible-of-type (type subenv)
  (let* ((vars (subenv-vars subenv))
         (visible (remove-duplicates vars :from-end t :key #'binding-name))
         (of-type (remove-if-not (of-type type) visible)))
    of-type))

(defun symbol-macro-bindings (&optional (subenv *subenv*))
  (mapcar (juxt #'binding-name #'binding-init)
          (visible-of-type 'symbol-macro subenv)))

(defun remove-shadowed (binds &optional (subenv *subenv*))
  (remove-if (op (member (ensure-car _)
                         (subenv-vars subenv)
                         :key #'binding-name))
             binds))

(defun shadow-names (binds)
  (remove-duplicates binds :from-end t :key #'ensure-car))

(deftype internal-definition-form ()
  '(member
    without-internal-definitions
    defmacro define-symbol-macro
    declaim
    def
    defconstant defconst
    defun defalias
    progn prog1 multiple-value-prog1 prog2 eval-when locally
    block
    progv
    flet labels
    let let*
    multiple-value-bind
    destructuring-bind
    symbol-macrolet))

(eval-always
  (defparameter *internal-definitions-slots*
    '(self vars decls hoisted-vars var-aliases labels exprs global-symbol-macros env)))

(defmacro define-env-method (name (self &rest args) &body body)
  `(symbol-macrolet ,(loop for slot in *internal-definitions-slots*
                           collect `(,slot (slot-value ,self ',slot)))
     (defmethod ,name ((,self internal-definitions-env) ,@args)
       ,@body)))

(define-env-method ensure-var-alias (self var)
  (if-let (match (assoc var var-aliases))
    (cdr match)
    (let ((alias (gensym (string var))))
      (push (cons var alias) var-aliases)
      alias)))

(define-env-method var-alias-bindings (self)
  (mapcar (juxt #'car #'cdr) var-aliases))

(define-env-method alias-decls (self decls)
  ;; XXX Is this reliable?
  (sublis var-aliases decls))

(define-env-method hoisted-var? (self var)
  (member var hoisted-vars :key #'first))

(define-env-method known-var? (self var)
  (or (member var vars)
      (hoisted-var? self var)))

(define-env-method in-subenv? (self)
  "Are we within a binding form?"
  (or (not (subenv-empty?))
      global-symbol-macros))

(define-env-method at-beginning? (self)
  "Return non-nil if this is the first form in the `local'."
  (not (or exprs vars hoisted-vars labels (in-subenv? self))))

(define-env-method check-beginning (self offender)
  (unless (at-beginning? self)
    (error "Macro definitions in `local' must precede other expressions.~%Offender: ~s" offender)))

(define-env-method expand-top (self forms)
  (with-collector (seen)
    (loop (unless forms
            (return))
          (let ((orig-form (pop forms)))
            ;; NB This `restart-case' has no associated
            ;; signals or handlers: we're using the
            ;; continuations directly.
            (restart-case
                (seen (expand-partially self orig-form))
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

(define-env-method splice-forms (self spliced-forms)
  (invoke-restart 'splice spliced-forms))

(define-env-method eject-macro (self name wrapper)
  (invoke-restart 'eject-macro name wrapper))

(define-env-method expand-body (self body)
  "Shorthand for recursing on an implicit `progn'."
  `(progn ,@(mapcar (op (expand-partially self _)) body)))

(define-env-method save-symbol-macro (self name exp)
  (push (list name exp) global-symbol-macros))

(define-env-method shadow-symbol-macro (self name)
  ;; Note that this removes /all/ instances.
  (removef global-symbol-macros name :key #'car))

(define-env-method expansion-done (self form)
  (setf form (wrap-expr self form))
  (push form exprs)
  form)

(define-env-method expand-in-env-1 (self form &optional env)
  "Like macroexpand-1, but handle local symbol macro bindings."
  (if (symbolp form)
      (let ((exp (or (assoc form (symbol-macro-bindings *subenv*))
                     (assoc form (remove-shadowed global-symbol-macros *subenv*)))))
        (if exp
            (progn
              (when (eq (second exp) form)
                (error "Recursive symbol macro: ~a" form))
              (values (second exp) t))
            (macroexpand-1 form env)))
      (macroexpand-1 form env)))

(define-env-method expand-in-env (self form &optional env)
  (nlet rec ((form form)
             (exps? nil))
    (mvlet* ((form exp? (expand-in-env-1 self form env))
             (exps? (or exps? exp?)))
      (if (not exp?)
          (values form exps?)
          (rec form exps?)))))

(define-env-method wrap-expr (self form)
  (let ((symbol-macros
          (append (symbol-macro-bindings *subenv*)
                  (remove-shadowed global-symbol-macros *subenv*))))
    (if (null symbol-macros) form
        `(symbol-macrolet ,symbol-macros
           ,form))))

(define-env-method wrap-bindings (self bindings)
  (loop for (var expr) in (expand-bindings bindings)
        if (constantp expr)
          collect `(,var ,expr)
        else collect `(,var ,(wrap-expr self expr))))

(define-env-method wrap-fn-bindings (self bindings)
  (loop for (var args . body) in bindings
        collect (multiple-value-bind (body decls docstring)
                    (parse-body body :documentation t)
                  `(,var ,args
                         ,@decls
                         ,docstring
                         ,(wrap-expr
                           self
                           `(progn
                              ,@body))))))

(define-env-method step-expansion (self form)
  (receive (exp exp?) (expand-in-env-1 self form env)
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
              (expansion-done self next-exp)))
        (expansion-done self form))))

(define-env-method expand-partially (self form)
  "Macro-expand FORM until it becomes a definition form or macro expansion stops."
  (declare (notinline single))
  (when (atom form)
    (return-from expand-partially
      (step-expansion self form)))
  ;; Don't partially expand setq or setf.
  (when (member (car form) '(setq setf))
    (return-from expand-partially
      form))
  (destructuring-case-of internal-definition-form form
    ;; A specific form to stop expansion.
    ((without-internal-definitions &body _) (declare (ignore _))
     (expansion-done self form))

    ;; DEFINITION FORMS.
    ((defmacro name args &body body)
     (when (typep name 'internal-definition-form)
       ;; Cf. R7RS: " it is an error for a definition to define
       ;; an identifier whose binding has to be known in order
       ;; to determine the meaning of the definition itself, or
       ;; of any preceding definition that belongs to the same
       ;; group of internal definitions."
       (error "Cannot shadow ~a in an internal definition." name))
     (eject-macro self name
                  `(macrolet ((,name ,args ,@body)))))

    ;; NB `define-symbol-macro' does not take documentation.
    ((define-symbol-macro sym exp)
     (if (at-beginning? self)
         ;; Might as well eject the symbol macro if we're at the
         ;; beginning, to keep things simple.
         (eject-macro self sym
                      `(symbol-macrolet ((,sym ,exp))))
         (progn
           (save-symbol-macro self sym exp)
           `',sym)))

    ((declaim &rest specs)
     (dolist (spec specs)
       (push `(declare ,spec) decls)))

    ((def var &optional expr docstring)
     (declare (ignore docstring))
     (if (listp var)
         ;; That is, (def (values ...) ...).
         (expand-partially self (expand-in-env self form env))
         ;; Remember `def' returns a symbol.
         (progn
           (shadow-symbol-macro self var)
           (let* ((expr (expand-in-env self expr env))
                  (var (if (in-subenv? self)
                           (ensure-var-alias self var)
                           var))
                  (hoistable?
                    (and
                     (or (constantp expr)
                         ;; Don't hoist if it could be altered
                         ;; by a macro or symbol-macro, or if
                         ;; it's in a lexical env.
                         (and (not (in-subenv? self))
                              (constantp expr env)))
                     ;;Don't hoist if null.
                     (not (null expr))
                     ;; Don't hoist unless this is the first
                     ;; binding for this var.
                     (not (or (member var vars)
                              (member var hoisted-vars :key #'first)))))
                  (expr (wrap-expr self expr)))
             (if hoistable?
                 (progn
                   (push (list var expr) hoisted-vars)
                   ;; This is needed in case the var ends up
                   ;; being aliased. Hoisting vars isn't about
                   ;; saving setfs, it's about type inference.
                   `(progn (setf ,var ,expr) ',var))
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
           (push (list name `(static-load-time-value ,(wrap-expr self expr) t)) hoisted-vars)))
     `',name)

    ((defconst name expr &optional docstring)
     (expand-partially self `(defconstant ,name ,expr ,docstring)))

    ((defun name args &body body)
     (if (not (subenv-empty?))
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
     (let ((temp (string-gensym 'fn))
           (expr (wrap-expr self expr)))
       (push `(,temp #'identity) hoisted-vars)
       (push `(declare (type function ,temp)) decls)
       ;; In case of redefinition.
       (push `(declare (ignorable ,temp)) decls)
       (push `(,name (&rest args) (apply ,temp args)) labels)
       `(progn (setf ,temp (ensure-function ,expr)) ',name)))

    ;; SEQUENCING.
    ((progn &body body)
     (if (single body)
         (expand-partially self (first body))
         (if (not (subenv-empty?))
             `(progn ,@(mapcar (op (expand-partially self _)) body))
             (splice-forms self body))))

    ((prog1 f &body body)
     (let ((form
             (if (constantp f)
                 `(progn
                    ,@body
                    ,f)
                 (with-unique-names (temp)
                   `(let ((,temp ,f))
                      ,@body
                      ,temp)))))
       (expand-partially self form)))

    ((multiple-value-prog1 f &body body)
     (if (constantp f)
         `(progn
            ,@body
            ,f)
         (with-unique-names (temp)
           `(let ((,temp (multiple-value-list ,f)))
              ,@body
              (values-list ,temp)))))

    ((prog2 first second &body body)
     `(progn ,first
             (prog1 ,second
               ,@body)))

    ((eval-when situations &body body)
     (if (member :execute situations)
         (expand-body self body)
         nil))

    ((locally &body body)
     (multiple-value-bind (body decls) (parse-body body)
       `(locally ,@decls
          ,(expand-body self body))))

    ((block name &body body)
     (let ((*subenv* (augment/block name)))
       `(block ,name ,(expand-body self body))))

    ((progv vars expr &body body)
     ;; Is this really the right way to handle progv? Should we
     ;; bother?
     (multiple-value-bind (body decls) (parse-body body)
       `(,(car form) ,vars ,(wrap-expr self expr)
         ,@decls
         ,(expand-body self body))))

    ;; FUNCTION BINDING FORMS.
    (((flet labels)
      bindings &body body)
     (let ((*subenv* (augment/funs bindings)))
       (multiple-value-bind (body decls) (parse-body body)
         `(,(car form) ,(wrap-fn-bindings self bindings)
           ,@decls
           ,(expand-body self body)))))

    ;; VARIABLE BINDING FORMS.
    ((let bindings &body body)
     ;; NB Expand the bindings before you augment the env.
     (let* ((bindings (wrap-bindings self bindings))
            (*subenv* (augment/vars bindings)))
       (multiple-value-bind (body decls) (parse-body body)
         `(,(car form) ,bindings
           ,@decls
           ,(expand-body self body)))))

    ((let* bindings &body body)
     ;; NB Augment the env before you wrap the bindings.
     (let* ((*subenv* (augment/vars bindings))
            (bindings (wrap-bindings self bindings)))
       (multiple-value-bind (body decls) (parse-body body)
         `(,(car form) ,bindings
           ,@decls
           ,(expand-body self body)))))

    ((multiple-value-bind vars expr &body body)
     (let ((*subenv* (augment/vars vars)))
       (multiple-value-bind (body decls) (parse-body body)
         `(,(car form) ,vars ,(wrap-expr self expr)
           ,@decls
           ,(expand-body self body)))))

    ;; Don't even try to handle destructuring-bind ourselves.
    ((destructuring-bind vars expr &body body)
     (declare (ignore vars expr body))
     (expand-partially self (macroexpand form env)))

    ((symbol-macrolet binds &body body)
     (multiple-value-bind (body decls) (parse-body body)
       `(locally ,@decls
          ,(let ((*subenv* (augment/symbol-macros binds)))
             (expand-body self body)))))

    ;; Fallthrough.
    ((otherwise &rest rest) (declare (ignore rest))
     (step-expansion self form))))

(define-env-method generate-internal-definitions (self body)
  (let* ((body (expand-top self body))
         (fn-names (mapcar (lambda (x) `(function ,(car x))) labels))
         (var-names (append (mapcar #'first hoisted-vars) vars))
         (aliased-vars (mapcar #'first var-aliases))
         ;; XXX This is needed by ABCL 1.5.0. If I can work up a
         ;; minimal example it should be reported as a bug.
         #+abcl (decls decls))
    (when (null exprs)
      (simple-style-warning "No expressions in `local' form"))
    (mvlet* ((var-decls decls     (partition-declarations var-names decls))
             (fn-decls decls      (partition-declarations fn-names decls))
             (aliased-decls decls (partition-declarations aliased-vars decls)))
      ;; These functions aren't necessary, but they
      ;; make the expansion cleaner.
      (labels ((wrap-decls (body)
                 (expect-single-form
                  (if decls
                      `(locally ,@decls
                         ,@body)
                      `(progn ,@body))))
               (wrap-vars (body)
                 (expect-form-list
                  (if (or hoisted-vars vars)
                      ;; As an optimization, hoist constant bindings,
                      ;; e.g. (def x 1), so the compiler can infer
                      ;; their types or make use of declarations.
                      ;; (Ideally we would hoist anything we know for
                      ;; sure is not a closure, but that's
                      ;; impractical.) NB Since they're constant, we
                      ;; don't need to reverse them before splicing,
                      ;; but it makes the expansion easier to read.
                      `((let-initialized (,@(reverse hoisted-vars)
                                          ,@vars)
                          ,@var-decls
                          ;; Un-alias the vars.
                          (symbol-macrolet ,(var-alias-bindings self)
                            ,@aliased-decls
                            ,@body)))
                      body)))
               (wrap-labels (body)
                 (expect-form-list
                  (if labels
                      `((labels ,(shadow-names labels)
                          ,@fn-decls
                          ,@body))
                      body))))
        (wrap-decls
         (wrap-vars
          (wrap-labels
           body)))))))

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
- `define-values', for multiple lexical variables at once
- `defun', for local functions (as with `labels')
- `defalias', to bind values in the function namespace (like `fbindrec*')
- `declaim', to make declarations (as with `declare')
- `defconstant' and `defconst', which behave exactly like symbol macros
- `define-symbol-macro', to bind symbol macros (as with `symbol-macrolet')

Also, with serious restrictions, you can use:

- `defmacro', for local macros (as with `macrolet')

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

Support for macros is sharply limited. (Symbol macros, on the other
hand, are completely supported.)

1. Macros defined with `defmacro' must precede all other expressions.

2. Macros cannot be defined inside of binding forms like `let'.

3. `macrolet' is not allowed at the top level of a `local' form.

These restrictions are undesirable, but well justified: it is
impossible to handle the general case both correctly and portably, and
while some special cases could be provided for, the cost in complexity
of implementation and maintenance would be prohibitive.

The value returned by the `local' form is that of the last form in
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
    (catch 'local
      (generate-internal-definitions
       (make 'internal-definitions-env
             :decls decls
             :env env)
       body))))

(defmacro block-compile ((&key entry-points
                               (block-compile t))
                         &body body)
  "Shorthand for block compilation with `local*'.

Only the functions in ENTRY-POINTS will have global definitions. All
other functions in BODY will be compiled as purely local functions,
and all of their calls to one another will be compiled as local calls.
This includes calls to the entry points, and even self-calls from
within the entry points.

Note that `declaim' forms occuring inside of BODY will be translated
into local `declare' forms.

If you pass `:block-compile nil', this macro is equivalent to progn.
This may be useful during development."
  (if (not block-compile) `(progn ,@body)
      `(local*
         ,@body
         (progn
           ,@(loop for entry-point in entry-points
                   collect `(defalias ,entry-point #',entry-point))))))
