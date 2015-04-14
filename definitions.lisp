(in-package :serapeum)

(export '(defconst
          defsubst
          defalias
          def
          defplace
          defcondition
          local))

;;;# Lexical globals

;;; `def' and `defconst' are both applications of the same idea: using
;;; symbol macros to get lexical behavior from global variables (via
;;; `defparameter' and `defconstant', respectively).

(defmacro def (var &body (val &optional (doc nil docp)))
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

The original `deflex' is due to Rob Warnock."
  (let* ((s0 (symbol-name '#:*storage-for-deflex-var-))
         (s1 (symbol-name var))
         (s2 (symbol-name '#:*))
         (s3 (symbol-package var))	; BUGFIX [see above]
         (backing-var (intern (concatenate 'string s0 s1 s2) s3)))
    ;; Note: The DEFINE-SYMBOL-MACRO must precede VAL so VAL can close
    ;; over VAR.
    #+sbcl
    `(progn
       (define-symbol-macro ,var ,backing-var)
       (sb-ext:defglobal ,backing-var nil ,@(unsplice doc))
       (setq ,backing-var ,val)
       ,@(unsplice (when docp `(setf (documentation ',var 'variable) ,doc)))
       ',var)
    #+ccl
    `(progn
       (define-symbol-macro ,var ,backing-var)
       (ccl:defstatic ,backing-var nil ,@(unsplice doc))
       (setq ,backing-var ,val)
       ,@(unsplice (when docp `(setf (documentation ',var 'variable) ,doc)))
       ',var)
    #-(or sbcl ccl)
    `(progn
       (define-symbol-macro ,var ,backing-var)
       (defvar ,backing-var nil ,doc)
       (setq ,backing-var ,val)
       ,@(when docp
           (unsplice `(setf (documentation ',backing-var 'variable) ,doc
                            (documentation ',var 'variable) ,doc)))
       ',var)))

(defun same-literal-p (x y)
  "A predicate that compares whether X and Y have the same literal
  representation."
  (or (equal x y)
      ;; Crude, but reliable.
      (string= (prin1-to-string x) (prin1-to-string y))))

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
     ;; the compiler doesn't complain about it's being undefined.
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

(defmacro local (&body body &environment env)
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
- `defun', for local functions (as with `labels')
- `defmacro', for local macros (as with `defmacro')
- `defalias', to bind values in the function namespace (like `fbindrec*')
- `declaim', to make declarations (as with `declare')
- `define-symbol-macro', to bind symbol macros (as with `symbol-macrolet')
- `defconstant' and `defconst', which behave exactly like symbol macros

\(Note that the top-level definition forms defined by Common Lisp
are (necessarily) supplemented by two from Serapeum: `def' and
`defalias'.)

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

Note that `let' forms are *not* descended into, so you cannot use the
top-level idiom of wrapping `let' around `defun'.

The value returned by the `local` form is that of the last form in
BODY. Note that definitions have return values in `local' just like
they do at the top level. For example:

     (local
       (plus 2 2)
       (defun plus (x y)
         (+ x y)))

Returns `plus', not 4.

The `local' macro is based on Racket's support for internal
definitions (although not Racket's `local' macro, which does
something different)."
  ;; TODO Think about how to (whether to) handle dynamic vars?
  (multiple-value-bind (body decls) (parse-body body)
    (let (vars hoisted-vars labels macros symbol-macros)
      (labels ((expand-partially (form)
                 (if (consp form)
                     (destructuring-case form
                       ((declaim &rest specs)
                        (dolist (spec specs)
                          (push `(declare ,spec) decls)))
                       ((def var expr &optional docstring)
                        (declare (ignore docstring))
                        ;; Remember `def' returns a symbol.
                        (if (and (not (null expr)) ;Don't hoist if null.
                                 (constantp expr env))
                            (progn
                              (push (list var expr) hoisted-vars)
                              `',var)
                            (progn
                              (pushnew var vars)
                              `(progn (setf ,var ,expr) ',var))))
                       ((defconstant name expr &optional docstring)
                        (declare (ignore docstring))
                        (push (list name expr) symbol-macros)
                        `',name)
                       ((defconst name expr &optional docstring)
                        (declare (ignore docstring))
                        (push (list name expr) symbol-macros)
                        `',name)
                       ((define-symbol-macro sym exp)
                        (push (list sym exp) symbol-macros)
                        `',sym)
                       ((defun name args &body body)
                        (push (list* name args body) labels)
                        ;; `defun' returns a symbol.
                        `',name)
                       ((defalias name expr &optional docstring)
                        (declare (ignore docstring))
                        (let ((temp (gensym)))
                          (push temp vars)
                          (push `(,name (&rest args) (apply ,temp args)) labels)
                          `(progn (setf ,temp ,expr) ',name)))
                       ((defmacro name args &body body)
                        (push (list* name args body) macros)
                        ;; `defmacro' returns a symbol.
                        `',name)
                       ((progn &body body)
                        `(progn ,@(mapcar #'expand-partially body)))
                       ((eval-when situations &body body)
                        (if (set-equal situations '(:load-toplevel :execute))
                            (expand-partially `(progn ,@body))
                            (error "Unsupported eval-when: ~a" situations)))
                       ((otherwise &rest rest) (declare (ignore rest))
                        (multiple-value-bind (exp exp?)
                            (macroexpand-1 form env)
                          (if exp?
                              (expand-partially exp)
                              form))))
                     form)))
        (let ((body (mapcar #'expand-partially body)))
          (cond ((not (or vars labels macros symbol-macros))
                 `(locally ,@decls ,@body))
                ((no body)
                 (error "Internal definitions are present, but there are ~
              no expressions.~%Definitions: ~s"
                        (append (mapcar #'second vars)
                                (mapcar #'second labels)
                                (mapcar #'second macros))))
                (t
                 (let* ((fn-names (mapcar (lambda (x) `(function ,(car x))) labels))
                        (var-names (append (mapcar #'car hoisted-vars) vars)))
                   (multiple-value-bind (var-decls decls)
                       (partition-declarations var-names decls)
                     (multiple-value-bind (fn-decls decls)
                         (partition-declarations fn-names decls)
                       `(locally ,@decls
                          (symbol-macrolet ,symbol-macros
                            (macrolet ,macros
                              ;; As an optimization, hoist constant
                              ;; bindings, e.g. (def x 1), so the compiler
                              ;; can infer their types or make use of
                              ;; declaration.
                              (let (,@hoisted-vars
                                    ,@vars)
                                ,@var-decls
                                (labels ,labels
                                  ,@fn-decls
                                  ,@body)))))))))))))))
