(in-package :serapeum)

(export '(defconst
          defsubst
          defalias
          def
          defplace
          defcondition))

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
