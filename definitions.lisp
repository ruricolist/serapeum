(defpackage :serapeum/definitions
  (:documentation "Global definition macros")
  #+sb-package-locks (:lock t)
  (:use
   :cl
   :alexandria
   :serapeum/iter
   :serapeum/macro-tools
   :trivia)
  (:import-from
   :serapeum/macro-tools
   :lambda-list-vars
   :let-over-lambda)
  (:export
   #:def
   #:defalias
   #:defconst
   #:define-values
   #:defloop
   #:defparameter-unbound
   #:defplace
   #:defsubst
   #:defvar-unbound))
(in-package :serapeum/definitions)
;;; For internal use.

(defstruct (unbound (:constructor unbound (var)))
  "Placeholder for an unbound variable."
  (var (error "No var") :read-only t :type symbol))

(defmethod make-load-form ((self unbound) &optional env)
  (make-load-form-saving-slots self
                               :slot-names '(var)
                               :environment env))

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

The original `deflex' is due to Rob Warnock.

This version of `deflex' differs from the original in the following ways:

- It is possible for VAL to close over VAR.
- On implementations that support it (SBCL, CCL, and LispWorks, at the
moment) this version creates a backing variable that is \"global\" or
\"static\", so there is not just a change in semantics, but also a
gain in efficiency.
- If VAR is a list that starts with `values`, each element is treated as
a separate variable and initialized as if by `(setf (values VAR...)
VAL)`."
  ;; From the ISO-COMPATIBILITY issue writeup: "DEFINE-SYMBOL-MACRO
  ;; can be used to define global lexicals, by having a global lexical
  ;; be a symbol macro that expands into a reference to a globally
  ;; allocated cell that is not subject to dynamic binding." So not
  ;; only was this use of `define-symbol-macro' foreseen, it is also a
  ;; major reason why it was included in CL at all. (The same goes for
  ;; `defconst' below.)
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
             collect `(def ,var (unbound ',var) ,@(unsplice documentation)))
     (setf (values ,@vars) ,expr)
     ',vars))

(defmacro define-values (values &body (expr))
  "Like `def', but for multiple values.
Each variable in VALUES is given a global, lexical binding, as with
`def', then set all at once, as with `multiple-value-setq'."
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

A constant defined with `defconst' is guaranteed to be available as
soon as it has been defined (for example, for use with reader macros
later in the same file). This is not guaranteed to be portably true
for `defconstant'.

The name is from Emacs Lisp."
  (let ((backing-var (symbolicate '#:+storage-for-deflex-var- symbol '+)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (define-constant ,backing-var ,init
         :test 'same-literal-p
         :documentation ,docstring)
       (define-symbol-macro ,symbol ,backing-var))))

;;;# Emacs-alikes

;;;## `defsubst'

;;; The name is of course by way of Emacs, although it's actually much
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

(defmacro defalias (alias &body (def &optional docstring)
                    &environment env)
  "Define a value as a top-level function.

     (defalias string-gensym (compose #'gensym #'string))

Like (setf (fdefinition ALIAS) DEF), but with a place to put
documentation and some niceties to placate the compiler.

Note that a function defined with `defalias' is declared `notinline'.
This is a matter of semantics: before we can assign to the function,
we must make it assignable (which is what `notinline' means).

Name from Emacs Lisp."
  `(progn
     (declaim (notinline ,alias))
     ,(multiple-value-bind (env decls lambda)
          (let-over-lambda def env)
        ;; If we can expand DEF into a lambda (possibly with some
        ;; variable bindings around it) then splice that lambda in
        ;; directly as the body of the function. (This is the same
        ;; function we use to resolve lambdas in `fbind'.)
        (if lambda
            (ematch lambda
              ((list* (and _ (eql 'lambda)) args body)
               `(let ,env
                  (declare ,@decls)
                  (defun ,alias ,args
                    ,@(unsplice docstring)
                    ,@body))))
            (with-unique-names (temp)
              `(let ((,temp (ensure-function ,def)))
                 (declare (type function ,temp))
                 ;; Give the function a temporary definition at
                 ;; compile time so the compiler doesn't complain
                 ;; about its being undefined.
                 (defun ,alias (&rest args)
                   (apply ,temp args))
                 (setf (fdefinition ',alias) ,temp)
                 ,@(unsplice
                    (and docstring
                         `(setf (documentation ',alias 'function) ,docstring)))))))))

;;;# Etc

(defmacro defplace (name args &body body)
  "Define NAME and (SETF NAME) in one go.

BODY is a list of forms, starting with an optional docstring. The last
form in BODY, however, must be a single, setf-able expression."
  (match body
    ((list (type string))
     (error "No form for ~s: ~a" 'defplace body))
    ((last (cons (type string) nil) 1)
     `(defplace ,name ,args ,@(reverse body)))
    (otherwise
     (with-gensyms (value)
       `(progn
          (defun ,name ,args
            ,@body)
          (defun (setf ,name) (,value ,@args)
            ,@(butlast body)
            (setf ,(lastcar body) ,value)))))))

(defmacro defvar-unbound (var &body (docstring))
  "Define VAR as if by `defvar' with no init form, and set DOCSTRING
as its documentation.

I believe the name comes from Edi Weitz."
  `(progn
     (defvar ,var)
     (setf (documentation ',var 'variable) ,docstring)
     ',var))

(defmacro defparameter-unbound (var &body (docstring))
  "Like `defvar-unbound', but ensures VAR is unbound when evaluated."
  `(progn
     (defvar ,var)
     (makunbound ',var)
     (setf (documentation ',var 'variable) ,docstring)
     ',var))

(defmacro defloop (name args &body body)
  "Define a function, ensuring proper tail recursion.
This is entirely equivalent to `defun' over `nlet'."
  (multiple-value-bind (body decls docstring)
      (parse-body body :documentation t)
    (let ((vars (lambda-list-vars args)))
      `(defun ,name ,args
         ,@(unsplice docstring)
         ,@decls
         (nlet ,name ,(mapcar #'list vars vars)
           (flet ((,name ,args (,name ,@vars)))
             (declare (inline ,name))
             ,@body))))))
