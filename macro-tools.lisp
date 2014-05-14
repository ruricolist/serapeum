(in-package :serapeum)
(in-readtable :fare-quasiquote)

(export '(unsplice string-gensym with-thunk
          expand-macro expand-macro-recursively
          parse-declarations
          expand-declaration
          partition-declarations))

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
Return each non-default clauses normalized to `(keys . body)', where
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

;;;## `unsplice'
;;; Found this gem in the code for Lparallel.

(defun unsplice (form)
  "If FORM is non-nil, wrap it in a list.

This is useful with ,@ in macros, and with `mapcan'.

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
        `(call-with-foo #',body)))

It is also possible to construct a \"thunk\" with arguments.

    (with-thunk (body foo)
      `(call-with-foo #',body))
    ≡ `(flet ((,thunk (,foo)
          ,@body))
        (declare (dynamic-extent #',thunk))
        (call-with-foo #',thunk))

Needs a better name."
  (with-gensyms (b thunk gargs)
    `(let ((,b ,var)
           (,var ',thunk)
           (,gargs (list ,@args)))
       `(flet ((,',thunk ,,gargs
                 ,@,b))
          (declare (dynamic-extent (function ,',thunk)))
          ,,@body))))

;;;# Expanding macros
;;; Expanding macros, Swank-style. We use `labels' in these
;;; definitions because `nlet' hasn't been defined yet.

(defun compiler-macroexpand-1 (form &optional env)
  "Expand a compiler macro."
  (let ((fun (and (consp form)
                  (ignore-errors (fboundp (car form)))
                  (compiler-macro-function (car form)))))
    (if fun
        (let ((result (funcall *macroexpand-hook* fun form env)))
          (values result (not (eq result form))))
        (values form nil))))

(defun compiler-macroexpand (form &optional env)
  "Recursively call `compiler-macroexpand-1'."
  (labels ((expand (form expanded?)
             (multiple-value-bind (form new?)
                 (compiler-macroexpand-1 form)
               (if (not new?)
                   (values form expanded?)
                   (expand form t)))))
    (expand form env)))

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

;;; `parse-declarations' needs to be able to tell if a declaration is
;;; a type. Swank has something along these lines, but I'm not sure
;;; how useful it is.

;;; Found the implementation-specific functions in
;;; <https://github.com/m2ym/trivial-types/blob/master/src/typespecs.lisp>
;;; after Googling `type-specifier-p'.

;;; If I am reading the hyperspec correctly, however, atom and
;;; sequence form an exhaustive partition of all CL types, so checking
;;; for a subtype of `(or atom sequence)' should suffice.

(defun type-specifier-p (x)
  "Is TYPE a type specifier?"
  (when (or (symbolp x) (listp x))
    (or (documentation x 'type)
        #+sbcl (sb-ext:valid-type-specifier-p x)
        #+openmcl (ccl:type-specifier-p x)
        #+ecl (c::valid-type-specifier x)
        #-(or sbcl openmcl ecl)
        (or (eql x t)
            (subtypep x '(or sequence atom))))))

(defun parse-declarations (declarations)
  "Pick apart a list of `declare' forms.

Parse DECLARATIONS into an alist of (identifier . declarations).
Declarations should be a list like ((declare ...) ...), as would be
returned by `alexandria:parse-body'.

Declarations that are specific to functions are normalized to
use `(function ,identifier).

Type declarations are normalized to the form `(type ,type).

Ftype declarations are also normalized.

     (parse-declarations
      '((declare
         (fixnum x)
         (type list xs)
         (ftype (-> list fixnum) frob)
         (inline frob)
         (dynamic-extent #'frob))))
     => '((#'frob dynamic-extent inline (ftype (-> list fixnum)))
          (xs (type list))
          (x (type fixnum)))

Return any optimizations declared as a second value."
  (let ((decls '())
        (optimize nil))
    (labels ((decls-for (x)
               (or (assoc x decls :test 'equal)
                   (let ((cons (cons x nil)))
                     (push cons decls)
                     cons))))
      (dolist (decl (mappend #'cdr declarations))
        (ematch decl
          ;; Optimization settings.
          ((list* 'optimize settings)
           (setf optimize settings))
          ((list* 'ftype ftype fns)
           (dolist (fn fns)
             (pushnew (list 'ftype ftype)
                      (cdr (decls-for `(function ,fn)))
                      :test 'equal)))
          ((list* 'inline fns)
           (dolist (fn fns)
             (pushnew 'inline (cdr (decls-for `(function ,fn))))))
          ((list* 'notinline fns)
           (dolist (fn fns)
             (pushnew 'notinline (cdr (decls-for `(function ,fn))))))
          ((list* 'type type vars)
           (dolist (var vars)
             (pushnew `(type ,type)
                      (cdr (decls-for var))
                      :test 'equal)))
          ((list* declaration identifiers)
           ;; The declaration is a type specifier.
           (when (type-specifier-p declaration)
             (setf declaration `(type ,declaration)))
           (dolist (id identifiers)
             (pushnew declaration (cdr (decls-for id))))))))
    (values decls optimize)))

(defun expand-declaration (decl)
  "Opposite of `parse-declarations'.

Take a (identifier . declarations) pair, as returned by
`parse-declarations', and turn it into a declaration form that can be
used in Lisp code.

     (locally ,(expand-declaration decl) ...)

Might be used to transfer declarations made for a variable to another,
temporary variable."
  (destructuring-bind (x . decls) decl
    `(declare ,@(if (atom x)
                    (loop for decl in decls
                          if (atom decl)
                            collect `(,decl ,x)
                          else collect `(,@decl ,x))
                    (loop for decl in decls
                          if (atom decl)
                            collect (case decl
                                      (inline `(inline ,(second x)))
                                      (notinline `(notinline ,(second x)))
                                      (t `(,decl ,x)))
                          else collect (if (eql (car decl) 'ftype)
                                           `(,@decl ,(second x))
                                           `(,@decl ,x)))))))

(defun partition-declarations (xs declarations)
  "Split DECLARATIONS into those that do and do not apply to XS.
Return two values, one with each set.

Both sets of declarations are returned in a form that can be spliced
directly into Lisp code:

     (locally ,@(partition-declarations vars decls) ...)"
  ;; NB `partition' is not yet defined.
  (loop for decl in (parse-declarations declarations)
        if (member (car decl) xs :test 'equal)
          collect decl into kept
        else collect decl into removed
        finally (return (values (mapcar #'expand-declaration kept)
                                (mapcar #'expand-declaration removed)))))

(defun substitute-declarations (substitutions declarations)
  (let ((decls (parse-declarations declarations)))
    (mapcar #'expand-declaration
            (loop for (var . ds) in decls
                  collect (cons (or (cdr (assoc var substitutions))
                                    var)
                                ds)))))
