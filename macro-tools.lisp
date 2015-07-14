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

;;;## `unsplice'
;;; Found this gem in the code for Lparallel.

(declaim (inline unsplice))
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
        `(call-with-foo ,body)))

It is also possible to construct a \"thunk\" with arguments.

    (with-thunk (body foo)
      `(call-with-foo ,body))
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
          (symbol-macrolet ((,',thunk (function ,',thunk)))
            ,,@body)))))

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
  ;; NB `partition' is not yet defined.
  (let ((env2 (parse-declarations declarations env)))
    (flet ((build (env)
             (build-declarations 'declare env)))
      (if (null xs)
          (values nil (build env2))
          (values
           (build (filter-declaration-env env2 :affecting xs))
           (build (filter-declaration-env env2 :not-affecting xs)))))))

(defmacro seq-dispatch (seq &body (list-form array-form &optional other-form)
                        &environment env)
  "Efficiently dispatch on the type of SEQ."
  (declare (ignorable other-form))
  (let* ((dispatch-form
           #+ccl `(ccl::seq-dispatch ,seq ,list-form ,array-form)
           ;; Only SBCL and ABCL support extensible sequences right now.
           #+(or sbcl abcl)
           (once-only (seq)
             `(if (listp ,seq)
                  ,list-form
                  ,(if other-form
                       `(if (arrayp ,seq)
                            ,array-form
                            ,other-form)
                       array-form)))
           #-(or sbcl ccl)
           `(if (listp ,seq) ,list-form ,array-form)))
    (if (not (symbolp seq)) dispatch-form
        (let ((type (variable-type seq env)))
          (cond ((subtypep type 'list env)
                 (return-from seq-dispatch
                   list-form))
                ((subtypep type 'array env)
                 (return-from seq-dispatch
                   array-form))
                #+(or sbcl abcl)
                ((subtypep type 'sequence env)
                 (return-from seq-dispatch
                   other-form))
                (t dispatch-form))))))

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
  the values to return from the loop. While this return form is
  being evaluated, the iteration variables are bound to `nil'.

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
  (let ((ret-var (cadr (member '&optional (car binds))))
        ;; Handle both (key value table) and ((key value) table
        (vars (flatten (butlast (car binds) 3)))
        (body-var (cadr (member '&body (cdr binds)))))
    (unless ret-var
      (error "No binding for return form in ~s" (car binds)))
    (unless body-var
      (error "No binding for body in ~s" binds))
    `(defmacro ,name ,binds
       (multiple-value-bind (,body-var decls)
           (parse-body ,body-var)
         (let ((,body-var
                 `(,@decls
                   (tagbody ,@,body-var))))
           `(block nil
              ,,@body
              ,(when ,ret-var
                 `(let (,,@vars)
                    (declare (ignorable ,,@vars))
                    ,,ret-var))))))))

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
