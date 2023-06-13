(in-package #:serapeum)

;;; References:
;;; Waddell, Sarkar, and Dybvig, "Fixing Letrec".
;;; Ghuloum and Dybvig, "Fixing Letrec (reloaded)".
;;; Sullivan and Wand, "Incremental Lambda Lifting".

;;; TODO Ideally, lift flet, labels, and fbind forms immediately
;;; inside a literal lambda into the surrounding fbind.

;;; TODO Handle named-lambda and equivalent labels forms.

(defvar *lexenv* nil
  "The environment of the macro being expanded.")

(define-condition letrec-restriction-violation (error)
  ((args :initarg :args :accessor args-of))
  (:documentation "Violation of the letrec restriction.

The \"letrec restriction\" means that the expressions being bound in a
`letrec' cannot refer to the value of other bindings in the same
`letrec'.

For `fbindrec', the restriction applies everywhere. For `fbindrec*',
it only applies to functions not yet bound.")
  (:report (lambda (c s)
             (format s
                     "Letrec restriction violated with arguments ~a"
                     (args-of c)))))

(defun invalid (&rest args)
  (error 'letrec-restriction-violation :args args))

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

(defun declared-ftype (fn decls &optional env)
  "If DECLS contains an ftype declaration for FN, return it.
Returns three values. The first two values are the arguments type and
return type. The third is a boolean, which is `t' only when there was
an ftype declaration."
  (check-type fn symbol)
  (let ((decls (partition-declarations `(#',fn) decls env)))
    (dolist (decl decls (values nil nil nil))
      (match decl
        ((list 'declare (list 'ftype sig _))
          (return
            (match sig
              ((list 'function args ret)
                (values args ret t))
              ((list '-> args ret)
                (values args ret t)))))))))

(defun build-bind/ftype (fn var decls env)
  "Return a form, suitable for the bindings of a `flet' or `labels'
form, that binds a function named FN which calls the function in a
variable named VAR.

If there is a declared ftype for FN in the decls-env combination, it
may be used to make calling VAR more efficient by avoiding `apply'."
  (multiple-value-bind (args ret known?)
      (declared-ftype fn decls env)
    (declare (ignore ret))
    (flet ((give-up ()
             `(,fn (&rest args)
                   (declare (dynamic-extent args))
                   (apply ,var args))))
      (cond ((not known?) (give-up))
            ((null (intersection args lambda-list-keywords))
             (let ((args (make-gensym-list (length args))))
               `(,fn ,args (funcall ,var ,@args))))
            ;; We only care about fixed args at the moment.
            (t (give-up))))))

(defun partition-declarations-by-kind
    (simple complex lambda decls)
  (flet ((partn (defs decls)
           (let ((fns (loop for (name . nil) in defs
                            collect `(function ,name))))
             (partition-declarations fns decls *lexenv*))))
    (mvlet* ((simple decls (partn simple decls))
             (complex decls (partn complex decls))
             (lambda decls (partn lambda decls)))
      (values simple complex lambda decls))))

(defun expand-fbindings (bindings)
  (cond ((null bindings) nil)
        ((symbolp bindings)
         `((,bindings ,bindings)))
        (t (let ((bindings
                   (loop for binding in bindings
                         if (symbolp binding)
                           collect `(,binding ,binding)
                         else collect binding)))
             ;; Ensure bindings are of the correct length.
             (loop for (name expr) in bindings
                   collect (list name expr))))))

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
    ((list* 'partial fn args)
      (let-over-lambda `(curry ,fn ,@args) lexenv))
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

(defun analyze-fbinds (bindings lexenv)
  "Pull apart BINDINGS, looking for lambdas to lift."
  (let ((env '())
        (declarations '())
        (lambdas '())
        (binds '()))
    (loop for (var expr) in bindings do
      (multiple-value-bind (lets decls lambda)
          (let-over-lambda expr lexenv)
        (if (not lambda)
            (push (list var expr) binds)
            (progn (setf env (revappend lets env))
                   (setf declarations (revappend decls declarations))
                   (push (list var lambda) lambdas)))))
    (values (nreverse env)
            (nreverse declarations)
            (nreverse binds)
            (nreverse lambdas))))

(defun merely-syntactic-functions (names body-with-decls env)
  "Which functions in NAMES are merely syntactic?

Functions are \"merely syntactic\" if they are never dereferenced with
`function' (or sharp-quote).

This function may return false negatives -- it may fail to detect that
a function is syntactic -- but it should never return a false positive
-- it should never say a function is syntactic when it is not."
  ;; ECL does not seem to like this.
  (when (eql uiop:*implementation-type* :ecl)
    (return-from merely-syntactic-functions
      nil))
  ;; This is as simple as can be: we expand the body with
  ;; `macroexpand-all' and, if the expansion is valid, look for
  ;; function quotes in the expansion.
  (mvlet* ((body decls (parse-body body-with-decls))
           (exp exp? env-used?
            (trivial-macroexpand-all:macroexpand-all `(progn ,@body) env))
           (exp-useful?
            (and exp?
                 (or (null env)
                     (and env env-used?)))))
    (if (not exp-useful?) nil
        (let ((quoted-functions
                (collecting
                  (walk-tree (lambda (leaf)
                               (match leaf
                                 ((list 'function (and fn (type symbol)))
                                  (collect fn))))
                             exp))))
          (remove-if (lambda (name)
                       (let ((fq `(function ,name)))
                         (or
                          ;; We can't compile the function as a macro if
                          ;; there are declarations for it.
                          (partition-declarations (list fq) decls env)
                          (member name quoted-functions))))
                     names)))))

(defmacro fbind (bindings &body body &environment *lexenv*)
  "Binds values in the function namespace.

That is,
     (fbind ((fn (lambda () ...))))
     ≡ (flet ((fn () ...))),

except that a bare symbol in BINDINGS is rewritten as (symbol
symbol)."
  (setf bindings (expand-fbindings bindings))
  (unless bindings
    (return-from fbind `(locally ,@body)))
  ;; When possible (if the binding expands to a simple
  ;; let-over-lambda), we hoist the bindings to the top and bind
  ;; the lambda directly with flet.

  ;; Failing that, if we know that the function is never directly
  ;; dereferenced with sharp-quote, we can bind with `macrolet'
  ;; instead of `flet', leaving no runtime overhead vs. `funcall'.
  (mvlet* ((env env-decls bindings lambdas (analyze-fbinds bindings *lexenv*))
           (macro-vars
            (merely-syntactic-functions (mapcar #'first bindings)
                                        body
                                        *lexenv*))
           (vars  (mapcar #'first bindings))
           (exprs (mapcar #'second bindings))
           (temps (mapcar #'string-gensym vars))
           (body decls (parse-body body)))
    `(let ,env
       ,@(unsplice (and env-decls `(declare ,@env-decls)))
       "Hidden variable bindings to close over."
       (let ,(loop for temp in temps
                   for expr in exprs
                   collect `(,temp (ensure-function ,expr)))
         ,@(when temps
             (unsplice
              `(declare (function ,@temps))))
         (comment "fbind: Hidden variable bindings for function values.")
         (flet (,@(loop for (name lambda) in lambdas
                        collect `(,name ,@(cdr lambda)))
                ,@(loop for var in vars
                        for temp in temps
                        unless (member var macro-vars)
                          collect (build-bind/ftype var temp decls env)))
           #-sbcl (declare (inline ,@(set-difference vars macro-vars)))
           ,@decls
           (comment "fbind: Functions that might be sharp-quoted.")
           (macrolet (,@(loop for var in vars
                              for temp in temps
                              when (member var macro-vars)
                                collect `(,var
                                          (&rest args)
                                          (list* 'funcall ',temp args))))
             (comment "fbind: Macros for functions provably never sharp-quoted.")
             ,@body))))))

(defmacro fbind* (bindings &body body &environment env)
  "Like `fbind', but creates bindings sequentially."
  (setf bindings (expand-fbindings bindings))
  (unless bindings
    (return-from fbind* `(locally ,@body)))
  (mvlet* ((fn (caar bindings))
           (body decls (parse-body body))
           (local others (partition-declarations `((function ,fn))
                                                 decls
                                                 env)))
    `(fbind (,(first bindings))
       ,@local
       (fbind* ,(rest bindings)
         ,@others
         ,@body))))

(defun ignored-functions-in-decls (decls)
  (declare (notinline filter keep))         ;phasing
  ;; The names of the ignored functions.
  (mapcar #'second
          ;; Ignore declarations for functions.
          (filter #'listp
                  ;; The ignore declarations.
                  (mappend (lambda (decl)
                             (let ((decls (cdr decl)))
                               (mappend #'cdr
                                        (keep 'ignore decls :key #'car))))
                           decls))))

(defun ignored-functions-in-body (body)
  (~> body
      body-decls
      ignored-functions-in-decls))

(defun body-decls (body)
  (nth-value 1
    (parse-body body)))

(defun partition-fbinds (fbinds body)
  "Partition FBINDS using the \"Fixing Letrec\" algorithm.
Returns the bindings in four sets: unreferenced, literal lambdas,
simple (external references) and complex (everything else).

BODY is needed because we detect unreferenced bindings by looking for
`ignore' declarations."
  (let ((ignored (ignored-functions-in-body body)))
    ;; Since we cannot walk the code, our checks are limited to
    ;; special cases.
    (labels ((unreferenced? (expr)
               ;; If the function is declared to be ignored, then we
               ;; know for sure it is unreferenced. (This is more
               ;; useful than it sounds. After all the compiler warns
               ;; you if the function is unused but not declared
               ;; ignored.)
               (member expr ignored))
             (lambda? (expr)
               ;; Note that `analyze-fbinds' has already done the work
               ;; of exposing lambda inside let.
               (match expr
                 ((cons 'lambda _) t)))
             (simple? (expr)
               ;; Special cases where we can be sure the expressions
               ;; are simple -- that is, that they contain no
               ;; references to the functions bound by `fbindrec'.
               (let ((expr (expand-macro expr)))
                 (match expr
                   ((and _ (type symbol)) t)
                   ((list 'quote _) t)
                   ((list 'function _) t)
                   ((cons 'if body)
                     (and (simple? (first body))
                          (if (not (second body))
                              t
                              (simple? (second body)))))
                   ;; TODO Locally.
                   ((cons 'progn body)
                     (every #'simple? body))))))
      (loop for binding in fbinds
            for var = (first binding)
            for expr = (second binding)
            if (unreferenced? var)
              collect binding into unreferenced
            else if (lambda? expr)
                   collect binding into lambdas
            else if (simple? expr)
                   collect binding into simple
            else collect binding into complex
            finally (return (values simple complex lambdas unreferenced))))))

(defmacro fbindrec (bindings &body body &environment *lexenv*)
  "Like `fbind', but creates recursive bindings.

The consequences of referring to one binding in the expression that
generates another are undefined."
  (setf bindings (expand-fbindings bindings))
  (unless bindings
    (return-from fbindrec `(locally ,@body)))
  (mvlet* ((env env-decls bindings lambdas (analyze-fbinds bindings *lexenv*))
           (body decls (parse-body body))
           (simple complex lambda unref
            (partition-fbinds (append bindings lambdas)
                              body))
           (temps (mapcar (op (gensym (string (first _)))) complex))
           (simple-decls complex-decls lambda-decls others
            (partition-declarations-by-kind simple complex lambda decls)))
    `(let ,env
       ,@(unsplice (and env-decls `(declare ,@env-decls)))
       ;; Simple expressions reference functions already defined
       ;; outside of the letrec, so we can handle them with fbind.
       (comment "fbind: Simple")
       (fbind ,simple
         ,@simple-decls
         (comment "fbind: Temps for complex bindings")
         (let ,(loop for temp in temps collect `(,temp #'invalid))
           (declare ,@(loop for temp in temps collect `(function ,temp)))
           (flet ,(loop for (name nil) in complex
                        for temp in temps
                        collect (build-bind/ftype name temp complex-decls env))
             ,@complex-decls
             (comment "fbind: Lambdas")
             (labels (,@(loop for (name lambda) in lambda
                              collect `(,name ,@(cdr lambda))))
               ,@lambda-decls
               (comment "fbind: Unreferenced")
               (progn ,@(mapcar #'second unref))
               (comment "fbind: Complex")
               (psetf ,@(loop for temp in temps
                              for (nil expr) in complex
                              append `(,temp (ensure-function ,expr))))
               (locally ,@others
                 ,@body))))))))

(defmacro fbindrec* (bindings &body body &environment *lexenv*)
  "Like `fbindrec`, but the function defined in each binding can be
used in successive bindings."
  (setf bindings (expand-fbindings bindings))
  (unless bindings
    (return-from fbindrec* `(locally ,@body)))
  (mvlet* ((env env-decls binds lambdas (analyze-fbinds bindings *lexenv*))
           (simple complex lambda unref
            (partition-fbinds (append binds lambdas)
                              body))
           (temps (mapcar (op (gensym (string (first _)))) complex))
           (body decls (parse-body body))
           (simple-decls complex-decls lambda-decls others
            (partition-declarations-by-kind simple complex lambda decls)))
    `(let
         ;; Use dummies when we can (with ensure-function).
         ,(loop for (var init) in env
                collect (if (and (listp init)
                                 (eql (car init) 'ensure-function))
                            `(,var #'invalid)
                            var))
       ,@(unsplice (and env-decls `(declare ,@env-decls)))
       (comment "fbind: Simple bindings")
       (fbind ,simple
         ,@simple-decls
         (comment "fbind: Temps for complex bindings")
         (let ,(loop for temp in temps collect `(,temp #'invalid))
           (declare ,@(loop for temp in temps collect `(function ,temp)))
           (flet ,(loop for (name nil) in complex
                        for temp in temps
                        collect (build-bind/ftype name temp complex-decls env))
             ,@complex-decls
             (comment "fbind: Lambdas")
             (labels (,@(loop for (name lambda) in lambda
                              collect `(,name ,@(cdr lambda))))
               ,@lambda-decls
               (comment "fbind: Interleave unreferenced and complex bindings in order.")
               ,@(remove nil
                         (loop for (name nil) in bindings
                               append (or (loop for (uname init) in unref
                                                if (eql uname name)
                                                  return (list init))
                                          (loop for (cname init) in complex
                                                for temp in temps
                                                if (eql cname name)
                                                  return `((setf ,temp (ensure-function ,init)))))))
               (comment "fbind: Set the `env` variables for the lambdas")
               (setf ,@(apply #'append env))
               (locally ,@others
                 ,@body))))))))
