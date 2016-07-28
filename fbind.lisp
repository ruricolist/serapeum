(in-package #:serapeum)
(in-readtable :fare-quasiquote)

;;; References:
;;; Waddell, Sarkar, and Dybvig, "Fixing Letrec".
;;; Ghuloum and Dybvig, "Fixing Letrec (reloaded)".
;;; Sullivan and Wand, "Incremental Lambda Lifting".

;;; TODO

;;; The binding of lifted environments in fbindrec* is inefficient and
;;; disregards the declarations. In at least some cases, like
;;; `ensure-function', we should be able to assign dummies.

;;; fbindrec and fbindrec* don't do enough to enforce the letrec
;;; restriction.

;;; Ideally, lift flet, labels, and fbind forms immediately inside a
;;; literal lambda into the surrounding fbind.

;;; When rebinding non-gensyms, analyze the lambda lists and elide the
;;; inner lambda when possible.

(defvar *env* nil
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

(defun check-lambda-list (lambda-list)
  (parse-ordinary-lambda-list lambda-list))

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
        (`(declare (ftype ,sig ,_))
          (return
            (match sig
              (`(function ,args ,ret)
                (values args ret t))
              (`(-> ,args ,ret)
                (values args ret t)))))))))

(defun build-bind/ftype (var temp decls env)
  (multiple-value-bind (args ret known?)
      (declared-ftype var decls env)
    (flet ((give-up ()
             `(,var (&rest args)
                    (declare (dynamic-extent args))
                    (apply ,temp args))))
      (cond ((not known?) (give-up))
            ((notany (lambda (arg)
                       (member arg lambda-list-keywords))
                     args)
             (let ((args (make-gensym-list (length args))))
               `(,var ,args (the ,ret (funcall ,temp ,@args)))))
            ;; We only care about fixed args at the moment.
            (t (give-up))))))

(defun partition-declarations-by-kind
    (simple complex lambda decls)
  (flet ((partn (defs decls)
           (let ((fns (loop for (name . nil) in defs
                            collect `(function ,name))))
             (partition-declarations fns decls *env*))))
    (mvlet* ((simple decls (partn simple decls))
             (complex decls (partn complex decls))
             (lambda decls (partn lambda decls)))
      (values simple complex lambda decls))))

(defun expand-fbindings (bindings)
  (cond ((null bindings) nil)
        ((symbolp bindings)
         `((,bindings ,bindings)))
        (t (loop for binding in bindings
                 if (symbolp binding)
                   collect `(,binding ,binding)
                 else collect binding))))

;;; TODO Handle let*, mvbind?
(defun let-over-lambda (form)
  "Expand form, using `expand-macro'. If the result is a simple let-over-lambda,
analyze it into an environment, declarations, and a lambda."
  (match form
    ;; Special cases for `complement` and `constantly`.
    (`(complement ,fn)
      (with-gensyms (temp)
        (values `((,temp (ensure-function ,fn)))
                `((function ,temp))
                `(lambda (&rest args)
                   (declare (dynamic-extent args))
                   (not (apply ,temp args))))))
    (`(constantly ,x)
      (with-gensyms (temp)
        (values `((,temp ,x))
                nil
                `(lambda (&rest args)
                   (declare (ignore args))
                   ,temp))))
    ;; TODO Disjoin, conjoin, and rcurry don't have compiler macros.
    (`(,(and fun (or 'conjoin 'disjoin)) ,pred ,@preds)
      (let* ((preds (cons pred preds))
             (temps (loop for nil in preds collect (gensym))))
        (values (mapcar (lambda (temp pred)
                          `(,temp (ensure-function ,pred)))
                        temps preds)
                nil
                `(lambda (&rest args)
                   (,(case fun
                       (conjoin 'and)
                       (disjoin 'or))
                    ,@(loop for temp in temps
                            collect `(apply ,temp args)))))))
    (`(rcurry ,fun ,@args)
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
    ;; A plain lambda.
    ((or `(lambda ,args ,@body)
         `(function (lambda ,args ,@body)))
     (values nil nil `(lambda ,args ,@body)))
    ;; let* with single binding. Note that Clozure, at least, expands
    ;; let with only one binding into let*.
    (`(let* (,binding) ,@body)
      (let-over-lambda `(let ,binding ,@body)))
    ;; let-over-lambda.
    (`(let ,(and bindings (type list)) ,@body)
      (multiple-value-bind (forms decls)
          (parse-body body)
        (match forms
          (`(,(or `(lambda ,args ,@body)
                  `(function (lambda ,args ,@body))))
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
    (otherwise (let ((exp (expand-macro form)))
                 (if (eq exp form)
                     nil
                     (let-over-lambda exp))))))

(defun analyze-fbinds (bindings)
  "Pull apart BINDINGS, looking for lambdas to lift."
  (let ((env '())
        (declarations '())
        (lambdas '())
        (binds '()))
    (loop for (var expr) in bindings do
      (multiple-value-bind (lets decls lambda)
          (let-over-lambda expr)
        (if (not lambda)
            (push (list var expr) binds)
            (progn (setf env (revappend lets env))
                   (setf declarations (revappend decls declarations))
                   (push (list var lambda) lambdas)))))
    (values (nreverse env)
            (nreverse declarations)
            (nreverse binds)
            (nreverse lambdas))))

(defmacro fbind (bindings &body body &environment *env*)
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
  (mvlet* ((env env-decls bindings lambdas (analyze-fbinds bindings))
           (body decls (parse-body body))
           (vars  (mapcar #'first bindings))
           (exprs (mapcar #'second bindings))
           (temps (mapcar #'string-gensym vars)))
    `(let ,env
       ,@(when env-decls (unsplice `(declare ,@env-decls)))
       (let ,(loop for temp in temps
                   for expr in exprs
                   collect `(,temp (ensure-function ,expr)))
         ,@(when temps
             (unsplice
              `(declare (function ,@temps))))
         (flet (,@(loop for (name lambda) in lambdas
                        collect `(,name ,@(cdr lambda)))
                ,@(loop for var in vars
                        for temp in temps
                        collect (build-bind/ftype var temp decls env)))
           #-sbcl (declare (inline ,@vars))
           ,@decls
           ,@body)))))

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

(defun partition-fbinds (fbinds body)
  "Partition FBINDS using the \"Fixing Letrec\" algorithm.
Returns the bindings in four sets: unreferenced, literal lambdas,
simple (external references) and complex (everything else).

BODY is needed because we detect unreferenced bindings by looking for
`ignore' declarations."
  (let ((ignored
          (filter #'listp
                  (mappend (lambda (decl)
                             (cdr (assoc 'ignore (cdr decl))))
                           (nth-value 1 (parse-body body))))))
    (labels ((unreferenced? (expr)
               (find expr ignored :key #'second))
             (lambda? (expr)
               (and (listp expr) (eql (car expr) 'lambda)))
             (simple? (expr)
               (let ((expr (expand-macro expr)))
                 (match expr
                   ((and _ (type symbol)) t)
                   (`(quote ,_) t)
                   (`(function ,_) t)
                   (`(if ,@body)
                    (and (simple? (first body))
                         (if (not (second body))
                             t
                             (simple? (second body)))))
                   ;; TODO Locally.
                   (`(progn ,@body)
                    (every #'simple? body)))))
             (tag-expr (var expr)
               (cond ((unreferenced? var) 'unreferenced)
                     ((lambda? expr) 'lambda)
                     ((simple? expr) 'simple)
                     (t 'complex))))
      (let* ((tagged (loop for binding in fbinds
                           for var = (first binding)
                           for expr = (second binding)
                           collect (list (tag-expr var expr) var expr)))
             (partitioned (mapcar (lambda (kind)
                                    (cons (caar kind) (mapcar #'cdr kind)))
                                  (assort tagged :key #'car))))
        (values (cdr (assoc 'simple partitioned))
                (cdr (assoc 'complex partitioned))
                (cdr (assoc 'lambda partitioned))
                (cdr (assoc 'unreferenced partitioned)))))))

(defmacro fbindrec (bindings &body body &environment *env*)
  "Like `fbind', but creates recursive bindings.

The consequences of referring to one binding in the expression that
generates another are undefined."
  (setf bindings (expand-fbindings bindings))
  (unless bindings
    (return-from fbindrec `(locally ,@body)))
  (mvlet* ((env env-decls bindings lambdas (analyze-fbinds bindings))
           (body decls (parse-body body))
           (simple complex lambda unref
            (partition-fbinds (append bindings lambdas)
                              body))
           (temps (mapcar (compose #'gensym #'string #'first) complex))
           (simple-decls complex-decls lambda-decls others
            (partition-declarations-by-kind simple complex lambda decls)))
    `(let ,env
       ,@(when env-decls (unsplice `(declare ,@env-decls)))
       ;; Simple expressions reference functions already defined
       ;; outside of the letrec, so we can handle them with fbind.
       (comment "Simple")
       (fbind ,simple
         ,@simple-decls
         (comment "Temps for complex bindings")
         (let ,(loop for temp in temps collect `(,temp #'invalid))
           (declare ,@(loop for temp in temps collect `(function ,temp)))
           (flet ,(loop for (name nil) in complex
                        for temp in temps
                        collect (build-bind/ftype name temp complex-decls env))
             ,@complex-decls
             (comment "Lambdas")
             (labels (,@(loop for (name lambda) in lambda
                              collect `(,name ,@(cdr lambda))))
               ,@lambda-decls
               (comment "Unreferenced")
               (progn ,@(mapcar #'second unref))
               (comment "Complex")
               (psetf ,@(loop for temp in temps
                              for (nil expr) in complex
                              append `(,temp (ensure-function ,expr))))
               (locally ,@others
                 ,@body))))))))

(defmacro fbindrec* (bindings &body body &environment *env*)
  "Like `fbindrec`, but the function defined in each binding can be
used in successive bindings."
  (setf bindings (expand-fbindings bindings))
  (unless bindings
    (return-from fbindrec* `(locally ,@body)))
  (mvlet* ((env env-decls binds lambdas (analyze-fbinds bindings))
           (simple complex lambda unref
            (partition-fbinds (append binds lambdas)
                              body))
           (temps (mapcar (compose #'gensym #'string #'first) complex))
           (body decls (parse-body body))
           (simple-decls complex-decls lambda-decls others
            (partition-declarations-by-kind simple complex lambda decls)))
    env-decls ;; TODO Use env-decls.
    `(let
         ;; Use dummies when we can (with ensure-function).
         ,(loop for (var init) in env
                collect (if (and (listp init)
                                 (eql (car init) 'ensure-function))
                            `(,var #'invalid)
                            var))
       (comment "Simple bindings")
       (fbind ,simple
         ,@simple-decls
         (comment "Temps for complex bindings")
         (let ,(loop for temp in temps collect `(,temp #'invalid))
           (declare ,@(loop for temp in temps collect `(function ,temp)))
           (flet ,(loop for (name nil) in complex
                        for temp in temps
                        collect (build-bind/ftype name temp complex-decls env))
             ,@complex-decls
             (comment "Lambdas")
             (labels (,@(loop for (name lambda) in lambda
                              collect `(,name ,@(cdr lambda))))
               ,@lambda-decls
               (comment "Interleave unreferenced and complex bindings in order.")
               ,@(remove nil
                         (loop for (name nil) in bindings
                               append (or (loop for (uname init) in unref
                                                if (eql uname name)
                                                  return (list init))
                                          (loop for (cname init) in complex
                                                for temp in temps
                                                if (eql cname name)
                                                  return `((setf ,temp (ensure-function ,init)))))))
               (comment "Set the `env` variables for the lambdas")
               (setf ,@(apply #'append env))
               (locally ,@others
                 ,@body))))))))
