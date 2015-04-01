(in-package :serapeum)
(in-readtable :fare-quasiquote)

(import 'uiop:nest)

(export '(eval-and-compile
          no nor nand
          typecase-of case-of
          etypecase-of ecase-of
          ctypecase-of ccase-of
          ;; typecase-of case-of
          string-case string-ecase
          case-using
          select selector
          econd econd-failure
          econd-let ecase-let
          cond-let case-let
          bcond
          comment example
          nix
          ensure ensure2
          ~> ~>>
          cond-every
          uiop:nest))

(defmacro prog0 (&body body)
  "Execute BODY like `progn' but return nothing."
  `(progn
     ,@body
     (values)))

(defmacro compile-time-value (expr &optional read-only-p)
  "Evaluate EXPR at compile time and inline the result."
  ;; 3.2.2.2
  ;; “The first argument in a ‘load-time-value’ form in source code
  ;; processed by ‘compile’ is evaluated at compile time; in source
  ;; code processed by ‘compile-file’ , the compiler arranges for it
  ;; to be evaluated at load time. In either case, the result of the
  ;; evaluation is remembered and used later as the value of the
  ;; ‘load-time-value’ form at execution time.”
  (if *compile-file-pathname*
      (let ((result (eval expr)))
        (when (typep result '(or character number))
          (setf read-only-p t))
        ;; Should we do something with read-only-p here? SBCL wraps
        ;; the value with a cell to suppress warnings about constant
        ;; modification if read-only-p is nil.
        `(quote ,result))
      `(load-time-value ,expr ,read-only-p)))

(defmacro eval-and-compile (&body body)
  "Emacs's `eval-and-compile'.

Shorthand for
        (eval-when (:compile-toplevel :load-toplevel :execute) ...)"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defun no (x)
  "Another alias for `not' and `null'.

From Arc."
  (not x))

(define-compiler-macro no (x)
  `(not ,x))

(defmacro nor (&rest forms)
  "Equivalent to (not (or ...)).

From Arc."
  (if (null forms) t
      (if (null (rest forms))
          `(not ,(first forms))
          `(if ,(first forms)
               nil
               (nor ,@(rest forms))))))

(defmacro nand (&rest forms)
  "Equivalent to (not (and ...))."
  (if (null forms) nil
      (if (null (rest forms))
          `(not ,(first forms))
          `(if ,(first forms)
               (nand ,@(rest forms))
               t))))

(defun check-exhaustiveness (style type body env)
  ;; Should we do redundancy checking? Is there any Lisp that doesn't
  ;; already warn about that?
  (check-type style (member case typecase))
  (labels ((subtype? (subtype type) (subtypep subtype type env))
           (same-type? (type1 type2)
             (multiple-value-bind (sub sure) (subtype? type1 type2)
               (if (not sub)
                   (values nil sure)
                   (subtype? type2 type1))))
           (check-subtypep (subtype)
             (multiple-value-bind (subtype sure)
                 (subtype? subtype type)
               (cond ((not sure)
                      (warn "Can't tell if ~s is a subtype of ~s. Is ~s defined?"
                            subtype type type))
                     ((not subtype)
                      (warn "~s is not a subtype of ~s" subtype type)))))
           (explode-type (type)
             (match type
               ((list* 'or subtypes) subtypes)
               ((list* 'member subtypes)
                (loop for subtype in subtypes collect `(eql ,subtype)))))
           (extra-types (partition)
             (loop for subtype in (explode-type partition)
                   unless (subtype? subtype type)
                     collect subtype))
           (format-extra-types (stream partition)
             (when-let (et (extra-types partition))
               (format stream "~&There are extra types: ~s" et)))
           (typexpand (type)
             ;; TODO Other implementations?
             #+sbcl (sb-ext:typexpand type)
             #+ccl  (let ((exp (ccl::type-expand type)))
                      (values exp (not (eq exp type))))
             #-(or sbcl ccl) (values nil nil))
           (missing-types (partition)
             (multiple-value-bind (exp exp?) (typexpand type)
               (when exp?
                 (set-difference (explode-type exp)
                                 (explode-type partition)
                                 :test #'type=))))
           (format-missing-types (stream partition)
             (when-let (mt (missing-types partition))
               (format stream "~&There are missing types: ~s" mt)))
           (generate-warning (part)
             (with-output-to-string (s)
               (format s "~&Non-exhaustive match: ")
               (cond ((subtype? part type)
                      (format s "~s is a proper subtype of ~s." part type))
                     ((subtype? type part)
                      (format s "~s contains types not in ~s." part type))
                     (t (format s "~s is not the same as ~s" part type)))
               (format-extra-types s part)
               (format-missing-types s part)))
           (check-exhaustive (partition)
             (multiple-value-bind (same sure) (same-type? partition type)
               (cond ((not sure)
                      (warn "Can't check exhaustiveness: cannot determine if ~s is the same as ~s"
                            partition type))
                     (same)
                     (t (warn "~a" (generate-warning partition))))))
           (check-subtypes (body)
             (dolist (clause body)
               (check-subtypep (clause-type clause))))
           (clause-type (clause)
             (ecase style
               ((typecase) (car clause))
               ((case) `(member ,@(ensure-list (car clause))))))
           (merge-clause-types (body)
             (ecase style
               ((typecase) `(or ,@(mapcar #'car body)))
               ((case) `(member ,@(mappend (compose #'ensure-list #'car) body))))))
    (check-subtypes body)
    (check-exhaustive (merge-clause-types body))))

(defmacro typecase-of (type x &body clauses &environment env)
  "Like `etypecase-of', but may, and must, have an `otherwise' clause
in case X is not of TYPE."
  (let* ((otherwise (find 'otherwise clauses :key #'car))
         (clauses (remove otherwise clauses)))
    (unless otherwise
      (error "No otherwise clause in typecase-of for type ~s" type))

    (check-exhaustiveness 'typecase type clauses env)
    `(typecase ,x
       ,@clauses
       ,otherwise)))

(defmacro etypecase-of (type x &body body)
  "Like `etypecase' but, at compile time, warn unless each clause in
BODY is a subtype of TYPE, and the clauses in BODY form an exhaustive
partition of TYPE."
  (once-only (x)
    `(typecase-of ,type ,x
       ,@body
       (otherwise
        (error 'type-error
               :datum ,x
               :expected-type ',type)))))

(defmacro case-of (type x &body clauses &environment env)
  "Like `case-of' but may, and must, have an `otherwise' clause "
  (let* ((otherwise (find 'otherwise clauses :key #'car))
         (clauses (remove otherwise clauses)))
    (unless otherwise
      (error "No otherwise clause in case-of for type ~s" type))

    (check-exhaustiveness 'case type clauses env)
    `(case ,x
       ,@clauses
       ,otherwise)))

(defmacro ecase-of (type x &body body)
  "Like `ecase' but, given a TYPE (which should be defined as `(member
...)'), warn, at compile time, unless the keys in BODY are all of TYPE
and, taken together, they form an exhaustive partition of TYPE."
  (once-only (x)
    `(case-of ,type ,x
       ,@body
       (otherwise
        (error 'type-error
               :datum ,x
               :expected-type ',type)))))

(defmacro ctypecase-of (type keyplace &body body &environment env)
  "Like `etypecase-of', but providing a `store-value' restart to correct KEYPLACE and try again."
  (check-exhaustiveness 'typecase type body env)
  `(ctypecase ,keyplace ,@body))

(defmacro ccase-of (type keyplace &body body &environment env)
  "Like `ecase-of', but providing a `store-value' restart to correct KEYPLACE and try again."
  (check-exhaustiveness 'case type body env)
  `(ccase ,keyplace ,@body))

(defmacro case-using (pred keyform &body clauses)
  "ISLISP's case-using.

     (case-using #'eql x ...)
     ≡ (case x ...).

Note that, no matter the predicate, the keys are not evaluated. (But see `selector'.)

This version supports both single-item clauses (x ...) and
multiple-item clauses ((x y) ...), as well as (t ...) or (otherwise
...) for the default clause."
  (case (extract-function-name pred)
    (eql `(case ,keyform ,@clauses))
    (string= `(string-case ,keyform ,@clauses))
    (t (once-only (keyform)
         (rebinding-functions (pred)
           `(case-using-aux ,pred ,keyform ,@clauses))))))

(defmacro case-using-aux (pred keyform &body clauses)
  (if (not clauses)
      nil
      (destructuring-bind ((key . body) . clauses) clauses
        (if (member key '(t otherwise))
            `(progn ,@body)
            `(if (or ,@(mapcar (lambda (key)
                                 `(funcall ,pred ,keyform ',key))
                               (ensure-list key)))
                 (progn ,@body)
                 (case-using-aux ,pred ,keyform
                   ,@clauses))))))

(eval-and-compile
  (defun expand-string-case (sf default cases)
    "Expand a string-case macro with a minimum of duplicated code."
    (once-only (sf)
      (let ((keys (mapcar #'car cases)))
        (flet ((single (l) (null (cdr l))))
          (if (every #'single keys)
              ;; Simple.
              `(string-case:string-case (,sf :default ,default)
                 ,@(loop for ((k) . body) in cases
                         collect (cons k body)))
              (let* ((simple (remove-if-not #'single cases :key #'car))
                     (complex (set-difference cases simple)))
                (with-gensyms (block)
                  `(block ,block
                     ,(let ((tags (make-gensym-list (length complex) (string 'body))))
                        `(tagbody
                            (return-from ,block
                              (string-case:string-case (,sf :default ,default)
                                ,@(loop for ((key) . body) in simple
                                        collect `(,key ,@body))
                                ,@(loop for keys in (mapcar #'car complex)
                                        for tag in tags
                                        append (loop for k in keys
                                                     collect `(,k (go ,tag))))))
                            ,@(loop for tag in tags
                                    for body in (mapcar #'cdr complex)
                                    append `(,tag (return-from ,block (progn ,@body)))))))))))))))

(defmacro string-case (stringform &body cases)
  "Efficient `case'-like macro with string keys.

This uses Paul Khuong's `string-case' macro internally."
  (multiple-value-bind (cases default)
      (normalize-cases cases)
    (expand-string-case stringform `(progn ,@default) cases)))

(defmacro string-ecase (stringform &body cases)
  "Efficient `ecase'-like macro with string keys.

Cf. `string-case'."
  (let* ((cases (normalize-cases cases :allow-default nil))
         (keys (mappend (compose #'ensure-list #'car) cases)))
    (once-only (stringform)
      (expand-string-case stringform
                          `(error "~s is not one of ~s"
                                  ,stringform ',keys)
                          cases))))

(define-condition econd-failure (error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "ECOND fell through.")))
  (:documentation "A failed ECOND form."))

(defmacro econd (&rest clauses)
  "Like `cond', but signal an error of type `econd-failure' if no
clause succeeds."
  `(cond ,@clauses
         (t (error 'econd-failure))))

(defmacro cond-let (var &body clauses)
  "Cross between COND and LET.

     (cond-let x ((test ...)))
     ≡ (let (x)
         (cond ((setf x test) ...)))

Cf. `acond' in Anaphora."
  (match clauses
    (() nil)
    (`((,test) ,@clauses)
     `(if-let (,var ,test)
        ,var
        (cond-let ,var ,@clauses)))
    (`((t ,@body) ,@_)
     `(progn ,@body))
    (`((,test ,@body) ,@clauses)
     `(if-let (,var ,test)
        (progn ,@body)
        (cond-let ,var ,@clauses)))))

(defmacro econd-let (symbol &rest clauses)
  "Like `cond-let' for `econd'."
  `(cond-let ,symbol
     ,@clauses
     (t (error 'econd-failure))))

;;; cond-every has the same syntax as cond, but executes every clause
;;; whose condition is satisfied, not just the first. If a condition
;;; is the symbol otherwise, it is satisfied if and only if no
;;; preceding condition is satisfied. The value returned is the value
;;; of the last body form in the last clause whose condition is
;;; satisfied. Multiple values are not returned.

(defmacro cond-every (&body clauses)
  "Like `cond', but instead of stopping after the first clause that
succeeds, run all the clauses that succeed.

Return the value of the last successful clause.

If a clause begins with `cl:otherwise', it runs only if no preceding
form has succeeded.

From Zetalisp."
  (with-gensyms (sat ret any)
    (let* ((otherwise-clause (find 'otherwise clauses :key #'car))
           (any-decl (if otherwise-clause `(declare (ignorable ,any)))))
      (labels ((expand (clauses)
                 (if (null clauses)
                     ret
                     (multiple-value-bind (test body)
                         (match (first clauses)
                           ;; Test without body (return the value of the test).
                           (`(,test)
                             (values t (list test)))
                           ;; Otherwise; only run if nothing else has.
                           (`(otherwise ,@body)
                             (values `(not ,any) body))
                           ;; An ordinary clause.
                           (`(,test ,@body)
                             (values test body)))
                       `(let* ((,sat ,test)
                               (,ret (if ,sat (progn ,@body) ,ret))
                               ,@(unsplice (if otherwise-clause `(,any (or ,any ,sat)))))
                          ,@(unsplice any-decl)
                          ,(expand (rest clauses)))))))
        `(let (,@(unsplice (if otherwise-clause `(,any nil)))
               (,ret nil))
           ,@(unsplice any-decl)
           ,(expand clauses))))))

(defmacro bcond (&rest clauses)
  "Scheme's extended COND.

This is exactly like COND, except for clauses having the form

     (test :=> recipient)

In that case, if TEST evaluates to a non-nil result, then RECIPIENT, a
function, is called with that result, and the result of RECIPIENT is
return as the value of the `cond`.

As an extension, a clause like this:

     (test :=> var ...)

Can be used as a shorthand for

     (test :=> (lambda (var) ...))

The name `bcond' for a “binding cond” goes back at least to the days
of the Lisp Machines. I do not know who was first to use it, but the
oldest examples I have found are by Michael Parker and Scott L.
Burson."
  (flet ((send-clause? (clause)
           (let ((second (second clause)))
             (and (symbolp second)
                  (string= second :=>))))
         (parse-send-clause (clause)
           (destructuring-bind (test => . body) clause
             (declare (ignore =>))
             (cond ((null body) (error "Missing clause"))
                   ((single body)
                    (values test (car body)))
                   (t (destructuring-bind (var . body) body
                        (let ((fn `(lambda (,var) ,@body)))
                          (values test fn))))))))
    ;; Note that we expand into `cond' rather than `if' so we don't
    ;; have to handle tests without bodies.
    (cond ((null clauses) nil)
          ((member-if #'send-clause? clauses)
           (let* ((tail (member-if #'send-clause? clauses))
                  (preceding (ldiff clauses tail))
                  (clause (car tail))
                  (clauses (cdr tail)))
             (multiple-value-bind (test fn)
                 (parse-send-clause clause)
               (with-gensyms (tmp)
                 `(cond ,@preceding
                        (t (if-let (,tmp ,test)
                             (funcall ,fn ,tmp)
                             (bcond ,@clauses))))))))
          (t `(cond ,@clauses)))))

(defmacro case-let ((var expr) &body cases)
  "Like (let ((VAR EXPR)) (case VAR ...))"
  `(let ((,var ,expr))
     (case ,var
       ,@cases)))

(defmacro ecase-let ((var expr) &body cases)
  "Like (let ((VAR EXPR)) (ecase VAR ...))"
  `(let ((,var ,expr))
     (case ,var
       ,@cases)))

(defmacro comment (&body body)
  "A macro that ignores its body and does nothing. Useful for
comments-by-example.

Also, as noted in EXTENSIONS.LISP of 1992, \"This may seem like a
silly macro, but used inside of other macros or code generation
facilities it is very useful - you can see comments in the (one-time)
macro expansion!\""
  (declare (ignore body)))

(defmacro example (&body body)
  "Like `comment'."
  `(comment ,@body))

(defmacro nix (place &environment env)
  "Set PLACE to nil and return the old value of PLACE.

This may be more efficient than (shiftf place nil), because it only
sets PLACE when it is not already null."
  (multiple-value-bind (vars vals new setter getter)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list vars vals)
            (,(car new) ,getter))
       (and ,(car new)
            (prog1 ,(car new)
              (setq ,(car new) nil)
              ,setter)))))

;;; https://groups.google.com/d/msg/comp.lang.lisp/cyWz2Vyd70M/wYPKr24OEYMJ
(defmacro ensure (place &body newval
                        &environment env)
  "Essentially (or place (setf place newval)).

PLACE is treated as unbound if it returns `nil', signals
`unbound-slot', or signals `unbound-variable'.

Note that ENSURE is `setf'-able, so you can do things like
     (incf (ensure x 0))

Cf. `ensure2'."
  (multiple-value-bind (vars vals stores setter getter)
      (get-setf-expansion place env)
    `(let* ,(mapcar #'list vars vals)
       (or (ignoring (or unbound-slot unbound-variable)
             ,getter)
           (multiple-value-bind ,stores
               (progn ,@newval)
             (when ,(first stores)
               ,setter))))))

(define-setf-expander ensure (place &body newval &environment env)
  (multiple-value-bind (vars vals stores setter getter)
      (get-setf-expansion place env)
    (values vars
            vals
            stores
            setter
            `(or (ignoring (or unbound-slot unbound-variable)
                   ,getter)
                 (progn ,newval)))))

(defmacro ensure2 (place &body newval &environment env)
  "Like `ensure', but specifically for accessors that return a second
value like `gethash'."
  (multiple-value-bind (vars vals stores setter getter)
      (get-setf-expansion place env)
    (with-gensyms (old presentp)
      `(let* ,(mapcar #'list vars vals)
         (multiple-value-bind (,old ,presentp)
             ,getter
           (if ,presentp
               ,old
               (multiple-value-bind ,stores
                   (progn ,@newval)
                 ,setter)))))))

(define-setf-expander ensure2 (place &body newval &environment env)
  (multiple-value-bind (vars vals stores setter getter)
      (get-setf-expansion place env)
    (values vars
            vals
            stores
            setter
            (with-gensyms (old presentp)
              `(multiple-value-bind (,old ,presentp)
                   ,getter
                 (if ,presentp
                     ,old
                     ,newval))))))

(defun thread-aux (threader needle holes thread-fn)
  ;; http://christophe.rhodes.io/notes/blog/posts/2014/code_walking_for_pipe_sequencing/
  (flet ((str= (x y)
           (and (symbolp x) (symbolp y) (string= x y))))
    #+sbcl
    (labels ((find-_ (form env)
               (sb-walker:walk-form form env
                                    (lambda (f c e) (declare (ignore c e))
                                      (cond
                                        ((str= f '_) (return-from find-_ f))
                                        ((eql f form) f)
                                        (t (values f t)))))
               nil)
             (walker (form c env) (declare (ignore c))
               (cond
                 ((symbolp form) (list form))
                 ((atom form) form)
                 (t (if-let (_ (find-_ form env))
                      (values `(let ((,_ ,needle))
                                 ,form)
                              t)
                      (values (funcall thread-fn needle form) t))))))
      (if (not holes)
          needle
          `(,threader ,(sb-walker:walk-form (first holes) nil #'walker)
                      ,@(rest holes))))
    #-sbcl
    (if (not holes)
        needle
        `(,threader ,(let ((hole (first holes)))
                       (if (listp hole)
                           (if-let (_ (find '_ hole :test #'str=))
                             `(let ((,_ ,needle))
                                ,hole)
                             (funcall thread-fn needle hole))
                           `(,hole ,needle)))
                    ,@(rest holes)))))

(defmacro ~> (needle &rest holes)
  "Threading macro from Clojure (by way of Racket).

Thread NEEDLE through HOLES, where each hole is either a
symbol (equivalent to `(hole needle)`) or a list (equivalent to `(hole
needle args...)`).

As an extension, an underscore in the argument list is replaced with
the needle, so you can pass the needle as an argument other than the
first."
  (thread-aux '~> needle holes
              (lambda (needle hole)
                (list* (car hole) needle (cdr hole)))))

(defmacro ~>> (needle &rest holes)
  "Like `~>' but, by default, thread NEEDLE as the last argument
instead of the first."
  (thread-aux '~>> needle holes
              (lambda (needle hole)
                (append1 hole needle))))

(defmacro select (keyform &body clauses)
  "Like `case', but with evaluated keys.

Note that, like `case', `select' interprets a list as the first
element of a clause as a list of keys. To use a form as a key, you
must add an extra set of parentheses.

     (select 2
       ((+ 2 2) t))
     => T

     (select 4
       (((+ 2 2)) t))
     => T

From Zetalisp."
  `(selector ,keyform eql
     ,@clauses))

(defmacro selector (keyform fn &body clauses)
  "Like `select', but compare using FN.

Note that (unlike `case-using'), FN is not evaluated.

From Zetalisp."
  `(select-aux ,keyform cond ,fn ,@clauses))

(defmacro select-aux (keyform cond fn &body clauses)
  (once-only (keyform)
    `(,cond
       ,@(loop for (test . body) in clauses
               collect (if (atom test)
                           `((,fn ,keyform ,test) ,@body)
                           `((or ,@(mapcar (lambda (x) `(,fn ,keyform ,x)) test))
                             ,@body))))))
