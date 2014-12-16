(in-package :serapeum)
(in-readtable :fare-quasiquote)

(export '(eval-and-compile
          no nor
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
          callf callf2
          ~> ~>>
          cond-every))

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
  (if (null forms)
      t
      `(if ,(first forms)
           nil
           (nor ,@(rest forms)))))

(defun check-exhaustiveness (style type body)
  ;; Should we do redundancy checking? Is there any Lisp that doesn't
  ;; already warn about that?
  (check-type style (member case typecase))
  (labels ((check-subtypep (subtype)
             (unless (subtypep subtype type)
               (warn "~s is not a subtype of ~s" subtype type)))
           (check-exhaustive (partition)
             ;; TODO It would be nice if we could list the types that
             ;; are not matched, or (per OCaml) given an example of a
             ;; value.
             (unless (type= partition type)
               (warn "Non-exhaustive match: ~s is not the same as ~s"
                     partition type)))
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

(defmacro etypecase-of (type x &body body)
  "Like `etypecase' but, at compile time, warn unless each clause in
BODY is a subtype of TYPE, and the clauses in BODY form an exhaustive
partition of TYPE."
  (check-exhaustiveness 'typecase type body)
  (once-only (x)
    `(typecase ,x
       ,@body
       (otherwise
        (error 'type-error
               :datum ,x
               :expected-type ',type)))))

(defmacro ecase-of (type x &body body)
  "Like `ecase' but, given a TYPE (which should be defined as `(member
...)'), warn, at compile time, unless the keys in BODY are all of TYPE
and, taken together, they form an exhaustive partition of TYPE."
  ;; `(etypecase-of ,type ,x
  ;;    ,@ (loop for (cases . clause-body) in body
  ;;             if (eql cases t)
  ;;               collect clause-body
  ;;             else collect
  ;;             `((member ,@(ensure-list cases)) ,@clause-body)))
  (check-exhaustiveness 'case type body)
  (once-only (x)
    `(case ,x
       ,@body
       (otherwise
        (error 'type-error
               :datum ,x
               :expected-type ',type)))))

;;; These are easy to define, but do they make sense?

(defmacro typecase-of (type keyform &body body)
  "Like `etypecase-of', but allow a fallthrough clause starting with
`t' or `otherwise'.

Note that it is still an error if KEYFORM does not satisfy TYPE."
  `(etypecase-of ,type ,keyform
     ,@(loop for clause in body
             for (k . b) = clause
             when (member k '(t otherwise))
               collect `(,type ,@b)
             else collect clause)))

(defmacro case-of (type keyform &body body)
  "Like `ecase-of', but allow a fallthrough clause beginning with `t'
or `otherwise'.

Note that it is still an error if KEYFORM does not satisfy TYPE."
  ;; The only portable way.
  `(etypecase-of ,type ,keyform
     ,@(loop for clause in body
             for (k . b) = clause
             when (member k '(t otherwise))
               collect `(,type ,@b)
             else collect `((member ,@(ensure-list k)) ,@b))))

(defmacro ctypecase-of (type keyplace &body body)
  "Like `etypecase-of', but providing a `store-value' restart to correct KEYPLACE and try again."
  (check-exhaustiveness 'typecase type body)
  `(ctypecase ,keyplace ,@body))

(defmacro ccase-of (type keyplace &body body)
  "Like `ecase-of', but providing a `store-value' restart to correct KEYPLACE and try again."
  (check-exhaustiveness 'case type body)
  `(ccase ,keyplace ,@body))

(defmacro case-using (pred keyform &body clauses)
  "ISLISP's case-using.

     (case-using #'eql x ...)
     ≡ (case x ...).

Note that, no matter the predicate, the keys are not evaluated.

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

;; TODO More informative error.
(defmacro string-ecase (stringform &body cases)
  "Efficient `ecase'-like macro with string keys.

Cf. `string-case'."
  (let* ((cases (normalize-cases cases :allow-default nil))
         (keys (mappend (compose #'ensure-list #'car) cases))
         (err-string (format nil "~~a is not one of ~a" keys)))
    (once-only (stringform)
      (expand-string-case stringform `(error ,err-string ,stringform) cases))))

(assert (eql 'two
             (case-using #'= (+ 1.0 1.0)
               ((1) 'one)
               ((2) 'two)
               (t 'more))))

(assert (eql 2
             (case-using #'string= "bar"
               (("foo") 1)
               (("bar") 2))))

(assert (case-using #'eql 'x
          (x t)
          (t nil)))

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

(assert (null (cond-every)))
(assert (eql (cond-every (t 1) (otherwise 2)) 1))
(assert (eql (cond-every (otherwise 1)) 1))
(assert (eql (cond-every (t 1) (nil 2)) 1))
(assert (eql (let ((x 1)) (cond-every (x) (nil 2))) 1))

(defmacro bcond (&rest clauses)
  "Scheme's extended COND.

This is exactly like COND, except for clauses having the form

     (test :=> recipient)

In that case, if TEST evaluates to a non-nil result, then RECIPIENT, a
function, is called with that result, and the result of RECIPIENT is
return as the value of the `cond`.

The name `bcond' for a “binding cond” goes back at least to the days
of the Lisp Machines. I do not know who was first to use it, but the
oldest examples I have found are by Michael Parker and Scott L.
Burson."
  (flet ((send-clause? (clause)
           (let ((second (second clause)))
             (and (symbolp second)
                  (string= second :=>))))
         (parse-send-clause (clause)
           (destructuring-bind (test => fn . excessive)
               clause
             (declare (ignore =>))
             (when excessive (error "Too many terms in => clause"))
             (values test fn))))
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

(assert
 (select Pi
   (pi t)
   (t nil)))
(assert
 (select 1
   (((- 2 1)) t)))
(assert
 (let ((x 1))
   (select 1
     (x t))))
