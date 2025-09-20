(defpackage :serapeum/control-flow
  (:documentation "Control flow macros.")
  #+sb-package-locks (:lock t)
  #+sb-package-locks (:implement :serapeum/control-flow :serapeum/types)
  (:use
   :cl
   :alexandria
   :serapeum/binding
   :serapeum/macro-tools
   :serapeum/portability
   :serapeum/types
   :trivia)
  (:import-from
   :introspect-environment
   :typexpand)
  (:import-from
   :serapeum/binding
   :let1)
  (:import-from
   :serapeum/macro-tools
   :extract-function-name
   :rebinding-functions)
  (:export
   :bcond
   :case-let
   :case-of
   :case-using
   :ccase-let
   :ccase-of
   :comment
   :cond-every
   :cond-let
   :ctypecase-let
   :ctypecase-of
   :destructuring-case-of
   :destructuring-ccase-of
   :destructuring-ecase-of
   :ecase-let
   :ecase-of
   :ecase-using
   :econd
   :econd-failure
   :econd-let
   :eif
   :eif-let
   :ensure
   :ensure2
   :eq* #:eql* #:equal* #:equalp*
   :etypecase-let
   :etypecase-of
   :eval-always
   :eval-and-compile
   :example
   :nand
   :nest
   :nix
   :no
   :nor
   :null-if
   :recursion-forbidden
   :select
   :selector
   :sort-values
   :string-case
   :string-ecase
   :typecase-let
   :typecase-of
   :without-recursion
   :~>
   :~>>))
(in-package :serapeum/control-flow)

(defmacro eval-always (&body body)
  "Shorthand for
        (eval-when (:compile-toplevel :load-toplevel :execute) ...)"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro eval-and-compile (&body body)
  "Emacs's `eval-and-compile'.
Alias for `eval-always'."
  `(eval-always
     ,@body))

(defun no (x)
  "Another alias for `not' and `null'.

From Arc."
  (not x))

(define-compiler-macro no (x)
  `(not ,x))

(declaim (inline null-if))
(defun null-if (arg1 arg2 &key (test #'eql))
  "Return nil if arguments are equal under TEST, ARG1 otherwise.
Return a second value of nil if the arguments were equal, T
otherwise.

From SQL."
  (if (funcall test arg1 arg2)
      (values nil nil)
      (values arg1 t)))

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

(defun same-type? (type1 type2 &optional env)
  "Like `alexandria:type=', but takes an environment."
  (multiple-value-bind (sub sure) (subtypep type1 type2 env)
    (if (not sub)
        (values nil sure)
        (subtypep type2 type1 env))))

(defun explode-type (type env)
  "Extract the individual types from a type defined as a disjunction.
TYPE could be the actual disjunction, or the name of the type.

If TYPE is not a disjunction, just return TYPE as a list.

Note that `member' types are disjunctions:

    (explode-type (member :x :y :z) nil)
    => ((eql :x) (:eql y) (eql :z))"
  (labels ((explode-type (type)
             (match type
               ((list* 'or subtypes)
                (mappend #'explode-type subtypes))
               ((list* 'member subtypes)
                ;; Remember (member) is also the bottom type.
                (if (null subtypes) (list nil)
                    (mappend #'explode-type
                             (loop for subtype in subtypes
                                   collect `(eql ,subtype)))))
               (otherwise (list type)))))
    (explode-type (typexpand type env))))

(defun describe-non-exhaustive-match (stream partition type env)
  (assert (not (same-type? partition type)))
  (labels ((extra-types (partition)
             (loop for subtype in (explode-type partition env)
                   unless (subtypep subtype type env)
                     collect subtype))

           (format-extra-types (stream partition)
             (when-let (et (extra-types partition))
               (format stream "~&There are extra types: ~s" et)))

           (missing-types (partition)
             (remove-if (lambda (type)
                          (subtypep type partition env))
                        (explode-type type env)))

           (format-missing-types (stream partition)
             (when-let (mt (missing-types partition))
               (format stream "~&There are missing types: ~s" mt)))

           (format-subtype-problem (stream partition)
             (cond ((subtypep partition type env)
                    (format stream "~s is a proper subtype of ~s." partition type))
                   ((subtypep type partition env)
                    (format stream "~s contains types not in ~s." partition type))
                   (t (format stream "~s is not the same as ~s" partition type)))))

    (format stream "~&Non-exhaustive match: ")
    (format-subtype-problem stream partition)
    (format-extra-types stream partition)
    (format-missing-types stream partition)))

(defun check-exhaustiveness (style type clauses env)
  ;; Should we do redundancy checking? Is there any Lisp that doesn't
  ;; already warn about that?
  (check-type style (member case typecase))
  (multiple-value-bind (clause-types partition)
      (ecase style
        ((typecase)
         (loop for (type . nil) in clauses
               collect type into clause-types
               finally (return (values clause-types
                                       `(or ,@clause-types)))))
        ((case)
         (loop for (key-spec . nil) in clauses
               for keys = (ensure-list key-spec)
               for clause-type = `(member ,@keys)
               collect clause-type into clause-types
               append keys into all-keys
               finally (return (values clause-types
                                       `(member ,@all-keys))))))
    ;; Check that every clause type is a subtype of TYPE.
    (dolist (clause-type clause-types)
      (multiple-value-bind (subtype? sure?)
          (subtypep clause-type type env)
        (cond ((not sure?)
               (warn "Can't tell if ~s is a subtype of ~s.~@[Is ~s defined?~]"
                     clause-type type
                     ;; Only warn about definition when the type could
                     ;; be defined.
                     (and (symbolp type)
                          type)))
              ((not subtype?)
               (warn "~s is not a subtype of ~s" clause-type type)))))
    ;; Check that the clause types form an exhaustive partition of TYPE.
    (multiple-value-bind (same sure)
        (same-type? partition type env)
      (cond ((not sure)
             (warn "Can't check exhaustiveness: cannot determine if ~s is the same as ~s"
                   partition type))
            (same)
            (t (warn "~a"
                     (with-output-to-string (s)
                       (describe-non-exhaustive-match s partition type env)))))))
  (values))

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
  "Like `case' but may, and must, have an `otherwise' clause."
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

;;; Adapted from Alexandria.
(defun expand-destructuring-case-of (type key clauses case-of)
  (once-only (key)
    `(if (typep ,key 'cons)
         (,case-of ,type (car ,key)
           ,@(mapcar (lambda (clause)
                       (destructuring-bind ((keys . lambda-list) &body body) clause
                         `(,keys
                           (destructuring-bind ,lambda-list (cdr ,key)
                             ,@body))))
                     clauses))
         (error "Invalid key to DESTRUCTURING-~S: ~S" ',case-of ,key))))

(defmacro destructuring-ecase-of (type expr &body body)
  "Like `destructuring-ecase', from Alexandria, but with exhaustivness
checking.

TYPE is a designator for a type, which should be defined as `(member
...)'. At compile time, the macro checks that, taken together, the
symbol at the head of each of the destructuring lists in BODY form an
exhaustive partition of TYPE, and warns if it is not so."
  (expand-destructuring-case-of type expr body 'ecase-of))

(defmacro destructuring-case-of (type expr &body body)
  "Like `destructuring-ecase-of', but an `otherwise' clause must also be supplied.

Note that the otherwise clauses must also be a list:

    ((otherwise &rest args) ...)"
  (expand-destructuring-case-of type expr body 'case-of))

(defmacro destructuring-ccase-of (type keyplace &body body)
  "Like `destructuring-case-of', but providing a `store-value' restart
to collect KEYPLACE and try again."
  (expand-destructuring-case-of type keyplace body 'ccase-of))

(defmacro case-using (pred keyform &body clauses)
  "ISLISP's case-using.

     (case-using #'eql x ...)
     ≡ (case x ...).

Note that, no matter the predicate, the keys are not evaluated. (But
see `selector'.)

The PRED form is evaluated.

When PRED is invoked, KEYFORM is its first argument. You can use
`flip' if you want the arguments passed the other way around. For
example, to dispatch on potential elements of a list:

    (case-using list (flip #'member)
      (:item1 ...))

This version supports both single-item clauses (x ...) and
multiple-item clauses ((x y) ...), as well as (t ...) or (otherwise
...) for the default clause."
  (case (extract-function-name pred)
    (eql
     `(case ,keyform
        ,@clauses))
    ;; TODO Check that this isn't rebound in the environment. (Not a
    ;; concern on SBCL as the package is locked there.)
    (string=
     `(string-case ,keyform
        ,@clauses))
    (t `(case-using-aux ,pred ,keyform
          ,@clauses))))

(define-case-macro case-using-aux (pred keyform &body clauses)
    (:default default)
  (once-only (keyform)
    (rebinding-functions (pred)
      `(cond ,@(loop for (key . body) in clauses
                     collect `((funcall ,pred ,keyform ',key)
                               ,@body))
             (t ,@default)))))

(defmacro ecase-using (pred keyform &body clauses)
  "Exhaustive variant of `case-using'."
  (once-only (keyform)
    `(case-using ,pred ,keyform
       ,@clauses
       (otherwise
        (error "~s fell through ~a with ~s"
               ,keyform
               'ecase-using
               ',pred)))))

(define-case-macro string-case (stringform &body clauses)
    (:default default)
  "Efficient `case'-like macro with string keys.

Note that string matching is always case-sensitive.

This uses Paul Khuong's `string-case' macro internally."
  (let ((keys (mapcar #'first clauses)))
    (if (every (lambda (key)
                 (and (stringp key)
                      (= (length key) 1)))
               keys)
        (with-unique-names (block)
          `(block ,block
             (and (= (length ,stringform) 1)
                  (case (aref ,stringform 0)
                    ,@(loop for (key . body) in clauses
                            collect `(,(aref key 0)
                                      (return-from ,block
                                        (progn ,@body))))))
             ,@default))
        `(string-case:string-case
             (,stringform
              :default (progn ,@default))
           ,@clauses))))

(defun string-case-failure (expr keys)
  (error "~s is not one of ~s"
         expr
         keys))

(define-case-macro string-ecase (stringform &body clauses)
    (:error string-case-failure)
  "Efficient `ecase'-like macro with string keys.

Note that string matching is always case-sensitive.

Cf. `string-case'."
  `(string-case ,stringform
     ,@clauses))

(defmacro if-let1 (var test &body (then else))
  `(let1 ,var ,test
     (if ,var
         ,then
         ,else)))

(defmacro eif (&whole whole test then &optional (else nil else?))
  "Like `cl:if', but expects two branches.

If there is only one branch a warning is signaled.

This macro is useful when writing explicit decision trees; it will
warn you if you forget a branch.

Short for “exhaustive if”."
  (unless else?
    (warn "Missing else-branch in eif form:~%~a"
          whole))
  `(if ,test ,then ,else))

(defmacro eif-let (&whole whole binds &body (then &optional (else nil else?)))
  "Like `alexandria:if-let', but expects two branches.
Compare `eif'."
  (unless else?
    (warn "Missing else-branch in eif-let form:~%~a"
          whole))
  `(if-let ,binds ,then ,else))

(define-condition econd-failure (error)
  ((test-count :type (integer 0 *) :initarg :test-count))
  (:default-initargs :test-count 0)
  (:report (lambda (c s)
             (with-slots (test-count) c
               (format s "~s fell through after ~d failed test~:p."
                       'econd test-count))))
  (:documentation "An ECOND failed."))

(defmacro econd (&rest clauses)
  "Like `cond', but signal an error of type `econd-failure' if no
clause succeeds."
  (let ((test-count (length clauses)))
    `(cond ,@clauses
           ;; SBCL will silently eliminate this branch if it is
           ;; unreachable.
           (t (error 'econd-failure :test-count ',test-count)))))

(defmacro cond-let (var &body clauses)
  "Cross between COND and LET.

     (cond-let x ((test ...)))
     ≡ (let (x)
         (cond ((setf x test) ...)))

Cf. `acond' in Anaphora."
  (match clauses
    (() nil)
    ((list* (list test) clauses)
     `(if-let1 ,var ,test
        ,var
        (cond-let ,var ,@clauses)))
    ((list* (list* t body) _)
     `(progn ,@body))
    ((list* (list* test body) clauses)
     `(let ((,var ,test))
        (if ,var
            ;; Rebind the variables for declarations.
            (let1 ,var ,var
              ,@body)
            (cond-let ,var ,@clauses))))))

(defmacro econd-let (symbol &body clauses)
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

Note that this does *not* do the same thing as a series of `when'
forms: `cond-every' evaluates *all* the tests *before* it evaluates
any of the forms.

From Zetalisp."
  (let* ((otherwise-clause (find 'otherwise clauses :key #'car))
         (test-clauses (remove otherwise-clause clauses))
         (temps (make-gensym-list (length test-clauses))))
    `(let* ,(loop for temp in temps
                  for (test . nil) in test-clauses
                  collect `(,temp ,test))
       ;; Work around Iterate bug. See
       ;; <https://gitlab.common-lisp.net/iterate/iterate/-/issues/11>.
       (if (or ,@temps)
           ,(with-gensyms (ret)
              `(let (,ret)
                 ,@(loop for temp in temps
                         for (nil . body) in test-clauses
                         collect `(when ,temp
                                    (setf ,ret
                                          ,(if (null body)
                                               temp
                                               `(progn ,@body)))))
                 ,ret))
           (progn ,@(rest otherwise-clause))))))

(defmacro bcond (&body clauses)
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
                   ((null (rest body))
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
                        (t (if-let1 ,tmp ,test
                             (funcall ,fn ,tmp)
                             (bcond ,@clauses))))))))
          (t `(cond ,@clauses)))))

(defmacro case-let ((var expr) &body cases)
  "Like (let ((VAR EXPR)) (case VAR ...)), with VAR read-only."
  `(let1 ,var ,expr
     (case ,var
       ,@cases)))

(defmacro ccase-let ((var expr) &body cases)
  "Like (let ((VAR EXPR)) (ccase VAR ...)), with VAR correctable."
  `(let ((,var ,expr))
     (ccase ,var
       ,@cases)))

(defmacro ecase-let ((var expr) &body cases)
  "Like (let ((VAR EXPR)) (ecase VAR ...)), with VAR read-only."
  `(let1 ,var ,expr
     (ecase ,var
       ,@cases)))

(defmacro typecase-let ((var expr) &body cases)
  "Like (let ((VAR EXPR)) (typecase VAR ...)), with VAR read-only."
  `(let1 ,var ,expr
     (typecase ,var
       ,@cases)))

(defmacro ctypecase-let ((var expr) &body cases)
  "Like (let ((VAR EXPR)) (ctypecase VAR ...)), with VAR correctable."
  `(let ((,var ,expr))
     (ctypecase ,var
       ,@cases)))

(defmacro etypecase-let ((var expr) &body cases)
  "Like (let ((VAR EXPR)) (etypecase VAR ...)), with VAR read-only."
  `(let1 ,var ,expr
     (etypecase ,var
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

(defmacro nix-1 (place &environment env)
  (multiple-value-bind (vars vals new setter getter)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list vars vals)
            (,(car new) ,getter))
       (and ,(car new)
            (prog1 ,(car new)
              (setq ,(car new) nil)
              ,setter)))))

(defmacro nix (&rest places)
  "Set PLACES to nil and return the old value(s) of PLACES.

If there is more than one PLACE, return their old values as multiple values.

This may be more efficient than (shiftf place nil), because it only
sets PLACE when it is not already null."
  `(values
    ,@(loop for place in places
            collect `(nix-1 ,place))))

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
       (or (ignore-some-conditions (unbound-slot unbound-variable)
             ,getter)
           (multiple-value-bind ,stores
               (progn ,@newval)
             (when ,(first stores)
               ,setter))))))

(define-setf-expander ensure (place &rest newval &environment env)
  (multiple-value-bind (vars vals stores setter getter)
      (get-setf-expansion place env)
    (values vars
            vals
            stores
            setter
            `(or (ignore-some-conditions (unbound-slot unbound-variable)
                   ,getter)
                 (progn ,@newval)))))

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

(define-setf-expander ensure2 (place &rest newval &environment env)
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
                     (progn ,@newval)))))))

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
                      (values `(let1 ,_ ,needle
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
                             `(let1 ,_ ,needle
                                ,hole)
                             (funcall thread-fn needle hole))
                           `(,hole ,needle)))
                    ,@(rest holes)))))

(defun check-no-underscores (holes)
  (loop for hole in holes
        when (find '_ (ensure-list hole))
          do (error "Arrow macros with underscores cannot be used as patterns: ~a" hole)))

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

(defpattern ~> (needle &rest holes)
  (check-no-underscores holes)
  (macroexpand-1 `(~> ,needle ,@holes)))

(defmacro ~>> (needle &rest holes)
  "Like `~>' but, by default, thread NEEDLE as the last argument
instead of the first."
  (thread-aux '~>> needle holes
              (lambda (needle hole)
                (append hole (list needle)))))

(defpattern ~>> (needle &rest holes)
  (check-no-underscores holes)
  (macroexpand-1 `(~>> ,needle ,@holes)))

(defun expand-nest (things)
  "Helper function for `nest'."
  (reduce (lambda (outer inner)
            (let ((outer (ensure-list outer)))
              `(,@outer ,inner)))
          things
          :from-end t))

(defmacro nest (&rest things)
  "Like ~>>, but backward.

This is useful when layering `with-x' macros where the order is not
important, and extra indentation would be misleading.

For example:

    (nest
     (with-open-file (in file1 :direction input))
     (with-open-file (in file2 :direction output))
     ...)

Is equivalent to:

    (with-open-file (in file1 :direction input)
      (with-open-file (in file2 :direction output)
        ...))

\(But see `with-open-files').

If the outer macro has no arguments, you may omit the parentheses.

    (nest
      with-standard-io-syntax
      ...)
    ≡ (with-standard-io-syntax
        ...)

From UIOP, based on a suggestion by Marco Baringer."
  (expand-nest things))

(defpattern nest (&rest things)
  (expand-nest things))

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

(define-case-macro selector (keyform fn &body clauses)
    (:default default)
  "Like `select', but compare using FN.

Note that (unlike `case-using'), FN is not evaluated.

Prefer `selector' to `case-using' when FN is a macro, or has a
compiler macro.

From Zetalisp."
  `(cond
     ,@(loop for (test . body) in clauses
             collect `((,fn ,keyform ,test)
                       ,@body))
     (t ,@default)))

(defparameter *sorting-networks*
  '((2
     (0 1))
    (3
     (1 2)
     (0 2)
     (0 1))
    (4
     (0 1)
     (2 3)
     (0 2)
     (1 3)
     (1 2))
    (5
     (0 1)
     (3 4)
     (2 4)
     (2 3)
     (0 3)
     (0 2)
     (1 4)
     (1 3)
     (1 2))
    (6
     (1 2)
     (0 2)
     (0 1)
     (4 5)
     (3 5)
     (3 4)
     (0 3)
     (1 4)
     (2 5)
     (2 4)
     (1 3)
     (2 3))
    (7
     (1 2)
     (0 2)
     (0 1)
     (3 4)
     (5 6)
     (3 5)
     (4 6)
     (4 5)
     (0 4)
     (0 3)
     (1 5)
     (2 6)
     (2 5)
     (1 3)
     (2 4)
     (2 3))
    (8
     (0 1)
     (2 3)
     (0 2)
     (1 3)
     (1 2)
     (4 5)
     (6 7)
     (4 6)
     (5 7)
     (5 6)
     (0 4)
     (1 5)
     (1 4)
     (2 6)
     (3 7)
     (3 6)
     (2 4)
     (3 5)
     (3 4)))
  "Sorting networks for 2 to 8 elements.")

(defun sorting-network (size)
  (check-type size (integer 2 *))
  (or (cdr (assoc size *sorting-networks*))
      (error "No sorting network of size ~d" size)))

(defmacro sort-values/network (pred &rest values)
  (with-gensyms (swap)
    `(macrolet ((,swap (x y)
                  `(unless (funcall ,',pred ,x ,y)
                     (rotatef ,x ,y))))
       ,(let ((network (sorting-network (length values))))
          (assert network)
          (let ((temps (make-gensym-list (length values))))
            `(let ,(mapcar #'list temps values)
               ,@(loop for (x y) in network
                       collect `(,swap ,(nth x temps) ,(nth y temps)))
               (values ,@temps)))))))

(defmacro sort-values/temp-vector (pred &rest values)
  (with-gensyms (temp)
    `(let ((,temp (make-array ,(length values))))
       (declare (dynamic-extent ,temp))
       ,@(loop for i from 0
               for v in values
               collect `(setf (svref ,temp ,i) ,v))
       ;; Keep compiler quiet.
       (setf ,temp (sort ,temp ,pred))
       (values ,@(loop for i from 0
                       for nil in values
                       collect `(svref ,temp ,i))))))

(defmacro sort-values (pred &rest values)
  "Sort VALUES with PRED and return as multiple values.

Equivalent to

    (values-list (sort (list VALUES...) pred))

But with less consing, and potentially faster."
  ;; Remember to evaluate `pred' no matter what.
  (with-gensyms (gpred)
    `(let ((,gpred (ensure-function ,pred)))
       (declare (ignorable ,gpred)
                (function ,gpred)
                (optimize (safety 1) (debug 0) (compilation-speed 0)))
       ,(match values
          ((list) `(values))
          ((list x) `(values ,x))
          ;; The strategy here is to use a sorting network if the
          ;; inputs are few, and a stack-allocated vector if the
          ;; inputs are many.
          (otherwise
           (if (<= (length values) 8)
               `(sort-values/network ,gpred ,@values)
               `(sort-values/temp-vector ,gpred ,@values)))))))

(defun expand-variadic-equality (binary variadic args)
  (match args
    (() t)
    ((list x) `(progn ,x t))
    ((list x y) `(,binary ,x ,y))
    ;; It might be worth special-casing the three-argument case; of
    ;; all the variadic cases it is likely to be the most common by
    ;; far.
    ((list x y z)
     (once-only (x y z)
       `(and (,binary ,x ,y)
             (,binary ,y ,z))))
    (otherwise
     ;; Remember that we need to evaluate all of the arguments,
     ;; always, and in left-to-right order.
     (let ((temps (make-gensym-list (length args))))
       (destructuring-bind (x y . zs) temps
         `(let ,(mapcar #'list temps args)
            (and (,binary ,x ,y)
                 (,variadic ,y ,@zs))))))))

(defmacro define-variadic-equality (variadic binary)
  `(progn
     (defun ,variadic (&rest xs)
       ,(format nil "Variadic version of `~(~a~)'.

With no arguments, return T.

With one argument, return T.

With two arguments, same as `~:*~(~a~)'.

With three or more arguments, return T only if all of XS are
equivalent under `~:*~(~a~)'.

Has a compiler macro, so there is no loss of efficiency relative to
writing out the tests by hand."
                binary)
       (match xs
         (() t)
         ((list _) t)
         ((list x y) (,binary x y))
         (otherwise (every #',binary xs (rest xs)))))
     (define-compiler-macro ,variadic (&rest xs)
       (expand-variadic-equality ',binary ',variadic xs))))

(define-variadic-equality eq* eq)

(define-variadic-equality eql* eql)

(define-variadic-equality equal* equal)

(define-variadic-equality equalp* equalp)

(define-condition recursion-forbidden (error)
  ((form :initarg :form))
  (:report (lambda (c s)
             (with-slots (form) c
               (format s "Forbidden recursion in form:~%~a" form)))))

(defvar *recursions* nil)

(defmacro without-recursion ((&key) &body body)
  "If BODY calls itself, at any depth, signal a (continuable) error of
type `recursion-forbidden'."
  (with-unique-names (recursion-id)
    `(progn
       (when (member ',recursion-id *recursions*)
         (cerror "Recurse anyway."
                 'recursion-forbidden
                 :form ',body))
       (let ((*recursions* (cons ',recursion-id *recursions*)))
         (declare (dynamic-extent *recursions*))
         ,@body))))
