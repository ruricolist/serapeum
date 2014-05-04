(in-package :serapeum)
(in-readtable :fare-quasiquote)

(export '(eval-and-compile
          no nor
          string-case string-ecase
          case-using
          select selector
          econd econd-failure
          econd-let ecase-let
          cond-let case-let
          comment example
          nix
          ensure ensure2
          callf callf2
          ~> ~>>
          cond-every
          atomic))

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

(defmacro case-using (pred keyform &body clauses)
  "ISLISP's case-using.

     (case-using #'eql x ...)
     ≡ (case x ...).

Note that, no matter the predicate, the keys are not evaluated.

This version supports both single-item clauses (x ...) and
multiple-item clauses ((x y) ...), as well as (t ...) for the default
clause."
  (case (extract-function-name pred)
    (eql `(case ,keyform ,@clauses))
    (string= `(string-case ,keyform ,@clauses))
    #+ () ((eq equal equalp) `(hash-case ,keyform ,pred ,@clauses))
    #+ () (= `(tree-case ,keyform ,@clauses))
    (t (once-only (keyform)
         (rebinding-functions (pred)
           `(case-using-aux ,pred ,keyform ,@clauses))))))

(defmacro case-using-aux (pred keyform &body clauses)
  (if (not clauses)
      nil
      (destructuring-bind ((key . body) . clauses) clauses
        (if (or (eql key t)
                (eql key 'otherwise))
            `(progn ,@body)
            `(if (or ,@(mapcar (lambda (key)
                                 `(funcall ,pred ,keyform ',key))
                               (ensure-list key)))
                 (progn ,@body)
                 (case-using-aux ,pred ,keyform
                   ,@clauses))))))

(eval-and-compile
  (defun expand-string-case (cases)
    (loop for (keys . body) in cases
          if (or (stringp keys) (eql keys t))
            collect (cons keys body)
          else if (eql keys 'otherwise)
                 collect (cons t body)
          else if (and (consp keys)
                       (every #'stringp keys))
                 append (loop for case in keys
                              collect (cons case body)))))

(defmacro string-case (stringform &body cases)
  "Efficient `case'-like macro with string keys.

This uses Paul Khuong's `string-case' macro internally."
  `(string-case:string-case (,stringform :default nil)
     ,@(expand-string-case cases)))

;; TODO More informative error.
(defmacro string-ecase (stringform &body cases)
  "Efficient `ecase'-like macro with string keys.

Cf. `string-case'."
  (let* ((keys (mappend (compose #'ensure-list #'car) cases))
         (error (format nil "~~a is not one of ~a" keys)))
    (once-only (stringform)
      `(string-case:string-case (,stringform
                                 :default (error ,error ,stringform))
         ,@(expand-string-case cases)))))

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
                             (,any (or ,any ,sat)))
                        (declare (ignorable ,ret ,any))
                        ,(expand (rest clauses)))))))
      `(let ((,any nil)
             (,ret nil))
         (declare (ignorable ,ret ,any))
         ,(expand clauses)))))

(assert (null (cond-every)))
(assert (eql (cond-every (t 1) (otherwise 2)) 1))
(assert (eql (cond-every (otherwise 1)) 1))
(assert (eql (cond-every (t 1) (nil 2)) 1))
(assert (eql (let ((x 1)) (cond-every (x) (nil 2))) 1))

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

(defmacro ~> (needle &rest holes)
  "Threading macro from Clojure (by way of Racket).

Thread NEEDLE through HOLES, where each hole is either a
symbol (equivalent to `(hole needle)`) or a list (equivalent to `(hole
needle args...)`).

As an extension, an underscore in the argument list is replaced with
the needle, so you can pass the needle as an argument other than the
first."
  (flet ((str= (x y)
           (ignore-errors (string= x y))))
    (if (not holes)
        needle
        `(~> ,(let ((hole (first holes)))
                (if (listp hole)
                    (if (find '_ hole :test #'str=)
                        `(,(car hole) ,@(substitute needle '_ (cdr hole) :test #'str=))
                        `(,(car hole) ,needle ,@(cdr hole)))
                    `(,hole ,needle)))
             ,@(rest holes)))))

(defmacro ~>> (needle &rest holes)
  "Like `~>' but, by default, thread NEEDLE as the last argument
instead of the first."
  `(~> ,needle
       ,@(loop for hole in holes
               collect (if (listp hole)
                           (if (find '_ hole
                                     :test (lambda (x y)
                                             (and (typep x 'string-designator)
                                                  (typep y 'string-designator)
                                                  (string= x y))))
                               hole
                               (append hole '(_)))
                           (list hole '_)))))

(defmacro select (keyform &body clauses &environment env)
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

This version of `select' is smart enough to compile itself to a `case'
statement if all its keys are constants.

From Zetalisp."
  (multiple-value-bind (cases default)
      (normalize-cases clauses)
    (let ((keys (mapcar (lambda (key)
                          (expand-macro key env))
                        (mappend #'car cases))))
      (if (every (rcurry #'constant? env) keys)
          `(case ,keyform
             ,@(loop for (keys . body) in cases
                     collect (cons (mapcar (rcurry #'eval-constant env) keys)
                                   body))
             ,@(when default
                 (unsplice `(otherwise ,default))))
          `(selector ,keyform eql
             ,@clauses)))))

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

(defmacro atomic (&body body)
  "Run BODY as an anonymous critical section.
Only one thread can run BODY at a time.

From Arc.
"
  (with-gensyms (lock)
    `(let ((,lock (load-time-value (bt:make-lock "Anonymous critical section"))))
       (bt:with-lock-held (,lock)
         ,@body))))
