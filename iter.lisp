(defpackage :serapeum/iter
  (:documentation "Iteration constructs and utilities.")
  #+sb-package-locks (:lock t)
  (:use :cl :alexandria)
  (:export
   :collecting
   :nlet
   :summing
   :with-collector
   :with-collectors)
  (:import-from
   :serapeum/macro-tools
   :string-gensym)
  (:import-from
   :tcr.parse-declarations-1.0
   :map-declaration-env
   :parse-declarations
   :build-declarations))
(in-package :serapeum/iter)

;;;# nlet

;;; Some references:
;;; http://common-lisp.net/gitweb?p=users/frideau/ptc.git;a=summary
;;; http://0branch.com/notes/tco-cl.html
;;; https://github.com/rmoritz/trivial-tco
;;; https://groups.google.com/forum/#!msg/sbcl-devel/yRy3u6WiIcw/Ch8xRpQgRhwJ
;;; NB Clisp and ECL only do TCO in compiled code.

;;; At one point `nlet' was much more complicated, using
;;; `macroexpand-damnit' to expand the code and Optima as a code
;;; walker, looking for calls in tail position. This actually worked
;;; well, but it was prohibitively slow.

;;; Another approach would be to use different declarations for
;;; different Lisps to persuade them to do TCO. But for myself whether
;;; I want TCO is totally orthogonal to any other optimization
;;; quality.

(defmacro nlet (name (&rest bindings) &body body &environment env)
  "Within BODY, bind NAME as a function, somewhat like LABELS, but
with the guarantee that recursive calls to NAME will not grow the
stack.

`nlet' resembles Scheme’s named let, and is used for the same purpose:
writing loops using tail recursion. You could of course do this with
`labels' as well, at least under some Lisp implementations, but `nlet'
guarantees tail call elimination anywhere and everywhere.

    (nlet rec ((i 1000000))
      (if (= i 0)
          0
          (rec (1- i))))
    => 0

Beware: because of the way it is written (literally, a GOTO with
arguments), `nlet' is limited: self calls must be tail calls. That is,
you cannot use `nlet' for true recursion.

The name comes from `Let Over Lambda', but this is a more careful
implementation: the function is not bound while the initial arguments
are being evaluated, and it is safe to close over the arguments."
  (setf bindings (loop for binding in bindings
                       if (symbolp binding)
                         collect `(,binding ,binding)
                       else collect binding))
  (multiple-value-bind (body decls)
      (parse-body body)
    (let* ((bindings (mapcar #'ensure-list bindings))
           (vars  (mapcar #'first bindings))
           (inits (mapcar #'second bindings))
           (temps (mapcar #'string-gensym vars))
           (alist (pairlis vars temps))
           (decls
             (map-declaration-env
              (lambda (id names env)
                (values id (sublis alist names) env))
              (parse-declarations decls env))))
      (with-gensyms (tag)
        `(block ,name
           (let ,(mapcar #'list temps inits)
             ,@(build-declarations 'declare decls)
             (macrolet ((,name ,vars
                          `(progn
                             (psetq
                              ,@(mapcan #'list
                                        ',temps
                                        (list ,@vars)))
                             (go ,',tag))))
               (tagbody
                  ,tag (return-from ,name
                         (let ,(mapcar #'list vars temps)
                           ,@body))))))))))

(defmacro with-current-package-symbols (syms &body body)
  "Like `with-gensyms', but binds SYMS in the current package."
  `(let ,(loop for sym in syms
               if (symbolp sym)
                 collect `(,sym (symbolicate ',sym))
               else if (listp sym)
                      collect `(,(first sym) (symbolicate ',(second sym))))
     ,@body))

(defmacro collecting* (&body body)
  "Intern COLLECT in the current package and bind it as a collector
with MACROLET."
  (with-current-package-symbols (collect)
    (with-gensyms (head tail)
      `(let* ((,head (list nil))
              (,tail ,head))
         (macrolet ((,collect (&optional (x nil x-p))
                      (if x-p
                          `(setf (cdr ,',tail)
                                 (setf ,',tail (list ,x)))
                          `(cdr ,',head))))
           ,@body)
         (the list (cdr ,head))))))

(defmacro with-collector ((collector) &body body)
  "Within BODY, bind COLLECTOR to a function of one argument that
accumulates all the arguments it has been called with in order, like
the collect clause in `loop', finally returning the collection.

To see the collection so far, call COLLECTOR with no arguments.

Note that this version binds COLLECTOR to a closure, not a macro: you
can pass the collector around or return it like any other function."
  (with-gensyms (head tail)
    `(let* ((,head (list nil))
            (,tail ,head))
       (declare (ignorable ,tail))
       (flet ((,collector (&rest xs)
                (if xs
                    (dolist (x xs)
                      (setf (cdr ,tail) (setf ,tail (list x))))
                    (cdr ,head))))
         ;; This causes problems in CCL and ABCL.
         #-(or ccl abcl) (declare (inline ,collector))
         ,@body
         (the list (cdr ,head))))))

(defmacro collecting (&body body)
  "Like `with-collector', with the collector bound to the result of
interning `collect' in the current package."
  (with-current-package-symbols (collect)
    `(with-collector (,collect)
       ,@body)))

(defmacro with-collectors ((&rest collectors) &body body)
  "Like `with-collector', with multiple collectors.
Returns the final value of each collector as multiple values.

     (with-collectors (x y z)
       (x 1)
       (y 2)
       (z 3))
     => '(1) '(2) '(3)"
  ;; Warn if there are duplications in COLLECTORS.
  (when (> (length collectors)
           (length (remove-duplicates collectors)))
    (warn "Duplicated collectors in with-collectors form: ~s" collectors))
  (with-gensyms (outer)
    `(block ,outer
       ,(reduce (lambda (form collector)
                  `(with-collector (,collector)
                     ,form))
                collectors
                :initial-value
                `(progn
                   ,@body
                   (return-from ,outer
                     (values ,@(mapcar #'list collectors))))))))

(defmacro summing (&body body)
  "Within BODY, bind `sum' to a function that gathers numbers to sum.

If the first form in BODY is a literal number, it is used instead of 0
as the initial sum.

To see the running sum, call `sum' with no arguments.

Return the total."
  ;; TODO This should be numerically stable, at least if the zero is a
  ;; float.
  (let ((zero (if (numberp (first body)) (pop body) 0)))
    (with-gensyms (running-total x x-supplied?)
      (with-current-package-symbols (sum)
        `(let ((,running-total ,zero))
           (declare (number ,running-total))
           (flet ((,sum (&optional (,x 0 ,x-supplied?))
                    (if ,x-supplied?
                        (incf ,running-total ,x)
                        ,running-total)))
             (declare (ignorable (function ,sum)))
             ,@body)
           (the number ,running-total))))))
