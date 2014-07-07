(in-package #:serapeum)

(export '(nlet collecting summing))

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

(defmacro nlet (name (&rest bindings) &body body)
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

The name comes from `Let Over Lambda', but this is a different
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
           (decls (loop for decl in (parse-declarations decls)
                        for id = (car decl)
                        for temp = (assoc-value alist id)
                        if temp
                          collect (cons temp (cdr decl))
                        else collect decl)))
      (with-gensyms (tag)
        `(block ,name
           (let ,(mapcar #'list temps inits)
             ,@(mapcar #'expand-declaration decls)
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

;;; Make sure the variables can be closed over.
(assert (equal '(3 2 1)
               (mapcar 'funcall
                       (nlet rec ((i 3)
                                  (acc nil))
                         (if (= i 0)
                             (nreverse acc)
                             (rec (1- i) (cons (lambda () i) acc)))))))

(defmacro with-syms (syms &body body)
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
  (with-syms (collect)
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

(assert (equal '(0 1 2 3 4)
               (collecting*
                 (dotimes (i 5)
                   (collect i)))))

(defmacro collecting (&body body)
  "Within BODY, bind `collect' to a function of one argument that
accumulate all the arguments it has been called with in order, like
the collect clause in `loop', finally returning the collection.

To see the collection so far, call `collect' with no arguments.

Note that this version of `collecting' binds `collect' to a closure,
not a macro: you can pass the collector around or return it like any
other function."
  (with-syms (collect)
    `(collecting*
       (flet ((,collect (&optional (x nil x?))
                (if x?
                    (,collect x)
                    (,collect))))
         (declare (inline ,collect))
         ,@body))))

(defmacro summing (&body body)
  "Within BODY, bind `sum' to a function that gathers numbers to sum.

Return the total."
  (with-gensyms (n x)
    (with-syms (sum)
      `(the number
            (let ((,n 0))
              (declare (number ,n))
              (flet ((,sum (,x)
                       (incf ,n ,x)))
                (declare (ignorable (function ,sum)))
                ,@body)
              ,n)))))
