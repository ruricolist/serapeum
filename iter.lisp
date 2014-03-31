(in-package #:serapeum)

(export '(nlet
          collecting
          summing
          minimizing
          maximizing))

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
  `(nlet-2 ,name ,bindings ,@body))

(defmacro nlet-1 (name (&rest bindings) &body body)
  `(funcall
    (named-lambda ,name (mapcar #'first bindings)
      ,@body)
    ,@(mapcar #'second bindings)))

(defmacro nlet-2 (name (&rest bindings) &body body)
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

(defmacro collect1 (collector &body body)
  "Bind COLLECTOR (with MACROLET) as a collector."
  (with-gensyms (head tail)
    `(let* ((,head (list nil))
            (,tail ,head))
       (macrolet ((,collector (&optional (x nil x-p))
                    (if x-p
                        `(setf (cdr ,',tail)
                               (setf ,',tail (list ,x)))
                        `(cdr ,',head))))
         ,@body)
       (the list (cdr ,head)))))

(defmacro collecting* (&body body)
  "Intern COLLECT in the current package and bind it as a collector
with MACROLET."
  (with-syms (collect)
    `(collect1 ,collect
       ,@body)))

(assert (equal '(0 1 2 3 4)
               (collecting*
                 (dotimes (i 5)
                   (collect i)))))

(defmacro collecting (&body body)
  "Within BODY, bind `collect' to a function of one arguments that
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

(defmacro with-collectors* (collectors &body body)
  (if (null collectors)
      `(progn ,@body)
      `(collect1 ,(first collectors)
         (with-collectors* ,(rest collectors)
           ,@body))))

(defmacro with-collectors (collectors &body body)
  (if (endp collectors)
      `(progn ,@body)
      (let ((collector (first collectors)))
        `(collect1 ,collector
           (flet ((,collector (&optional (x nil x?))
                    (if x?
                        (,collector x)
                        (,collector))))
             (declare (inline ,collector))
             (with-collectors ,(rest collectors)
               ,@body))))))

(defmacro hashing ((&rest hash-table-args)
                   &body body)
  "Within BODY, bind HASH to a function that adds its two args to a
hash table; return the hash table at the end."
  (with-syms (hash)
    (with-gensyms (table)
      `(let ((,table (make-hash-table ,@hash-table-args)))
         (flet ((,hash (k v)
                  (setf (gethash k ,table) v)))
           ,@body
           ,table)))))

(defmacro reducing (name fun &body body)
  (with-gensyms (first-run so-far new newp)
    `(block ,name
       (let ((,so-far nil)
             (,first-run t))
         (flet ((,name (&optional (,new nil ,newp))
                  (if ,newp
                      (if ,first-run
                          (setf ,first-run nil
                                ,so-far ,new)
                          (callf ,fun ,so-far ,new))
                      ,so-far)))
           ,@body
           ,so-far)))))

(defmacro folding (name initial fun &body body)
  (with-gensyms (value new newp)
    `(block ,name
       (let ((,value ,initial))
         (flet ((,name (&optional (,new nil ,newp))
                  (if ,newp
                      (callf ,fun ,value ,new)
                      ,value)))
           ,@body
           ,value)))))

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

(defmacro minimizing (&body body)
  "Within BODY, bind `minimize' to a function that tracks its least argument.

Return the minimum."
  (with-syms (minimize)
    `(the number
          (reducing ,minimize #'min
            ,@body))))

(defmacro maximizing (&body body)
  "Within BODY, bind `maximize' to a function that tracks its greatest argument.

Return the maximum."
  (with-syms (maximize)
    `(the number
          (reducing ,maximize #'max
            ,@body))))

(defmacro testing (&body body)
  "Within BODY, bind ALWAYS, NEVER, and THEREIS.

If you mix THEREIS with ALWAYS or NEVER, the return value may not be
what you expect."
  (with-gensyms (ret block)
    (with-syms (always never thereis)
      `(block ,block
         (let ((,ret t))
           (flet ((,always (x)
                    (unless x
                      (return-from ,block nil)))
                  (,never (x)
                    (when x
                      (return-from ,block nil)))
                  (,thereis (x)
                    (if x
                        (return-from ,block x)
                        (setf ,ret nil))))
             (declare (ignorable #',always #',never #',thereis))
             ,@body
             ,ret))))))
