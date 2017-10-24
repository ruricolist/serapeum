(in-package :serapeum)
(in-readtable :standard)

;; TODO Handle dotted lists.
(defmacro op (&body body &environment env)
  "GOO's simple macro for positional lambdas.

An OP is like a lambda without an argument list. Within the body of the OP
form, an underscore introduces a new argument.

     (reduce (op (set-intersection _ _ :test #'equal))
             sets)

You can refer back to each argument by number, starting with _1.

     (funcall (op (+ _ _1)) 2) => 4

You can also use positional arguments directly:

     (reduce (op (funcall _2 _1)) ...)

Argument lists can be sparse:

     (apply (op (+ _1 _3 _5)) '(1 2 3 4 5)) => 9

Note that OP with a single argument is equivalent to CONSTANTLY:

     (funcall (op 1)) => 1

and that OP with a single placeholder is equivalent to IDENTITY:

     (funcall (op _) 1) => 1

OP can also be used to define variadic functions by using _* as the
placeholder. It is not necessary to use APPLY.

     (apply (op (+ _*)) '(1 2 3 4)) => 10

Note that OP is intended for simple functions. In particular nested
uses of OP are not supported."
  ;; No `single' yet.
  (let ((counter 0)
        (vars '()))
    (labels ((placeholder? (x env)
               (declare (ignorable env))
               #+sbcl (when (sb-walker:var-lexical-p x env)
                        (return-from placeholder? nil))
               (and (symbolp x)
                    (string= x '_)))
             (numbered-placeholder? (x env)
               (declare (ignorable env))
               #+sbcl (when (sb-walker:var-lexical-p x env)
                        (return-from numbered-placeholder? nil))
               (and (symbolp x)
                    (let ((x (string x)))
                      (and (>= (length x) 2)
                           (string= '_ x :end2 1)
                           (every #'digit-char-p (subseq x 1))))))
             (rest-placeholder? (x env)
               (declare (ignorable env))
               #+sbcl (when (sb-walker:var-lexical-p x env)
                        (return-from rest-placeholder? nil))
               (and (symbolp x)
                    (string= x '_*)))
             (splice (y env)
               (mapcar (lambda (x)
                         (if (rest-placeholder? x env)
                             `(values-list ,x)
                             `(values ,x)))
                       y))
             (quotation? (x env)
               (let ((exp (expand-macro-recursively x env)))
                 (and (listp exp)
                      (eql (car exp) 'quote))))
             (rest-op? (x env)
               (declare (ignorable env))
               #+sbcl
               (prog1 nil
                 (sb-walker:walk-form
                  x env
                  (lambda (f c e)
                    (cond ((and (eql c :eval) (rest-placeholder? f e))
                           (return-from rest-op? t))
                          (t f)))))

               #-sbcl
               (or (and (listp x)
                        (some (rcurry #'rest-op? env) x))
                   (rest-placeholder? x env)))
             (make-var ()
               (let ((var (intern (format nil "_~d" (incf counter)))))
                 (push var vars)
                 var))
             (make-var/numbered (x)
               (let ((n (parse-integer (subseq (string x) 1))))
                 (when (> n counter)
                   (loop repeat (- n counter) do (make-var))))
               x)
             (make-spliced-call (f env)
               (let ((fn (car f)))
                 (if (eql fn 'progn)
                     (make-spliced-call
                      `((lambda (&rest xs)
                          xs)
                        ,@(rest f))
                      env)
                     (let ((splice (splice (cdr f) env)))
                       `(multiple-value-call (function ,fn)
                          ,@splice)))))
             (walk-op (x env)
               (declare (ignorable env))
               #+sbcl
               (sb-walker:walk-form
                x env
                (lambda (f c e)
                  (cond ((not (eql c :eval)) f)
                        ((placeholder? f e)
                         (values (make-var) t))
                        ((numbered-placeholder? f e)
                         (values (make-var/numbered f) t))
                        ((and (listp f) (some (lambda (x) (rest-placeholder? x e)) f))
                         (let ((f (cons (car f) (mapcar (lambda (x) (walk-op x e)) (cdr f)))))
                           (values
                            (make-spliced-call f e)
                            t)))
                        (t f))))
               #-sbcl
               (cond ((quotation? x env) x)
                     ((placeholder? x env) (make-var))
                     ((numbered-placeholder? x env)
                      (make-var/numbered x))
                     ((listp x)
                      (let ((splice? (some (rcurry #'rest-placeholder? env) x))
                            (y (mapcar (rcurry #'walk-op env) x)))
                        (if splice?
                            (make-spliced-call y env)
                            y)))
                     (t x))))
      (declare (ignorable #'quotation?))
      (let ((body (walk-op `(progn ,@body) env))
            (rest
              (and (rest-op? `(progn ,@body) env)
                   `(&rest ,(intern (string '_*))))))
        `(lambda (,@(reverse vars) ,@rest)
           (declare (ignorable ,@vars))
           ,(forbid-nested-op body))))))

(defun forbid-nested-op (form)
  `(locally (declare #+sbcl (sb-ext:disable-package-locks op))
     (macrolet ((op (&rest args)
                  (declare (ignore args))
                  (warn "The OP macro cannot be nested.")))
       (declare #+sbcl (sb-ext:enable-package-locks op))
       ,form)))
