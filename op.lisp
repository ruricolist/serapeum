(in-package :serapeum)
(in-readtable :standard)

(export '(op))

;;;# op

;;; This differs from the original in expecting an extra layer of
;;; parentheses. I find it easier to put the extra parentheses in than
;;; to remember to leave them out. Doing it this way also lets
;;; completion work.

;;; Of course, the extra parentheses make it longer, but the point of
;;; positional lambdas isn't to save typing: it's to save the mental
;;; effort of giving things *names* when all we are interested in is
;;; the *shape* of the code.

;; TODO Handle dotted lists.
(defmacro op (&body body)
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

     (apply (op (+ _*)) '(1 2 3 4)) => 10"
  ;; No `single' yet.
  (let ((counter 0)
        (vars '()))
    (labels ((placeholder? (x &optional env)
               (declare (ignorable env))
               #+sbcl (when (sb-walker:var-lexical-p x env)
                        (return-from placeholder? nil))
               (and (symbolp x)
                    (string= x '_)))
             (numbered-placeholder? (x &optional env)
               (declare (ignorable env))
               #+sbcl (when (sb-walker:var-lexical-p x env)
                        (return-from numbered-placeholder? nil))
               (and (symbolp x)
                    (let ((x (string x)))
                      (and (>= (length x) 2)
                           (string= '_ x :end2 1)
                           (every #'digit-char-p (subseq x 1))))))
             (rest-placeholder? (x &optional env)
               (declare (ignorable env))
               #+sbcl (when (sb-walker:var-lexical-p x env)
                        (return-from rest-placeholder? nil))
               (and (symbolp x)
                    (string= x '_*)))
             (splice (y &optional env)
               (mapcar (lambda (x)
                         (if (rest-placeholder? x env)
                             `(values-list ,x)
                             `(values ,x)))
                       y))
             (rest-op? (x &optional env)
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
                        (some #'rest-op? x))
                   (rest-placeholder? x)))
             (make-var ()
               (let ((var (intern (format nil "_~d" (incf counter)))))
                 (push var vars)
                 var))
             (make-var/numbered (x)
               (let ((n (parse-integer (subseq (string x) 1))))
                 (when (> n counter)
                   (loop repeat (- n counter) do (make-var))))
               x)
             (walk-op (x &optional env)
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
                            `(multiple-value-call (function ,(car f))
                               ,@(splice (cdr f)))
                            t)))
                        (t f))))
               #-sbcl
               (cond ((placeholder? x) (make-var))
                     ((numbered-placeholder? x)
                      (make-var/numbered x))
                     ((listp x)
                      (let ((splice? (some #'rest-placeholder? x))
                            (y (mapcar (lambda (y) (walk-op y)) x)))
                        (if splice?
                            `(multiple-value-call (function ,(car y))
                               ,@(splice (cdr y)))
                            y)))
                     (t x))))
      (let ((body (walk-op `(progn ,@body)))
            (rest (and (rest-op? `(progn ,@body)) `(&rest ,(intern (string '_*))))))
        `(lambda (,@(reverse vars) ,@rest)
           (declare (ignorable ,@vars))
           ,body)))))

;; Constantly.
(assert (= (funcall (op 1)) 1))
;; Identity.
(assert (= (funcall (op _) 1) 1))
;; Positional.
(assert (= (funcall (op (+ 1 _)) 1) 2))
;; Backward reference.
(assert (= (funcall (op (+ _ _1)) 2) 4))
;; Rest.
(assert (equal (apply (op (list 1 _*)) '(2 3)) '(1 2 3)))
;; Positional and rest.
(assert (equal (apply (op (list _ _*)) 1 '(2 3)) '(1 2 3)))
;; Flip
(assert (eql 4 (find '(4) (iota 10) :test (op (member _2 _1)))))
;; nth-arg
(assert (equal '(4 5 6) (mapcar (op _2) '(1 2 3) '(4 5 6))))
;; Sparse argument lists.
(assert (= 9 (apply (op (+ _1 _3 _5)) '(1 2 3 4 5))))
;; Backquotes.
(assert (equal '((:x 1) (:x 2) (:x 3)) (mapcar (op `(:x ,_)) '(1 2 3))))
