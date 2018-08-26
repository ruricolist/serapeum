(in-package #:serapeum)

;;; See
;;; https://www.nextmovesoftware.com/technology/SwitchOptimization.pdf
;;; for ideas on further optimization strategies.

;; http://www.foldr.org/~michaelw/log/programming/lisp/icfp-contest-2006-vm
(define-case-macro tree-case (keyform &body cases)
    (:default default)
  "A variant of `case' optimized for when every key is an integer.

Comparison is done using `eql'."
  ;; Check that all the keys are integers.
  (let ((keys (mapcar #'first cases)))
    (dolist (key keys)
      (unless (typep key 'integer)
        (error 'type-error
               :datum key
               :expected-type 'integer))))
  (let ((cases (sort (copy-list cases) #'< :key #'first))
        (block (string-gensym 'block))
        (fail (string-gensym 'fail)))
    (labels ((rec (keyform cases)
               (if (< (length cases) 4)
                   `(case ,keyform
                      ,@(loop for (key . body) in cases
                              collect `(,key (return-from ,block
                                               (progn ,@body))))
                      (t (go ,fail)))
                   (mvlet* ((left-cases right-cases (halves cases))
                            (split (caar right-cases)))
                     `(if (< ,keyform ,split)
                          ,(rec keyform left-cases)
                          ,(rec keyform right-cases))))))
      `(block ,block
         (tagbody
            ,(rec keyform cases)
            ,fail (return-from ,block
                    (progn ,@default)))))))

(define-case-macro tree-ecase (keyform &body clauses)
    (:error t)
  "Like `tree-case', but signals an error if KEYFORM does not match
any of the provided cases."
  `(tree-case ,keyform
     ,@clauses))

(defun expand-char-case-keys (clauses)
  (loop for (key/s . body) in clauses
        if (stringp key/s)
          collect (cons (coerce key/s 'list) body)
        else collect (cons key/s body)))

(defmacro char-case (keyform &body clauses)
  "Like `case', but specifically for characters.
Expands into `tree-case'.

As an extension to the generalized `case' syntax, the keys of a clause
can be specified as a literal string.

    (defun vowel? (c)
      (char-case c
        (\"aeiouy\" t)))

Signals an error if KEYFORM does not evaluate to a character."
  `(char-case-1 ,keyform
     ,@(expand-char-case-keys clauses)))

(defmacro char-ecase (keyform &body clauses)
  "Like `ecase', but specifically for characters.
Expands into `tree-case'."
  `(char-ecase-1 ,keyform
     ,@(expand-char-case-keys clauses)))

(define-case-macro char-case-1 (keyform &body clauses)
    (:default default)
  `(tree-case (char-code ,keyform)
     ,@(loop for (char . body) in clauses
             collect `(,(char-code char) ,@body))
     (otherwise ,@default)))

(define-case-macro char-ecase-1 (keyform &body clauses)
    (:error t)
  `(char-case ,keyform
     ,@clauses))
