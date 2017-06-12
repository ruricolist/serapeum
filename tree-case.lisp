(in-package #:serapeum)

;; http://www.foldr.org/~michaelw/log/programming/lisp/icfp-contest-2006-vm
(define-case-macro tree-case (keyform &body cases)
    (:default default)
  "A variant of `case' optimized for when every key is an integer."
  (let ((keys (mapcar #'first cases)))
    (dolist (key keys)
      (unless (typep key 'integer)
        (error 'type-error
               :datum key
               :expected-type 'integer))))

  (let ((cases (sort (copy-list cases) #'< :key #'first))
        (fail (string-gensym 'fail)))
    (labels ((rec (keyform cases)
               (cond ((null cases)
                      `(,fail))
                     ((single cases)
                      (destructuring-bind ((key . body)) cases
                        `(if (= ,keyform ,key)
                             (progn ,@body)
                             (,fail))))
                     (t (mvlet* ((left-cases right-cases (halves cases))
                                 (split (caar right-cases)))
                          `(if (< ,keyform ,split)
                               ,(rec keyform left-cases)
                               ,(rec keyform right-cases)))))))
      `(flet ((,fail ()
                ,@default))
         ,(rec keyform cases)))))

(define-case-macro tree-ecase (keyform nil &body clauses)
    ()
  "Like `tree-case', but signals an error if KEYFORM does not match
any of the provided cases."
  `(tree-case ,keyform
     ,@clauses
     (otherwise
      (error 'type-error
             :datum ,keyform
             :expected-type '(member ,@(mapcar #'first clauses))))))
