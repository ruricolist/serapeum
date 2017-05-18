(in-package #:serapeum)

;; http://www.foldr.org/~michaelw/log/programming/lisp/icfp-contest-2006-vm
(defmacro tree-case (keyform &body cases)
  "A variant of `case' optimized for when every key is an integer."
  (local
    (defun case-key (case)
      (first case))

    (defun case-body (case)
      (rest case))

    (def defaults (keep 'cl:otherwise cases :key #'case-key))

    (when (rest defaults)
      (warn "Multiple default clauses: ~a" defaults))

    (def default (first defaults))

    (setf cases (remove default cases))

    (def bodies (mapcar #'case-body cases))

    (def default-body (case-body default))

    (def tags (make-gensym-list (length bodies) 'case))

    (setf cases
          (loop for case in cases
                for key = (case-key case)
                for tag in tags
                if (listp key)
                  append (loop for int in key
                               collect `(,int (go ,tag)))
                else collect `(,key (go ,tag))))

    (def keys
      (mapcar #'case-key cases))

    (dolist (key keys)
      (unless (typep key 'integer)
        (error 'type-error
               :datum key
               :expected-type 'integer)))

    (setf cases
          (sort (copy-list cases) #'< :key #'case-key))

    (def tree-case (string-gensym 'tree-case))

    (def fail-tag (string-gensym 'fail))

    (def fail-body
      (and default
           `(return-from ,tree-case
              ,@default-body)))

    (defun rec (keyform cases)
      (cond ((null cases)
             `(go ,fail-tag))
            ((single cases)
             (destructuring-bind ((key jump)) cases
               `(if (= ,keyform ,key)
                    ,jump
                    (go ,fail-tag))))
            (t (mvlet* ((left-cases right-cases (halves cases))
                        (split (case-key (first right-cases))))
                 `(if (< ,keyform ,split)
                      ,(rec keyform left-cases)
                      ,(rec keyform right-cases))))))

    (def case-forms
      (loop for tag in tags
            for body in bodies
            append `(,tag
                     (return-from ,tree-case
                       ,@body))))

    (once-only (keyform)
      (let ((dispatch-form (rec keyform cases)))
        `(block ,tree-case
           (tagbody
              (with-subtype-dispatch integer (fixnum) ,keyform
                ,dispatch-form)
              ,@case-forms
              ,fail-tag ,@(unsplice fail-body)))))))

(defmacro tree-ecase (keyform &body clauses)
  "Like `tree-case', but signals an error if KEYFORM does not match
any of the provided cases."
  (once-only (keyform)
    `(tree-case ,keyform
       ,@clauses
       (otherwise
        (error 'type-error
               :datum ,keyform
               :expected-type '(member ,@(mappend (op (ensure-list (car _)))
                                          clauses)))))))
