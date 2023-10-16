(in-package :serapeum)

(defun letf-bindings (bindings env)
  (with-collectors (savers setters restorers)
    (loop for (place expr) in bindings do
      (multiple-value-bind (vars vals stores setter getter)
          (get-setf-expansion place env)
        (let ((saves (make-gensym-list (length stores))))
          (mapc #'savers (mapcar #'list vars vals))
          (savers
           `(,@saves ,getter))
          (setters
           `(mvlet ((,@stores ,expr))
              ,setter))
          (restorers
           `(mvlet ((,@stores (values ,@saves)))
              ,setter)))))))

(defmacro letf* (bindings &body body)
  (if (no bindings) `(locally ,@body)
      `(letf ,(firstn 1 bindings)
         (letf* ,(rest bindings)
                ,@body))))

(defmacro letf (bindings &body body &environment env)
  (if (no bindings) `(locally ,@body)
      (mvlet ((savers setters restorers (letf-bindings bindings env)))
        `(mvlet* ,savers
           (unwind-protect
                (progn
                  ,@setters
                  (locally ,@body))
             ,@restorers)))))

(comment
  (letf (((car xs) 1)
         ((cdr ys) 2))
    (list xs ys)))
