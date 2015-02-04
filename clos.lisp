(in-package :serapeum)

(export '(class-name-safe find-class-safe
          make standard/context defmethods))

(defsubst make (class &rest initargs)
  "Shorthand for `make-instance'.
After Eulisp."
  (declare (dynamic-extent initargs))
  (apply #'make-instance class initargs))

(define-compiler-macro make (class &rest initargs)
  `(make-instance ,class ,@initargs))

(defun class-name-safe (x)
  "The class name of the class of X.
If X is a class, the name of the class itself."
  (if (typep x 'class)
      (class-name x)
      (class-name (class-of x))))

(defun find-class-safe (x)
  "The class designated by X.
If X is a class, it designates itself."
  (etypecase x
    (class x)
    (symbol (find-class x nil))))



;;; See http://www.tfeb.org/programs/lisp/wrapping-standard.lis
;;; and http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_4.htm

(define-method-combination standard/context ()
  ((around (:around))
   (context (:context) :order :most-specific-last)
   (before (:before))
   (primary () :required t)
   (after (:after)))
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods)))
    (let* ((form (if (or before after (rest primary))
                     `(multiple-value-prog1
                          (progn ,@(call-methods before)
                                 (call-method ,(first primary)
                                              ,(rest primary)))
                        ,@(call-methods (reverse after)))
                     `(call-method ,(first primary))))
           (around-form (if around
                            `(call-method ,(first around)
                                          (,@(rest around)
                                           (make-method ,form)))
                            form)))
      (if context
          `(call-method ,(first context)
                        (,@(rest context)
                         (make-method ,around-form)))
          around-form))))



(defmacro defmethods (class (self . slots) &body body)
  `(macrolet ((:method (name &body body)
                (let ((class ',class)
                      (self ',self)
                      (slots ',slots)
                      (qualifier (when (not (listp (car body))) (pop body)))
                      (args (pop body))
                      (docstring (when (stringp (car body)) (pop body))))
                  `(symbol-macrolet ,(loop for slot in slots
                                           collect `(,slot (slot-value ,self ',slot)))
                     (defmethod ,name ,@(unsplice qualifier) ,(substitute (list self class) self args)
                       ,@(unsplice docstring)
                       ,@body)))))
     ,@body))
