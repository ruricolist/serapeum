(defpackage #:serapeum/mop
  (:use #:c2cl #:alexandria)
  (:import-from #:serapeum #:standard/context #:topmost-object-class)
  #+sb-package-locks (:implement :serapeum :serapeum/mop))

(in-package :serapeum/mop)

;;; See https://github.com/tfeb/interim-lisp/blob/master/wrapping-standard.lisp
;;; and http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_4.htm

(define-method-combination standard/context ()
  ((around (:around))
   (context (:context) :order :most-specific-last)
   (before (:before))
   (primary () :required t)
   (after (:after)))
  (flet ((call-methods (methods)
           (mapcar (lambda (method)
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



(defun supertypep (x y)
  (subtypep y x))

(defclass topmost-object-class (standard-class)
  ((topmost-class
    :initarg :topmost-class
    :type symbol
    :reader topmost-class)))

(defmethod validate-superclass ((class1 topmost-object-class)
                                (class2 standard-class))
  t)

(defmethod initialize-instance :around
    ((class topmost-object-class) &rest initargs &key direct-superclasses
                                                      topmost-class)
  (let ((superclass topmost-class))
    (if (find superclass direct-superclasses :test #'supertypep)
        (call-next-method)
        (apply #'call-next-method
               class
               :direct-superclasses (append direct-superclasses
                                            (list (find-class superclass)))
               initargs))))

(defmethod reinitialize-instance :around
    ((class topmost-object-class) &rest initargs &key (direct-superclasses nil direct-superclasses-supplied?))
  (let ((superclass (topmost-class class)))
    (cond ((not direct-superclasses-supplied?)
           (call-next-method))
          ((find superclass direct-superclasses :test #'supertypep)
           (call-next-method))
          (t (apply #'call-next-method
                    class
                    :direct-superclasses
                    (append direct-superclasses (list (find-class superclass)))
                    initargs)))))

