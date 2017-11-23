(defpackage #:serapeum/mop
  (:use #:c2cl #:alexandria)
  (:import-from #:serapeum
    #:standard/context #:topmost-object-class
    #:supertypep)
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



;;; Cf. http://www.cliki.net/MOP%20design%20patterns

(defclass topmost-object-class (standard-class)
  ((topmost-class
    :initarg :topmost-class
    :type symbol
    :reader topmost-class)))

(defmethod print-object ((self topmost-object-class) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (with-slots (topmost-class) self
      (format stream "~a" topmost-class))))

(defmethod validate-superclass ((class1 topmost-object-class)
                                (class2 standard-class))
  t)

(defun insert-superclass (superclass list)
  ;; NB The version on the Cliki has the new superclass appended to
  ;; the direct superclasses, but that won't work if `standard-object'
  ;; is already one of the superclasses and is itself a superclass of
  ;; the superclass.
  (cond ((null list) (list superclass))
        ((subtypep superclass (first list))
         (cons superclass list))
        (t
         (cons (first list)
               (insert-superclass superclass (rest list))))))

(defmethod initialize-instance :around
    ((class topmost-object-class) &rest initargs &key direct-superclasses
                                                      topmost-class)
  (let ((superclass topmost-class))
    (if (find superclass direct-superclasses :test #'supertypep)
        (call-next-method)
        (apply #'call-next-method
               class
               :direct-superclasses (insert-superclass (find-class superclass)
                                                       direct-superclasses)
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
                    :direct-superclasses (insert-superclass (find-class superclass)
                                                            direct-superclasses)
                    initargs)))))

