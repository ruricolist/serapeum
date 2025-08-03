(defpackage :serapeum/internal-package
  (:use :cl)
  (:export :define-internal-package))
(in-package :serapeum/internal-package)

(defmacro define-internal-package
    (internal-package public-package &body options)
  "Define INTERNAL-PACKAGE, used internally by Serapeum, and implementing
PUBLIC-PACKAGE.

The internal package re-exports all symbols exported by the public package, but also re-exports other symbols for internal use by other Serapeum modules that are not part of Serapeum's public interface."
  (let* ((impl-pred
           (lambda (option)
             (eql (car option) :implement)))
         (implemented-packages
           (apply #'append
                  (mapcar #'rest
                          (cons `(:implement ,public-package)
                                (remove-if-not impl-pred options)))))
         (options
           (remove-if impl-pred options)))
    (declare #-sb-package-locks (ignorable implemented-packages))
    `(progn
       (uiop:define-package ,internal-package
         (:use-reexport ,public-package)
         ,@options)
       #+sb-package-locks
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,@(loop for implemented-package in implemented-packages
                 collect `(sb-ext:add-implementation-package
                           ,internal-package
                           ,implemented-package)))
       (find-package ,internal-package))))
