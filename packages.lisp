(in-package :serapeum)

(defun package-exports (&optional (package *package*))
  "Return a list of the symbols exported by PACKAGE."
  (loop for symbol being the external-symbols of package
        collect symbol))

(defun package-names (package)
  "Return a list of all the names of PACKAGE: its name and its nicknames."
  (cons (package-name package)
        (package-nicknames package)))

(defun package-name-keyword (package)
  "Return the name of PACKAGE as a keyword."
  (let* ((package (find-package package))
         (name (package-name package)))
    (intern name :keyword)))

(defun find-external-symbol (string package &key ((:error errorp) nil))
  "If PACKAGE exports a symbol named STRING, return it.
If PACKAGE does not contain such a symbol, or if the symbol is not
exported, then `nil' is returned, unless ERROR is non-nil, in which
case an error is signaled."
  (multiple-value-bind (sym status)
      (find-symbol (string string) package)
    (cond ((null sym)
           (when errorp
             (error "No symbol named ~a in ~a" string package)))
          ((eql status :external) sym)
          (t
           (when errorp
             (error "~s is not exported from package ~a."
                    sym package))))))

(defmacro export-always (symbols &optional (package nil package-supplied?))
  "Like `export', but also evaluated at compile time."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (export ,symbols ,@(and package-supplied? (list package)))))

(defun export-only (export/s &optional (package *package*))
  "Like EXPORT, but unexport any other, existing exports."
  (let* ((new (ensure-list export/s))
         (old (package-exports package))
         (disallowed (set-difference old new)))
    (unexport disallowed package)
    (export new package)))

(defmacro export-only-always (symbols &optional (package nil package-supplied?))
  "Like `export-only', but also evaluated at compile time."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (export-only ,symbols ,@(and package-supplied? (list package)))))
