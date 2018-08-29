(in-package :serapeum)

(defun package-exports (package)
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

(defun find-external-symbol (string package)
  "If PACKAGE exports a symbol named STRING, return it."
  (multiple-value-bind (sym status)
      (find-symbol string package)
    (and sym (eql status :external) sym)))
