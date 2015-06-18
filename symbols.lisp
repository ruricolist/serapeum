(in-package :serapeum)

;;;# `find-keyword'

(defun find-keyword (string)
  "If STRING has been interned as a keyword, return it.

Like `make-keyword', but preferable in most cases, because it doesn't
intern a keyword -- which is usually both unnecessary and unwise."
  ;; TODO Should the case of the readtable be respected?
  (find-symbol (string string) :keyword))

;;;# `bound-value'

;;; `bound-value' is a contraction for the idiom of checking `boundp'
;;; before calling `symbol-value'.

(defsubst bound-value (s &optional default)
  "If S is bound, return (values s t). Otherwise, return DEFAULT."
  (if (boundp s)
      (values (symbol-value s) t)
      (values default nil)))

;;;# `special-variable-p'

;;; In the absence of an implementation-specific way to check for
;;; boundness, we use a clever trick from comp.lang.lisp, author
;;; Thomas Burdick.

;;; <http://groups.google.com/group/comp.lang.lisp/msg/87c67720e0e7cc35>.

(defun special-variable-p-hack (s)
  "Hack to check if S is a special variable.
S is a symbol. We use `eval' to bind the symbol around a thunk, then
bind the symbol to a different value before calling it. If the thunk
sees the new value, then the symbol must designate a special
variable."
  (eval `(let ((thunk (let (,s)
                        (declare (ignorable ,s))
                        (lambda () ,s))))
           (let ((,s t))
             (declare (ignorable ,s))
             (funcall thunk)))))

(defun special-variable-p (symbol)
  "Is SYMBOL a special variable?"
  ;; Check is symbol is bound in the global environment. Otherwise, try to
  ;; make a closure over symbol, and return T if it fails.
  (check-type symbol symbol)
  (or (boundp symbol)
      #+lispworks (sys:declared-special-p symbol)
      #+sbcl (eql :special (sb-int:info :variable :kind symbol))
      #+allego (eq (sys:variable-information symbol) :special)
      #+clozure (ccl:proclaimed-special-p symbol)
      #-(or lispworks sbcl allegro clozure)
      (special-variable-p-hack symbol)))

(defun symbol-macro-p (symbol)
  (let ((*macroexpand-hook* (constantly t)))
    (nth-value 1 (macroexpand-1 symbol))))
