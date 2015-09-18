(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:serapeum :cl-ppcre :swank)))

(defpackage #:serapeum.docs
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:swank/backend #:find-source-location)
  (:export
   #:render-function-reference-as-markdown
   #:update-function-reference))

(in-package #:serapeum.docs)

(defun ungensym (s)
  "If S is a gensym, chop the numbers off the end.
This saves needless updates to the documentation."
  (if (symbolp s)
      (intern (ppcre:regex-replace "\\d{2,}$" (string s) "")
              (symbol-package s))
      s))

(defun arglist (s)
  (mapcar #'ungensym (swank-backend:arglist s)))

(defun asdf-components (comp)
  (etypecase comp
    (asdf:c-source-file nil)
    (asdf:cl-source-file (list comp))
    (asdf::static-file nil)
    (asdf::component
     (loop for c in (funcall
                     #+asdf3 #'asdf:component-children
                     #-asdf3 #'asdf:module-components
                     comp)
           append (asdf-components c)))))

(defun package-external-symbols (package)
  (collecting
    (do-external-symbols (s package)
      (when (eql (symbol-package s) package)
        (collect s)))))

(defun function-reference-data (package-name &optional (system-name package-name))
  (let* ((package (find-package package-name))
         (data (collect-reference-data package))
         (system (asdf:find-system system-name))
         (by-file (assort data :key (op (getf _ :file)) :test 'equal))
         (components (asdf-components system))
         (order (ordering (mapcar (op (namestring (slot-value _ 'asdf::absolute-pathname)))
                                  components)
                          :test 'equal)))
    (sort (loop for defs in by-file
                for file = (getf (first defs) :file)
                collect (cons file (sort defs
                                         #'<
                                         :key (op (getf _ :position)))))
          order
          :key #'car)))

(defun collect-reference-data (package)
  (collecting
    (do-external-symbols (s package)
      (when (and (fboundp s)
                 (eql (symbol-package s) package))
        (let* ((name s)
               (type (symbol-function-type s))
               (documentation (documentation s 'function))
               (args (arglist s))
               (location (cdr (swank-backend:find-source-location s)))
               (file (assocadr :file location))
               (position (assocadr :position location))
               (line-number (1+ (count #\Newline
                                       (read-file-into-string file)
                                       :end position))))
          (collect (list :name name
                         :type type
                         :documentation (or documentation
                                            "NO DOCS!")
                         :args args
                         :file file
                         :position position
                         :line-number line-number)))))))

(defun symbol-function-type (s)
  (cond ((macro-function s) :macro)
        ((typep (fdefinition s) 'generic-function) :generic-function)
        ((find-class s nil) :type)
        (t :function)))

(defun render-function-reference-as-markdown (package-name &key stream (system-name package-name))
  (labels ((render (stream)
             (let ((data (function-reference-data package-name system-name)))
               (format stream "# Function Listing For ~a (~d files, ~d functions)~2%"
                       package-name
                       (length data)
                       (reduce #'+ data :key (op (length (cdr _)))))
               ;; Table of contents
               (loop for (file . nil) in data do
                 (format stream "~&- [~a](#~a)~%"
                         (pathname-title file)
                         (pathname-name file))
                     finally (terpri stream))
               ;; Each file.
               (loop for (file . defs) in data do
                 (let (*print-pretty*) ;Keep long arg lists from overflowing.
                   (format stream "~&## ~a~2%" (pathname-title file)))
                 ;; Each definition.
                 (dolist (def defs)
                   (let* ((docs
                            (ppcre:regex-replace-all "`([^ ]+)'"
                                                     (getf def :documentation)
                                                     "`\\1`"))
                          (*print-case* :downcase)
                          (*package* (find-package package-name)))
                     (format stream "~&### `~a`~2%~a~2%[View source](~a#L~a)~2%"
                             (cons (getf def :name) (getf def :args))
                             docs
                             (file-name-nonsystem (getf def :file) system-name)
                             (getf def :line-number))))))))
    (etypecase stream
      (stream
       (render stream))
      (null
       (with-output-to-string (s)
         (render s)))
      ((or string pathname)
       (with-output-to-file (out stream
                                 :if-exists :supersede)
         (render out))))))

(defun file-name-nonsystem (fn system)
  (enough-namestring fn (system-base system)))

(defun system-base (system)
  (asdf:system-relative-pathname system ""))

(defun pathname-title (file)
  (fmt "~{~:(~a~)~^ ~}" (split-sequence #\- (pathname-name file))))

(defun update-function-reference ()
  (render-function-reference-as-markdown
   :serapeum
   :stream (asdf:system-relative-pathname :serapeum "reference.md")))
