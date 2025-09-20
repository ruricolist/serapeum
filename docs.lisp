(in-package #:cl-user)

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
      (let ((name (ppcre:regex-replace "\\d{2,}$" (string s) "")))
        (if-let (p (symbol-package s))
          (intern name p)
          (make-symbol name)))
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

(defun function-reference-data (package-name system-name)
  (let* ((package
           (or (find-package package-name)
               (error "No such package as ~a" package-name)))
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


(defun render-function-reference-as-markdown (package-names system-name &key stream)
  "Renders API reference for given `PACKAGE-NAMES` of system named `SYSTEM-NAME`.

`STREAM` argument can be nil, stream, string or a pathname.

If it is a stream, then output will be written to that stream.

If it is nil, then the functio will return output as a string.
In case of string or a pathname, output will be rendered into the
file with that name, superseding it if it is already exists."
  (labels ((render (stream)
             (let ((data
                     (mappend (op (function-reference-data _ system-name))
                              (ensure-list package-names))))
               (format stream "# Function Listing For ~a (~d files, ~d functions)~2%"
                       system-name
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
                   (format stream "~&## ~a~2%"
                           (pathname-title file)))
                 (let ((intro-file
                         (merge-pathnames
                          (make-pathname :type "md")
                          file)))
                   (when (uiop:file-exists-p intro-file)
                     (with-input-from-file (in intro-file :element-type 'character)
                       (copy-stream in stream))
                     (format stream "~2%")))
                 ;; Each definition.
                 (dolist (def defs)
                   (let* ((docs
                            (ppcre:regex-replace-all "`([^ ]+?)'"
                                                     (getf def :documentation)
                                                     "`\\1`"))
                          (*print-case* :downcase)
                          (*package* (symbol-package (getf def :name))))
                     (format stream "~&### `~a`~2%~a~2%[View source](~a#L~a)~2%"
                             (cons (getf def :name) (getf def :args))
                             docs
                             (file-name-nonsystem (getf def :file) system-name)
                             (getf def :line-number)))))
               (format stream "~&"))))
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

(defun update-function-reference (filename system &optional packages)
  "A short hand for calling `RENDER-FUNCTION-REFERENCE-AS-MARKDOWN`.

It accepts a short `FILENAME` and the result will be written to the `SYSTEM`'s folder.

Also, you can omit `PACKAGES` if your system provides only one package with the
same name.

Example usage:

    (ql:quickload :serapeum/docs)
    (serapeum.docs:update-function-reference
        \"REFERENCE.md\"
        :my-system)

"
  (check-type system keyword)
  (check-type filename string)

  ;; If packages aren't provided, consider there is only
  ;; one package with the same name as the system.
  (unless packages
    (setf packages
          (list system)))

  (render-function-reference-as-markdown
   packages
   (string-downcase
    (symbol-name system))
   :stream (asdf:system-relative-pathname system filename)))
