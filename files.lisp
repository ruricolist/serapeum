(in-package :serapeum)

;;; Pathname types. These correspond to the predicates defined by
;;; UIOP.

(declaim
 (inline
  absolute-pathname-p/total
  directory-pathname-p/total
  file-pathname-p/total
  relative-pathname-p/total
  wild-pathname-p/total))

(defun absolute-pathname-p/total (x)
  (and (pathnamep x) (uiop:absolute-pathname-p x)))

(defun directory-pathname-p/total (x)
  (and (pathnamep x) (uiop:directory-pathname-p x)))

(defun file-pathname-p/total (x)
  (and (pathnamep x) (uiop:file-pathname-p x)))

(defun relative-pathname-p/total (x)
  (and (pathnamep x) (uiop:relative-pathname-p x)))

(defun wild-pathname-p/total (x)
  "Like `wild-pathname-p', but total."
  (and (pathnamep x) (wild-pathname-p x)))

(deftype absolute-directory-pathname ()
  '(and absolute-pathname directory-pathname))

(deftype absolute-file-pathname ()
  '(and absolute-pathname file-pathname))

(deftype absolute-pathname ()
  '(and pathname (satisfies absolute-pathname-p/total)))

(deftype directory-pathname ()
  '(and pathname (satisfies directory-pathname-p/total)))

(deftype file-pathname ()
  '(and pathname (satisfies file-pathname-p/total)))

;;; logical-pathname is defined in CL.

(deftype non-wild-pathname ()
  "A pathname without wild components."
  '(or directory-pathname
    (and pathname (not (satisfies wild-pathname-p/total)))))

(deftype physical-pathname ()
  '(and pathname (not logical-pathname)))

(deftype relative-pathname ()
  '(and pathname (satisfies relative-pathname-p/total)))

(deftype wild-pathname ()
  "A pathname with wild components."
  '(and pathname (satisfies wild-pathname-p/total)))

(defmacro with-open-files ((&rest args) &body body)
  "A simple macro to open one or more files providing the streams for the BODY. The ARGS is a list of `(stream filespec options*)` as supplied to WITH-OPEN-FILE."
  (case (length args)
    ((0)
     `(progn ,@body))
    ((1)
     `(with-open-file ,(first args) ,@body))
    (t `(with-open-file ,(first args)
          (with-open-files
              ,(rest args) ,@body)))))

(defun path-basename (pathname)
  "Return the basename, that is:
- if it's a directory, the name of the directory,
- if it's a file, the name of the file including its type (extension)."
  (first (last (pathname-directory (uiop:ensure-directory-pathname pathname)))))

(-> path-join (&rest (or string stream pathname))
  (values pathname &optional))
(defun path-join (&rest pathnames)
  "Build a pathname by merging from right to left.
With `path-join' you can pass the elements of the pathname being built
in the order they appear in it:

    (path-join (user-homedir-pathname) config-dir config-file)
    ≡ (uiop:merge-pathnames* config-file
       (uiop:merge-pathnames* config-dir
        (user-homedir-pathname)))

Note that `path-join' does not coerce the parts of the pathname into
directories; you have to do that yourself.

    (path-join \"dir1\" \"dir2\" \"file\") -> #p\"file\"
    (path-join \"dir1/\" \"dir2/\" \"file\") -> #p\"dir1/dir2/file\"

Cf. `base-path-join' for a similar function with more intuitive
behavior."
  (the pathname
       (reduce (lambda (x y)
                 (uiop:merge-pathnames* y x))
               pathnames
               :initial-value (make-pathname))))

(-> base-path-join ((or string stream pathname) &rest (or string stream pathname))
  (values pathname &optional))
(defun base-path-join (base &rest suffixes)
  "Build a pathname by appending SUFFIXES to BASE.
For `path-join-base', the path on the left is always the *base* and
the path on the right is always the *suffix*. This means that even if
the right hand path is absolute, it will be treated as if it were
relative.

    (base-path-join #p\"foo/bar\" #p\"/baz\")
    => #p\"foo/bar/baz\")

Also, a bare file name as a suffix does not override but is appended
to the accumulated file name. This includes the extension.

    (base-path-join #p\"foo/bar\" \"baz\")
    => #p\"foo/barbaz\")

    (base-path-join #p\"foo/bar.x\" \"baz.y\")
    => #p\"foo/bar.xbaz.y\")

See `path-join' for a similar function with more consistent behavior."
  ;; Contributed by Pierre Niedhardt (@ambrevar).
  ;; https://github.com/ruricolist/serapeum/issues/127
  (if (null suffixes)
      (the (values pathname &optional)
           (uiop:ensure-pathname base))
      (reduce (lambda (path1 path2)
                (if (or (null (pathname-name path1))
                        (pathname-directory path2))
                    (uiop:merge-pathnames*
                     (uiop:relativize-pathname-directory
                      (uiop:ensure-pathname path2))
                     (uiop:ensure-pathname path1 :ensure-directory t))
                    (let ((new-base (string+ (path-basename path1)
                                             (path-basename path2))))
                      (make-pathname :defaults path1
                                     :type (pathname-type new-base)
                                     :name (pathname-name new-base)))))
              suffixes
              :initial-value base)))

(defun write-stream-into-file (stream pathname &key (if-exists :error) if-does-not-exist)
  "Read STREAM and write the contents into PATHNAME.

STREAM will be closed afterwards, so wrap it with
`make-concatenated-stream' if you want it left open."
  (check-type pathname pathname)
  (with-open-stream (in stream)
    (with-output-to-file (out pathname
                              :element-type (stream-element-type in)
                              :if-exists if-exists
                              :if-does-not-exist if-does-not-exist)
      (copy-stream in out)))
  pathname)

(defun write-file-into-stream (pathname output &key (if-does-not-exist :error)
                                                    (external-format :default))
  "Write the contents of FILE into STREAM."
  (check-type pathname pathname)
  (with-input-from-file (input pathname
                               :element-type (stream-element-type output)
                               :if-does-not-exist if-does-not-exist
                               :external-format external-format)
    (copy-stream input output)))

(defun file= (file1 file2 &key (buffer-size 4096))
  "Compare FILE1 and FILE2 octet by octet, \(possibly) using buffers
of BUFFER-SIZE."
  (declare (ignorable buffer-size))
  (let ((file1 (truename file1))
        (file2 (truename file2)))
    (or (equal file1 file2)
        (and (= (file-size-in-octets file1)
                (file-size-in-octets file2))
             #+ccl (file=/mmap file1 file2)
             #-ccl (file=/loop file1 file2 :buffer-size buffer-size)))))

#+ccl
(defun file=/mmap (file1 file2)
  "Compare FILE1 and FILE2 by memory-mapping them and comparing them
as vectors."
  (macrolet ((with-mmap ((var file) &body body)
               `(let* ((,var (ccl:map-file-to-octet-vector ,file)))
                  (unwind-protect
                       (progn ,@body)
                    (ccl:unmap-ivector ,var)))))
    (with-mmap (vec1 file1)
      (with-mmap (vec2 file2)
        ;; The vector returned when CCL memory maps a file is a
        ;; displaced vector, because of alignment issues. But
        ;; `octet-vector=' takes a `:start' parameter, so we can
        ;; directly compare the underlying simple vectors.
        (multiple-value-bind (vec1 start1)
            (array-displacement vec1)
          (multiple-value-bind (vec2 start2)
              (array-displacement vec2)
            (octet-vector= vec1 vec2
                           :start1 start1
                           :start2 start2)))))))

(defun file=/loop (file1 file2 &key (buffer-size 4096))
  "Compare two files by looping over their contents using a buffer."
  (declare
   (type pathname file1 file2)
   (type array-length buffer-size)
   (optimize (safety 1) (debug 0) (compilation-speed 0)))
  (flet ((make-buffer ()
           (make-array buffer-size
                       :element-type 'octet
                       :initial-element 0)))
    (declare (inline make-buffer))
    (with-open-files ((file1 file1 :element-type 'octet :direction :input)
                      (file2 file2 :element-type 'octet :direction :input))
      (and (= (file-length file1)
              (file-length file2))
           (locally (declare (optimize speed))
             (loop with buffer1 = (make-buffer)
                   with buffer2 = (make-buffer)
                   for end1 = (read-sequence buffer1 file1)
                   for end2 = (read-sequence buffer2 file2)
                   until (or (zerop end1) (zerop end2))
                   always (and (= end1 end2)
                               (octet-vector= buffer1 buffer2
                                              :end1 end1
                                              :end2 end2))))))))

(defun file-size (file &key (element-type '(unsigned-byte 8)))
  "The size of FILE, in units of ELEMENT-TYPE (defaults to bytes).

The size is computed by opening the file and getting the length of the
resulting stream.

If all you want is to read the file's size in octets from its
metadata, consider `trivial-file-size:file-size-in-octets' instead."
  (check-type file (or string pathname))
  (with-input-from-file (in file :element-type element-type)
    (file-length in)))

(defconstant +pathsep+
  (if (uiop:os-windows-p) #\; #\:)
  "Path separator for this OS.")

(defun exe (p)
  "If P, a pathname designator, has no extension, then, on Windows
only, add an extension of `.exe`."
  (let* ((p (pathname p))
         (type (pathname-type p)))
    (if (and (uiop:os-windows-p)
             (null type))
        (make-pathname :type "exe"
                       :defaults p)
        p)))

(defun $path ()
  "Split the PATH environment variable."
  (mapcar #'uiop:ensure-directory-pathname
          ;; This is enough; Neither Windows nor POSIX support
          ;; escaping the separator in $PATH.
          (split-sequence +pathsep+
                          (uiop:getenv "PATH")
                          :remove-empty-subseqs t)))

(defun resolve-executable (p)
  "Look for an executable using the PATH environment variable.
P is a pathname designator.

On Windows only, if P does not have an extension, it assumed to end in
`.exe`.

Note that this function does not check the current directory (even on
Windows) and it does not care if P is already an absolute pathname: it
only cares about its name and type."
  (let* ((p (exe p))
         (name (pathname-name p))
         (type (pathname-type p)))
    (loop for dir in ($path)
          for pathname = (make-pathname :name name
                                        :type type
                                        :defaults dir)
          when (uiop:file-exists-p pathname)
            do (return pathname))))

(defun format-file-size-human-readable (stream file-size
                                        &key flavor
                                             (space (eql flavor :si))
                                             (suffix (if (eql flavor :iec) "B" "")))
  "Write FILE-SIZE, a file size in bytes, to STREAM, in human-readable form.

STREAM is interpreted as by `format'.

If FLAVOR is nil, kilobytes are 1024 bytes and SI prefixes are used.

If FLAVOR is `:si', kilobytes are 1000 bytes and SI prefixes are used.

If FLAVOR is `:iec', kilobytes are 1024 bytes and IEC prefixes (Ki,
Mi, etc.) are used.

If SPACE is non-nil, include a space between the number and the
prefix. (Defaults to T if FLAVOR is `:si'.)

SUFFIX is the suffix to use; defaults to B if FLAVOR is `:iec',
otherwise empty."
  (check-type file-size (integer 0 *))
  (if (zerop file-size)
      (format stream "0")
      (let ((flavor (if (null flavor) :file flavor)))
        (multiple-value-bind (formatter args)
            (human-size-formatter file-size :flavor flavor :space space)
          (format stream "~?~a" formatter args suffix)))))

(defun file-size-human-readable (file &key flavor space suffix stream)
  "Format the size of FILE (in octets) using `format-file-size-human-readable'.
The size of file is found by `trivial-file-size:file-size-in-octets'.

Inspired by the function of the same name in Emacs."
  (let ((file-size (file-size-in-octets file)))
    (format-file-size-human-readable
     stream
     file-size
     :flavor flavor
     :suffix suffix
     :space space)))
