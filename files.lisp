(in-package :serapeum)

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
    (path-join \"dir1/\" \"dir2/\" \"file\") -> #p\"dir1/dir2/file\""
  (the pathname
       (reduce (lambda (x y)
                 (uiop:merge-pathnames* y x))
               pathnames
               :initial-value (make-pathname))))

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
  (declare (optimize speed)
           (ignorable buffer-size))
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
   (optimize (speed 3) (safety 1)
             (debug 0) (compilation-speed 0)))
  (flet ((make-buffer ()
           (make-array buffer-size
                       :element-type 'octet
                       :initial-element 0)))
    (declare (inline make-buffer))
    (with-input-from-file (file1 file1 :element-type 'octet)
      (with-input-from-file (file2 file2 :element-type 'octet)
        (and (= (file-length file1)
                (file-length file2))
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
