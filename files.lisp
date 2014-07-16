(in-package :serapeum)

(export '(build-path
          write-stream-into-file
          file=
          file-size
          delete-file-if-exists))

(defun build-path (path &rest parts)
  "Build a pathname by merging from right to left.
With `build-path' you can pass the elements of the pathname being
built in the order they appear in it:

    (build-path (user-homedir-pathname) config-dir config-file)
    ≡ (merge-pathnames config-file (merge-pathnames config-dir (user-homedir-pathname)))

Note that `build-path' does not coerce the parts of the pathname into
directories; you have to do that yourself.

    (build-path \"dir1\" \"dir2\" \"file\") -> \"file\"
    (build-path \"dir1/\" \"dir2/\" \"file\") -> \"dir1/dir2/file\""
  (the pathname
       (reduce (lambda (x y)
                 (merge-pathnames y x))
               parts
               :initial-value path)))

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

(defun file= (file1 file2 &key (buffer-size 4096))
  "Compare FILE1 and FILE2 octet by octet, using buffers of
BUFFER-SIZE."
  (with-input-from-file (file1 file1 :element-type 'octet)
    (with-input-from-file (file2 file2 :element-type 'octet)
      (and (= (file-length file1)
              (file-length file2))
           (let ((buffer1 (make-array buffer-size :element-type '(unsigned-byte 8)))
                 (buffer2 (make-array buffer-size :element-type '(unsigned-byte 8))))
             (loop for end1 = (read-sequence buffer1 file1)
                   for end2 = (read-sequence buffer2 file2)
                   until (or (= end1 0) (= end2 0))
                   always (and (= end1 end2)
                               (loop for i from 0 below end1
                                     for j from 0 below end2
                                     always (eql (aref buffer1 i)
                                                 (aref buffer2 j))))))))))

(defun file-size (file &key (element-type '(unsigned-byte 8)))
  "The size of FILE, in bytes."
  (check-type file (or string pathname))
  (with-input-from-file (in file :element-type element-type)
    (file-length in)))

(defun delete-file-if-exists (file)
  "Delete FILE if it exists."
  (handler-case
      (delete-file file)
    (file-error ()
      nil)))
