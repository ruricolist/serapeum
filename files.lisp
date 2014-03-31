(in-package :serapeum)

(export '(write-stream-into-file
          file=
          file-size
          delete-file-if-exists))

(defun touch (pathname)
  "Ensure that PATHNAME exists, is not a directory, and can be opened."
  (check-type pathname pathname)
  (when (fad:directory-pathname-p pathname)
    (error "~A is a directory" pathname))
  (open pathname :direction :probe :if-does-not-exist :create))

(defparameter *backup-suffix* "~")

(defun backup-file-name (pathname &optional suffix)
  (let ((suffix (or suffix *backup-suffix*)))
    (pathname (concat (namestring pathname) suffix))))

(defun move-target (source dest)
  (let ((source (fad:pathname-as-file source))
        (dest (fad:pathname-as-directory dest)))
    (make-pathname :defaults dest
                   :name (pathname-name source)
                   :type (pathname-type source))))

(defun rm (pathname &rest args &key recursive force)
  "Implement a useful subset of the functionality of Unix `rm`.

Delete PATHNAME if it is a file.

If PATHNAME is a directory, delete it only given RECURSIVE.

Given FORCE, do not stop for non-existent files."
  (cond ((not (fad:directory-exists-p pathname))
         (if force
             (delete-file-if-exists pathname)
             (delete-file pathname)))
        ((not recursive)
         (cerror "Delete anyway"
                 "~a is a directory" pathname)
         (apply #'rm :recursive t args))
        (t (let ((idnx (if force
                           :ignore
                           :error)))
             (fad:delete-directory-and-files pathname
                                             :if-does-not-exist idnx)))))

(defun mv (src dest &rest args &key force backup suffix)
  "A useful subset of the functionality of Unix `mv`."
  (flet ((handle-overwrite (thing)
           (when-let (thing (fad:file-exists-p thing))
             (if force
                 (if (fad:directory-pathname-p thing)
                     (fad:delete-directory-and-files thing)
                     (delete-file thing))
                 (progn (cerror "Overwrite it" "~a already exists" thing)
                        (when backup
                          (rename-file thing (backup-file-name thing suffix)))
                        (apply #'mv :force t args))))))
    (let* ((src-dir? (fad:directory-exists-p src))
           (dest-exists? (fad:file-exists-p dest))
           (dest-dir? (and dest-exists? (fad:directory-pathname-p dest-exists?))))
      (cond ((and src-dir? dest-dir?)
             (let ((target (move-target src dest)))
               (handle-overwrite target)
               (rename-file src target)))
            (src-dir?
             (if (fad:file-exists-p dest)
                 (error "Cannot overwrite non-directory ~a with directory ~a"
                        src
                        dest)
                 (rename-file src dest)))
            (dest-dir?
             (let ((target (move-target src dest)))
               (handle-overwrite target)
               (rename-file src dest)))
            (t (handle-overwrite dest)
               (rename-file src dest))))))

;; TODO Recursive, backup.
(defun cp (src dest &rest args &key force backup suffix recursive)
  "A useful subset of the functionality of Unix `cp`."
  (flet ((handle-overwrite (thing)
           (when-let (thing (fad:file-exists-p thing))
             (if force
                 (if (fad:directory-pathname-p thing)
                     (fad:delete-directory-and-files thing)
                     (delete-file thing))
                 (progn (cerror "Overwrite it" "~a already exists" thing)
                        (when backup
                          (rename-file thing (backup-file-name thing suffix)))
                        (apply #'cp :force t args))))))
    (let ((src-dir? (fad:directory-exists-p src)))
      (unless recursive
        (cerror "Copy it anyway"
                "~a is a directory" src)
        (return-from cp (apply #'cp src dest :recursive t args)))
      (flet ((handle-copy (src dest)
               (if src-dir?
                   (progn
                     #+ccl (ccl::recursive-copy-directory src dest)
                     #-ccl (error "Copying directories is not implemented."))
                   (fad:copy-file src dest))))
        (let* ((dest-exists? (fad:file-exists-p dest))
               (dest-dir? (and dest-exists? (fad:directory-pathname-p dest-exists?))))
          (cond ((and src-dir? dest-dir?)
                 (let ((target (move-target src dest)))
                   (handle-overwrite target)
                   (handle-copy src target)))
                (src-dir?
                 (if (fad:file-exists-p dest)
                     (error "Cannot overwrite non-directory ~a with directory ~a"
                            src
                            dest)
                     (handle-copy src dest)))
                (dest-dir?
                 (let ((target (move-target src dest)))
                   (handle-overwrite target)
                   (handle-copy src dest)))
                (t (handle-overwrite dest)
                   (handle-copy src dest))))))))

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
           (let ((buffer1 (make-octet-vector buffer-size))
                 (buffer2 (make-octet-vector buffer-size)))
             (loop for end1 = (read-sequence buffer1 file1)
                   for end2 = (read-sequence buffer2 file2)
                   until (or (= end1 0) (= end2 0))
                   always (and (= end1 end2)
                               (loop for i from 0 below end1
                                     for j from 0 below end2
                                     always (eql (aref buffer1 i)
                                                 (aref buffer2 j))))))))))

(defun file-size (file &key (element-type 'character))
  "The size of FILE."
  (check-type file (or string pathname))
  (with-input-from-file (in file :element-type element-type)
    (file-length in)))

(defun delete-file-if-exists (file)
  "Delete FILE if it exists."
  (when (fad:file-exists-p file)
    (ignoring file-error
      (delete-file file))))
