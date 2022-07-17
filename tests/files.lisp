(in-package :serapeum.tests)

(def-suite files :in serapeum)
(in-suite files)

(test resolve-executable
  (cond ((uiop:os-macosx-p)
         ;; TODO Are there any universal Mac executables?
         (skip "Mac."))
        ((uiop:os-windows-p)
         (is-true (resolve-executable "clip"))
         (is-true (resolve-executable "notepad"))
         (is (equal (resolve-executable "clip")
                    (resolve-executable "clip.exe"))))
        ((uiop:os-unix-p)
         (is-true (resolve-executable "sh"))
         ;; ECL needs `pathname-equal' here; the two file names have
         ;; different versions.
         (is (uiop:pathname-equal
              (pathname
               (chomp
                (uiop:run-program '("sh" "-c" "command -v sh")
                                  :output :string)))
              (resolve-executable "sh"))))
        (t (skip "Not Windows or Unix."))))

(test file-size-human-readable
  (is (equal "0" (format-file-size-human-readable nil 0)))
  (is (equal "0" (format-file-size-human-readable nil 0 :flavor :iec)))
  (is (equal "0" (format-file-size-human-readable nil 0 :flavor :si)))
  (is (equal "1k" (format-file-size-human-readable nil 1024)))
  (is (equal "1KiB" (format-file-size-human-readable nil 1024 :flavor :iec)))
  (is (equal "1000" (format-file-size-human-readable nil 1000)))
  (is (equal "1 k" (format-file-size-human-readable nil 1000 :flavor :si)))
  (is (equal "500 k" (format-file-size-human-readable nil 500000 :flavor :si))))

(test file=
  (let* ((file1 (asdf:system-relative-pathname "serapeum" "README.md"))
         (file2
           (uiop:with-temporary-file (:pathname p
                                      :stream out
                                      :element-type 'character
                                      :direction :output
                                      :keep t)
             (write-string (read-file-into-string file1 :external-format :utf-8)
                           out)
             p))
         (empty-file
           (uiop:with-temporary-file (:pathname p
                                      :keep t)
             p))
         (junk-file
           (uiop:with-temporary-file (:pathname p
                                      :stream out
                                      :element-type 'character
                                      :direction :output
                                      :keep t)
             (write-string
              (shuffle
               (read-file-into-string file1 :external-format :utf-8))
              out)
             p)))
    (is (file= file1 file2))
    (is (not (file= file1 empty-file)))
    (is (not (file= file2 empty-file)))
    (is (not (file= junk-file empty-file)))
    (is (not (file= junk-file file1)))
    (is (not (file= junk-file file2)))
    (uiop:delete-file-if-exists file2)
    (uiop:delete-file-if-exists empty-file)
    (uiop:delete-file-if-exists junk-file)))

(test join
  (is (path-join "foo")
      #p"foo"
      :test 'uiop:pathname-equal)
  (is (path-join #p"foo" "bar")
      #p"foobar"
      :test 'uiop:pathname-equal)
  (is (path-join #p"foo" "bar" #p"baz")
      #p"foobarbaz"
      :test 'uiop:pathname-equal)
  (is (path-join #p"foo" "bar/baz")
      #p"foo/bar/baz"
      :test 'uiop:pathname-equal)
  (is (path-join #p"foo/bar" "baz")
      #p"foo/barbaz"
      :test 'uiop:pathname-equal)
  (is (path-join #p"foo/bar/" "baz")
      #p"foo/bar/baz"
      :test 'uiop:pathname-equal)
  (is (path-join #p"foo/" "bar/" "baz" "qux")
      #p"foo/bar/bazqux"
      :test 'uiop:pathname-equal)
  (is (path-join #p"foo.txt" "bar/baz")
      #p"foo.txt/bar/baz"
      :test 'uiop:pathname-equal)
  (is (path-join #p"foo.txt" "bar.ext")
      #p"foo.txtbar.ext"
      :test 'uiop:pathname-equal))

(test basename
  (is (path-basename "")
      nil)
  (is (path-basename "foo/bar")
      "bar")
  (is (path-basename #p"")
      nil)
  (is (path-basename #p"/foo/bar/baz")
      "baz")
  (is (path-basename #p"/foo/bar/baz/")
      "baz")
  (is (path-basename #p"/foo/bar/baz.ext")
      "baz.ext")
  (is (path-basename #p"foo/bar/baz.ext")
      "baz.ext"))
