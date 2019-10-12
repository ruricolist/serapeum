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
         (is (equal
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
