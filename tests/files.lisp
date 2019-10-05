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
         (is (equal
              (pathname
               (uiop:run-program '("command" "-v" "ls")
                                 :output :string))
              (resolve-executable "ls"))))
        (t (skip "Not Windows or Unix."))))
