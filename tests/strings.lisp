(in-package :serapeum.tests)

(def-suite strings :in serapeum)
(in-suite strings)

(test with-string
  (flet ((test* (designator)
           (with-string (s designator)
             (write-string "string" s))))
    (is (equal "string" (test* nil)))
    (is (equal "string"
               (with-output-to-string (*standard-output*)
                 (test* t))))
    (is (equal "string"
               (with-output-to-string (str)
                 (test* str))))
    (is (equal "the string"
               (let ((out (make-array 4
                                      :adjustable t
                                      :fill-pointer 4
                                      :element-type 'character
                                      :initial-contents "the ")))
                 (with-output-to-string (str out)
                   (test* str))
                 out)))))

(test word-wrap
  (is (every (op (< (length _) 40))
             (lines
              (word-wrap "There is no way on god’s green earth I can perform that function, Will Robinson."
                         :column 40)))))

(test word-wrap-regression
  (let ((string "A really long string that requires wrapping.")
        (column 20))
    (is
     (every (lambda (line) (<= (length line) column))
            (serapeum:lines (serapeum:word-wrap string :column column))))))

(test lines
  (let ((nl (string #\Newline))
        (cr uiop:+cr+)
        (lf uiop:+lf+)
        (crlf uiop:+crlf+))

    ;; General behavior and #\Newline tests.
    (is (equal nil (lines nil)))
    (is (equal nil (lines "")))
    (is (equal '("") (lines nl)))
    (is (equal '("abc") (lines "abc")))
    (is (equal '("abc") (lines (concat "abc" nl))))
    (is (equal '("" "abc") (lines (concat nl "abc"))))
    (is (equal '("abc" "") (lines (concat "abc" nl nl))))

    ;; ASCII EOL character and KEEP-EOLS tests.
    ;;
    ;; S and S1 are such that the body of LINES's DO* is exhaustively
    ;; tested.
    (let* ((s (concat "abc" lf crlf cr cr))
           (s1 (concat s "z")))
      (is (equal (list (concat "abc" lf) lf "")
                 (lines s :eol-style :cr)))
      (is (equal (list "abc" cr (concat cr cr))
                 (lines s :eol-style :lf)))
      (is (equal (list (concat "abc" lf) (concat cr cr))
                 (lines s :eol-style :crlf)))
      (is (equal (list (concat "abc" lf) (concat cr cr "z"))
                 (lines s1 :eol-style :crlf)))
      (is (equal '("abc" "" "" "")
                 (lines s :eol-style :ascii)))
      (is (equal '("abc" "" "" "" "z")
                 (lines s1 :eol-style :ascii)))
      (is (equal (list (concat "abc" lf) "" "")
                 (lines s :eol-style :cr :honor-crlf t)))
      (is (equal (list "abc" "" (concat cr cr))
                 (lines s :eol-style :lf :honor-crlf t)))
      (is (equal (list (concat "abc" lf) (concat cr cr))
                 (lines s :eol-style :crlf :honor-crlf nil)))
      (is (equal '("abc" "" "" "" "")
                 (lines s :eol-style :ascii :honor-crlf nil)))
      (is (equal (list (concat "abc" lf) crlf cr cr)
                 (lines s :eol-style :ascii :keep-eols t)))

      ;; EOL-STYLE predicate tests.
      (let ((cr-p (lambda (c) (eql c #\Return)))
            (lf-p (lambda (c) (eql c #\Linefeed))))
        (is (equal (list (concat "abc" lf) lf "")
                   (lines s :eol-style cr-p)))
        (is (equal (list (concat  "abc" lf) "" "")
                   (lines s :eol-style cr-p :honor-crlf t)))
        (is (equal (list "abc" cr (concat cr cr))
                   (lines s :eol-style lf-p)))
        (is (equal (list "abc" "" (concat cr cr))
                   (lines s :eol-style lf-p :honor-crlf t)))))

    ;; Unicode EOL character tests.
    (let ((nel (string #.(code-char #x0085)))
          (vt (string #.(code-char #x000B)))
          (ff (string #\Page))
          (ls (string #.(code-char #x2028)))
          (ps (string #.(code-char #x2029))))

      ;; Standard EOL characters.
      (let ((s (concat "a" cr "b" lf "c" crlf
                       "d" nel "e" vt "f" ff
                       "g" ls "h" ps "i")))
        (is (equal '("a" "b" "c" "d" "e" "f" "g" "h" "i")
                   (lines s :eol-style :unicode)))
        (is (equal '("a" "b" "c" "" "d" "e" "f" "g" "h" "i")
                   (lines s :eol-style :unicode :honor-crlf nil))))

      ;; Nonstandard EOL characters.
      (let ((fs (string #.(code-char #x001C)))
            (gs (string #.(code-char #x001D)))
            (rs (string #.(code-char #x001E))))
        (is (equal '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l")
                   (lines (concat "a" cr "b" lf "c" crlf
                                  "d" vt "e" ff "f" fs
                                  "g" gs "h" rs "i" nel
                                  "j" ls "k" ps "l")
                          :eol-style (lambda (c)
                                       (in c #\Return #\Linefeed
                                           #.(code-char #x000B)
                                           #\Page
                                           #.(code-char #x001C)
                                           #.(code-char #x001D)
                                           #.(code-char #x001E)
                                           #.(code-char #x0085)
                                           #.(code-char #x2028)
                                           #.(code-char #x2029)))
                          :honor-crlf t)))))))

(test compile-fmt-extra-args
  ;; Test that fmt raises an error/warning for a wrong number of
  ;; arguments, but only if this Lisp actually checks that in general.
  (handler-case
      (compile nil
               (eval
                '(lambda () (format nil "~a" "x" "y"))))
    ((or error warning) ()
      (signals ((or error warning))
        (compile nil
                 (eval
                  '(lambda () (fmt "~a" "x" "y"))))))))

(test fmt-compiler-macro-print-numeric
  ;; Test that the compiler macro for fmt handles numeric types correctly.
  (dolist (*read-default-float-format* '(single-float double-float))
    (dolist (float '(0.0s0 0.0d0))
      (is (equal (format nil "~f" float) (fmt "~f" float)))))
  (dolist (*read-default-float-format* '(single-float double-float))
    (dolist (float '(0.0s0 0.0d0))
      (is (equal (format nil "~g" float)
                 (fmt "~g" float)))))
  (is (equal "10"
             (let ((*print-base* 8))
               (fmt "~d" 10)))))

(test lines/count
  (is (null (lines "" :count 50)))
  (is (null (lines "" :count 0)))
  (is (null (lines (fmt "x~%y") :count 0)))
  (is (equal '("x") (lines (fmt "x") :count 1)))
  (is (equal '("x") (lines (fmt "x") :count 2)))
  (is (equal '("x") (lines (fmt "x~%y") :count 1)))
  (is (equal '("x" "y") (lines (fmt "x~%y") :count 2)))
  (is (equal '("x" "y") (lines (fmt "x~%y") :count 50))))

(test collapse-whitespace
  (is (equal (collapse-whitespace "") ""))
  (is (equal (collapse-whitespace " ") " "))
  (is (equal (collapse-whitespace "x") "x"))
  (is (equal (collapse-whitespace "  ") " "))
  (is (equal (collapse-whitespace "  one   two    three  ") " one two three "))

  (is (equal (collapse-whitespace "  one   two    three  " :space #\Newline)
             (fmt "~%one~%two~%three~%")))

  ;; A string consisting of a single whitespace character.
  (is (equal " " (collapse-whitespace (coerce '(#\Tab) 'string))))

  ;; Sanity check: is the reference string of whitespace characters
  ;; collapsed to a single whitespace?
  (is (equal " " (collapse-whitespace whitespace))))

(test mapconcat
  (is (equal "" (mapconcat #'identity nil " ")))
  (is (equal "" (mapconcat #'identity #() " ")))
  (is (equal "A B C" (mapconcat #'string-upcase #("a" "b" "c") " ")))
  (is (equal "A B C" (mapconcat #'string-upcase '("a" "b" "c") " "))))

(test mapconcat-end
  (is (equal " " (mapconcat #'identity nil " " :end t)))
  (is (equal " " (mapconcat #'identity #() " " :end t)))
  (is (equal "A B C " (mapconcat #'string-upcase #("a" "b" "c") " " :end t)))
  (is (equal "A B C " (mapconcat #'string-upcase '("a" "b" "c") " " :end t))))

(test string-upcase-initials
  (is (equal (string-upcase-initials "") ""))
  (is (equal (string-upcase-initials "a") "A"))
  (is (equal (string-upcase-initials "an ACRONYM")
             "An ACRONYM")))

(test same-case-p
  (is (not (same-case-p "")))
  (is (same-case-p "f"))
  (is (not (same-case-p ".")))
  (is (same-case-p "foo"))
  (is (same-case-p "foo-bar"))
  (is (not (same-case-p "Foo")))
  (is (not (same-case-p "-Foo"))))

(test string-invert-case
  (is (equal "ZEBRA" (string-invert-case "zebra")))
  (is (equal "zebra" (string-invert-case "ZEBRA"))))

(test escape
  (let ((in (concatenate 'string "foo" '(#\Tab) "bar" '(#\Tab) "baz"))
        (out "foo\\tbar\\tbaz")
        (table (lambda (c)
                 (case c
                   (#\Tab "\\t")))))
    (is (equal out (escape in table)))))

(test string^=
  (is (string^= "foo" "foobar"))
  (is (string^= "foo" "foo"))
  (is (not (string^= "foo" "fo")))
  (is (string^= "a long string" "string" :start1 (length "a long ")))
  (is (not (string^= "a" "")))
  (is (string^= "a" "abe"))
  (is (string^= "a" '|abc|))
  (is (not (string^= "a" "be"))))

(test string$=
  (is (string$= "bar" "foobar"))
  (is (string$= "bar" "bar"))
  (is (not (string$= "bar" "ar")))
  (is (not (string$= "1x" "2x")))
  (is (string$= "/" "foo/"))
  (is (not (string$= "/" "")))
  (is (string$= "c" '|abc|))
  (is (string$= "/" "/")))

(test string*=
  (is (search nil "any string"))
  (is (not (string*= nil "any string")))
  (is (string*= nil "NIL"))
  (is (not (string*= "a" "")))
  (is (string*= "a" "a"))
  (is (string*= "a" '|abc|))
  (is (string*= "a" "abe")))

(test string~=
  (is (string~= "foo" "foo bar"))
  (signals error
    (string~= "foo " "foo bar"))
  (signals error
    (string~= " foo " "foo bar"))
  (signals error
    (string~= " " "foo bar"))
  (is (string~= "foo" "bar foo"))
  (is (string~= "foo" "bar foo baz"))
  (is (not (string~= "foo" "barfoo baz")))
  (is (not (string~= "foo" "foobar baz")))
  (is (string~= "foo-bar" "foo-bar")))

(test string-replace-all
  (is (equal (string-replace-all "foo" "foobar" "baz") "bazbar"))
  (is (equal "The new way"
             (string-replace-all "old" "The old way" "new")))
  (is (equal "The new old way"
             (string-replace-all "old" "The old old way" "new" :start 3 :end 7)))
  (is (equal "quux quux quux"
             (string-replace-all "foo" "foo foo foo" "quux")))
  (is (equal "quux quux foo"
             (string-replace-all "foo" "foo foo foo" "quux" :count 2))))

(test string-replace-all-to-stream
  (is (equal
       (with-output-to-string (s)
         (string-replace-all "x" "foo" "y" :stream s))
       "foo"))
  (is (equal
       (with-output-to-string (s)
         (string-replace-all "o" "foo" "u" :stream s))
       "fuu"))
  (is (equal
       (with-output-to-string (s)
         (string-replace-all "o" "foo" "u" :stream s :count 0))
       "foo")))

(test chomp
  ;; Remove the longest sequence first.
  (is (equal "abc"
             (chomp "abcxyz" (list "z" "yz" "xyz")))))

(test string-count
  (is (zerop (string-count "foo" "")))
  (is (zerop (string-count "foo" "fofofo")))
  (is (= 1 (string-count "foo" "fofoofo"))))

(test string+
  (is (equal "" (string+)))
  (is (equal "abc" (string+ "a" "b" "c")))
  (is (equal "12345" (string+ 1 2 3 4 5)))
  (let ((a "new string"))
    (is (not (eq a (string+ a)))))
  (let ((*print-base* 8))
    (equal "20" (string+ 16)))
  (let ((my-vec (vector 1 2 3 4)))
    (let ((*print-array* t))
      (is (equal "#(1 2 3 4)" (string+ my-vec))))
    (let ((*print-array* nil))
      (is (string^= "#<" (string+ my-vec))))))

(test positive-numeric-string+
  (for-all ((base (lambda () (random-in-range 2 36)))
            (n (lambda () (random 1000))))
    (let ((range (shuffle (range n)))
          (*print-base* base))
      (is (equal (apply #'string+ (coerce range 'list))
                 (with-output-to-string (s)
                   (do-each (n range)
                     (princ n s))))))))

(test negative-numeric-string+
  (for-all ((base (lambda () (random-in-range 2 36)))
            (n (lambda () (random 1000))))
    (let ((range (shuffle (range (- n) 0)))
          (*print-base* base))
      (is (equal (apply #'string+ (coerce range 'list))
                 (with-output-to-string (s)
                   (do-each (n range)
                     (princ n s))))))))

(test nil-string+
  (locally (declare (notinline string+))
    (is (equal "" (string+ nil)))
    (is (equal "xz" (string+ "x" nil "z"))))
  (is (equal "" (string+ nil)))
  (is (equal "xz" (string+ "x" nil "z"))))

(test print-case-string+
  "Check that print case is respected even for constant symbols."
  (is (equal "foo1" (string+ '|foo| 1)))
  (is (equal "FOO1" (string+ '|FOO| 1)))
  (let ((*print-case* :downcase))
    (is (equal "foo1" (string+ :|FOO| 1)))
    (is (equal "foo1" (string+ '|FOO| 1)))))

(test string-join
  (is (equal "" (string-join #() "")))
  (is (equal "" (string-join '("") "")))
  (is (equal "" (string-join #("") "")))
  (is (equal "" (string-join '("") "+")))
  (is (equal "" (string-join '("") #\+)))
  (is (equal "+" (string-join '("") "+" :end t)))
  (is (equal "+" (string-join '("") #\+ :end t)))
  (is (equal "xy" (string-join '("x" "y") "")))
  (is (equal "x+y" (string-join '("x" "y") "+")))
  (is (equal "x+y" (string-join '("x" "y") #\+)))
  (is (equal "x+y" (string-join '("x" "y") :+)))
  (is (equal "x+y" (string-join '("x" "y") #\+)))
  (is (equal "+" (string-join nil "+" :end t)))
  (is (equal "+" (string-join #() "+" :end t)))
  (is (equal "x+y+" (string-join '("x" "y") "+" :end t)))
  (is (equal "x+y+" (string-join '("x" "y") #\+ :end t)))
  (is (equal "x+y+" (string-join '("x" "y") :+ :end t)))
  (is (equal "x+y+" (string-join #("x" "y") :+ :end t))))
