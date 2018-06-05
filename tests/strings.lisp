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
  (is (equal "A B C" (mapconcat #'string-upcase #("a" "b" "c") " ")))
  (is (equal "A B C" (mapconcat #'string-upcase '("a" "b" "c") " "))))

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
  (is (string~= "foo" "bar foo"))
  (is (string~= "foo" "bar foo baz"))
  (is (not (string~= "foo" "barfoo baz")))
  (is (not (string~= "foo" "foobar baz")))
  (is (string~= "foo-bar" "foo-bar")))

(test string-replace-all
  (is (equal (string-replace-all "foo" "foobar" "baz") "bazbar"))
  (is-true
   (let ((s "foo, bar"))
     (eq s (string-replace-all ":" s ""))))
  (is (equal "The new way"
             (string-replace-all "old" "The old way" "new")))
  (is (equal "The new old way"
             (string-replace-all "old" "The old old way" "new" :start 3 :end 7)))
  (is (equal "quux quux quux"
             (string-replace-all "foo" "foo foo foo" "quux")))
  (is (equal "quux quux foo"
             (string-replace-all "foo" "foo foo foo" "quux" :count 2))))

(test chomp
  ;; Remove the longest sequence first.
  (is (equal "abc"
             (chomp "abcxyz" (list "z" "yz" "xyz")))))

(test string-count
  (is (zerop (string-count "foo" "")))
  (is (zerop (string-count "foo" "fofofo")))
  (is (= 1 (string-count "foo" "fofoofo"))))

(test string+
  (is (not (eq (string+) (string+))))
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

(def-suite pad :in strings)

(in-suite pad)

(test pad-start
  (is (equal "abc" (pad-start "abc" 0)))
  (is (equal "abc" (pad-start "abc" 1)))
  (is (equal "abc" (pad-start "abc" 0 "")))
  (is (equal "abc" (pad-start "abc" 1 "")))
  (is (equal "0000000001" (pad-start "1" 10 #\0)))
  (is (equal "0000000012" (pad-start "12" 10 #\0)))
  (is (equal "0000123456" (pad-start "123456" 10 #\0)))
  (is (equal "YYYY-MM-12" (pad-start "12" 10 "YYYY-MM-DD")))
  (is (equal "YYYY-09-12" (pad-start "09-12" 10 "YYYY-MM-DD")))
  (is (equal "xxabc" (pad-start "abc" 5 #\x)))
  (is (equal "       abc" (pad-start "abc" 10)))
  (is (equal "00000abc" (pad-start "abc" 8 "0")))
  (is (equal "foofoofabc" (pad-start "abc" 10 "foo")))
  (is (equal "123abc" (pad-start "abc" 6 "123456"))))

(test pad-end
  (is (equal "123   " (pad-end "123" 6)))
  (is (equal "123xxx" (pad-end "123" 6 #\x)))
  (is (equal "123xxx" (pad-end "123" 6 "x")))
  ;; Possibly surprising behaviors.
  (is (equal "2016YYYY-M" (pad-end "2016" 10 "YYYY-MM-DD")))
  (let ((year "2016"))
    (is (equal "2016-MM-DD" (pad-end year 10 (subseq "YYYY-MM-DD" (length year)))))))
