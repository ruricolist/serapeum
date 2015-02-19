(in-package :serapeum)

(export '(whitespace collapse-whitespace
          whitespacep blankp trim-whitespace
          concat mapconcat string-join
          string-upcase-initials
          nstring-upcase-initials
          same-case-p
          string-invert-case
          nstring-invert-case
          words tokens lines
          fmt
          with-string
          escape
          ellipsize
          string^= string$= string~= string*=
          string-prefixp string-suffixp string-containsp string-tokenp
          string-replace-all))

(deftype string-offset ()
  '(integer #.(- array-dimension-limit) #.array-dimension-limit))

(defconst alphabet
  "abcdefghijklmnopqrstuvwxyz"
  "The 26 lowercase letters of the English alphabet.")

(defconst no-break-space
  #-abcl #\No-break_space
  #+abcl (code-char 160))

(defconst whitespace
  #.(remove-duplicates
     (coerce (list #\Space #\Tab #\Linefeed #\Return #\Newline #\Page no-break-space)
             'string))
  "Whitespace characters.")

(defsubst whitespacep (char)
  "Is CHAR whitespace?

Spaces, tabs, any kind of line break, page breaks, and no-break spaces
are considered whitespace."
  (case char
    (#.(coerce whitespace 'list) t)))

(defsubst trim-whitespace (string)
  "STRING without whitespace at ends."
  (string-trim whitespace string))

(defun call/string (fn stream)
  "Resolve STREAM like `format' and call FN with the result."
  (fbind (fn)
    (declare (dynamic-extent #'fn))
    (etypecase stream
      ((eql t) (fn *standard-output*))
      ((eql nil)
       (with-output-to-string (s)
         (fn s)))
      ((and string (not simple-vector))
       (with-output-to-string (s stream)
         (fn s)))
      (stream (fn stream)))))

(defmacro with-string ((var &optional stream) &body body)
  "Bind VAR to the character stream designated by STREAM.

STREAM is resolved like the DESTINATION argument to `format': it can
be any of t (for `*standard-output*'), nil (for a string stream), a
string with a fill pointer, or a stream to be used directly.

When possible, it is a good idea for functions that build strings to
take a stream to write to, so callers can avoid consing a string just
to write it to a stream. This macro makes it easy to write such
functions.

    (defun format-x (x &key stream)
      (with-string (s stream)
        ...))"
  (with-thunk (body var)
    `(call/string #',body ,stream)))

(defun collapse-whitespace (string)
  "Collapse runs of whitespace in STRING.
Each run of space, newline, and other whitespace characters is
replaced by a single space character."
  (check-type string string)
  (if (< (length string) 1)
      string
      (with-output-to-string (s)
        (write-char (aref string 0) s)
        (loop for i of-type array-length from 0
              for j of-type array-length from 1
                below (length string)
              for c1 = (aref string i)
              for c2 = (aref string j)
              do (if (whitespacep c2)
                     (unless (whitespacep c1)
                       (write-char #\Space s))
                     (write-char c2 s))))))

(defsubst blankp (seq)
  "SEQ is either empty, or consists entirely of characters that
satisfy `whitespacep'."
  (every #'whitespacep seq))

(-> simplify-string (string) (simple-array character (*)))
(defun simplify-string (string)
  (declare (string string))
  (coerce string '(simple-array character (*))))

(-> copy-string (string-designator) string)
(defun copy-string (x)
  (copy-seq (string x)))

(defsubst concat (&rest strings)
  "Abbreviation for (concatenate 'string ...).

From Emacs Lisp."
  (apply #'concatenate 'string strings))

(defun mapconcat/list (fun list sep stream)
  (declare (list list) (function fun) (string sep) (optimize speed))
  (loop for (elt . more?) on list
        do (write-string (funcall fun elt) stream)
        if more?
          do (write-string sep stream)))

(defun mapconcat/seq (fun seq sep stream)
  (declare (function fun) (string sep))
  (let ((i 0)
        (ult (1- (length seq))))
    (declare (type array-index i ult))
    (map nil
         (lambda (elt)
           (write-string (funcall fun elt) stream)
           (unless (= (prog1 i (incf i)) ult)
             (write-string sep stream)))
         seq)))

(defun mapconcat (fun seq separator &key stream)
  "Build a string by mapping FUN over SEQ.
Separate each value with SEPARATOR.

Equivalent to
        (reduce #'concat (intersperse SEP SEQ) :key FUN)
but more efficient.

STREAM can be used to specify a stream to write to. It is resolved
like the first argument to `format'.

From Emacs Lisp."
  (values
   (if (emptyp seq)
       (make-string 0)
       (let ((fun (ensure-function fun)))
         (check-type separator string)
         (with-string (stream stream)
           (seq-dispatch seq
             (mapconcat/list fun seq separator stream)
             (mapconcat/seq fun seq separator stream)))))))

(defun string-join (strings &optional (separator ""))
  "Like `(mapconcat #'string STRINGS (string SEPARATOR))'."
  (mapconcat #'string strings (string separator)))

(-> string-upcase-initials (string-designator) string)
(defun string-upcase-initials (string)
  "Return STRING with the first letter of each word capitalized.
This differs from STRING-CAPITALIZE in that the other characters in
each word are not changed.

     (string-capitalize \"an ACRONYM\") -> \"An Acronym\")
     (string-upcase-initials \"an ACRONYM\") -> \"An ACRONYM\")

From Emacs Lisp (where it is simply `upcase-initials')."
  (nstring-upcase-initials (copy-string string)))

(-> nstring-upcase-initials (string-designator) string)
(defun nstring-upcase-initials (string)
  "Destructive version of `string-upcase-initials'."
  (lret ((string (string string)))
    (when (= (length string) 0)
      (return-from nstring-upcase-initials
        string))
    (setf (aref string 0)
          (char-upcase (aref string 0)))
    (loop for i from 0
          for j from 1 below (length string)
          for x = (aref string i)
          for y = (aref string j)
          when (and (not (alphanumericp x))
                    (alphanumericp y))
            do (setf (aref string j)
                     (char-upcase y)))))

;;; https://groups.google.com/d/msg/comp.lang.lisp/EO1mZBtiXX0/JuuhKJ6eMHIJ
;;; https://groups.google.com/d/msg/comp.lang.lisp/0CSkbAd8NTg/UnHQf9YIf8kJ
(defun same-case-p (string)
  "Every character with case in STRING has the same case.
Return `:upper' or `:lower' as appropriate."
  (let ((string (string string)))
    (declare (optimize speed))
    (let ((length (length string)))
      (declare (array-length length))
      (nlet invert ((i 0)
                    (ucp nil)
                    (lcp nil))
        (declare (array-length i))
        (if (= i length)
            (cond ((eq ucp lcp) nil)
                  (ucp :upper)
                  (lcp :lower))
            (let ((char (char string i)))
              (cond ((upper-case-p char)
                     (invert (1+ i) t lcp))
                    ((lower-case-p char)
                     (invert (1+ i) ucp t))
                    (t (invert (1+ i) ucp lcp)))))))))

(-> nstring-invert-case (string-designator) string)
(defun nstring-invert-case (string)
  "Destructive version of `string-invert-case'."
  (let ((string (string string)))
    (case (same-case-p string)
      (:upper (nstring-downcase string))
      (:lower (nstring-upcase string))
      (t string))))

(-> string-invert-case (string-designator) string)
(defun string-invert-case (string)
  "Invert the case of STRING.
This does the same thing as a case-inverting readtable."
  (nstring-invert-case (copy-string string)))

(-> words (string &key (:start array-index) (:end (or array-index null)))
    list)
(defun words (string &key (start 0) end)
  "Split STRING into words.

The definition of a word is the same as that used by
`string-capitalize': a run of alphanumeric characters.

    (words \"Four score and seven years\")
    => (\"Four\" \"score\" \"and\" \"seven\" \"years\")

    (words \"2 words\")
    => (\"2\" \"words\")

    (words \"two_words\")
    => (\"two\" \"words\")

    (words \"\\\"I'm here,\\\" Tom said presently.\")
    => (\"I\" \"m\" \"here\" \"Tom\" \"said\" \"presently\")

Cf. `tokens'."
  (declare (string string)
           (optimize speed)
           (inline split-sequence-if-not))
  (values (split-sequence-if-not #'alphanumericp
                                 string
                                 :remove-empty-subseqs t
                                 :start start
                                 :end end)))

(-> tokens (string &key (:start array-index) (:end (or array-index null)))
    list)
(defun tokens (string &key (start 0) end)
  "Separate STRING into tokens.
Tokens are runs of non-whitespace characters.

    (tokens \"\\\"I'm here,\\\" Tom said presently.\")
    => (\"\\\"I'm\" \"here,\\\"\" \"Tom\" \"said\" \"presently.\")

Cf. `words'."
  (declare (string string))
  (values (split-sequence-if #'whitespacep
                             string
                             :remove-empty-subseqs t
                             :start start
                             :end end)))

(defun newline? (c)
  (declare (character c))
  (case c
    (#.(remove-duplicates (list #\Newline #\Return #\Linefeed))
     t)))

(-> lines (string) list)
(defun lines (string)
  "A list of lines in STRING."
  (declare (string string))
  (values (split-sequence-if #'newline? string :remove-empty-subseqs t)))

(defun fmt (control-string &rest args)
  "A cousin of `format` expressly for fast formatting of strings.

Like (format nil ...), binding `*pretty-pretty*' to `nil', which in
some Lisps means a significant increase in speed.

Has a compiler macro with `formatter'."
  (let ((*print-pretty* nil))
    (the string (format nil "~?" control-string args))))

(define-compiler-macro fmt (&whole decline control-string &rest args)
  (if (stringp control-string)
      `(fmt (formatter ,control-string) ,@args)
      decline))

(defun escape (string table &key (start 0) end stream)
  "Write STRING to STREAM, escaping with TABLE.

TABLE should be either a hash table, with characters for keys and
strings for values, or a function that takes a character and returns a
string.

STREAM can be used to specify a stream to write to, like the first
argument to `format'. The default behavior, with no stream specified,
is to return a string."
  (unless end
    (setf end (length string)))
  (with-string (stream stream)
    (fbind ((rep (if (functionp table)
                     table
                     (lambda (c)
                       (gethash c table)))))
      (nlet escape ((start start))
        (when (< start end)
          (let ((next (position-if #'rep string
                                   :start start
                                   :end end)))
            (if (not next)
                (write-string string stream :start start :end end)
                (progn
                  (write-string string stream :start start :end next)
                  (let ((escape (rep (char string next))))
                    (unless (emptyp escape)
                      (write-string escape stream))
                    (escape (1+ next)))))))))))

(-> ellipsize (string array-length &key (:ellipsis string)) string)
(defun ellipsize (string n &key (ellipsis "..."))
  "If STRING is longer than N, truncate it and append ELLIPSIS.

Note that the resulting string is longer than N by the length of
ELLIPSIS, so the string may come out longer than it started.

     (ellipsize \"abc\" 2)
     => \"ab...\"

From Arc."
  (if (> (length string) n)
      (concat (string-right-trim whitespace (subseq string 0 n))
              ellipsis)
      string))

(macrolet ((defcmp ((name1 name2) (s1 s2 &rest keys) &body body)
             (let ((docstring (if (stringp (car body)) (pop body))))
               (flet ((mkdef (name &key docstring)
                        `(defsubst ,name (,s1 ,s2 &key (start1 0) end1 (start2 0) end2 ,@keys)
                           ,@(unsplice docstring)
                           (let ((,s1 (string ,s1))
                                 (,s2 (string ,s2)))
                             (macrolet ((call (fun &rest args)
                                          `(,fun ,@args
                                                 :start1 start1 :start2 start2
                                                 :end1 end1 :end2 end2
                                                 :test test)))
                               ,@body)))))
                 `(progn
                    ,(subst '#'char= 'test (mkdef name1 :docstring docstring))
                    ,(subst '#'char-equal 'test
                            (let ((docstring (format nil "Like `~(~a~)', but case-insensitive." name1)))
                              (mkdef name2 :docstring docstring))))))))

  (defcmp (string^= string-prefixp) (s1 s2)
    "Is S1 a prefix of S2?"
    (let ((ms (call mismatch s1 s2)))
      (or (not ms) (= ms (or end1 (length s1))))))

  (defcmp (string$= string-suffixp) (s1 s2)
    "Is S1 a suffix of S2?"
    (and (<= (length s1) (length s2))
         (let ((ms (call mismatch s1 s2 :from-end t)))
           (or (not ms) (= ms start1)))))

  (defcmp (string*= string-containsp) (s1 s2)
    "Is S1 a substring of S2?

This is similar, but not identical, to SEARCH.

     (search nil \"foo\") => 0
     (search \"nil\" \"nil\") => 0
     (string*= nil \"foo\") => NIL
     (string*= nil \"nil\") => T"
    (call search s1 s2))

  (defcmp (string~= string-tokenp) (s1 s2)
    "Does S1 occur in S2 as a token?

Equivalent to
     (find S1 (tokens S2) :test #'string=),
but without consing."
    ;; Adapted from split-sequence.
    (let ((length (length s2))
          (end (or end2 (length s2))))
      (declare (array-length length end))
      (macrolet ((compare-segment (left right)
                   `(not (mismatch s1 s2 :start1 start1 :end1 end1
                                         :start2 ,left :end2 ,right
                                         :test test))))
        (loop for left of-type array-length
                = start2
                  then (+ right 1)
              for right of-type array-length
                = (min (or (position-if #'whitespacep s2 :start left) length)
                       end)
                  thereis (and (not (= right left))
                               (compare-segment left right))
              until (>= right end))))))

(defun string-replace-all (old string new &key (start 0) end stream)
  "Do regex-style search-and-replace for constant strings.

Note that START and END only affect where the replacements are made:
the part of the string before START, and the part after END, are
always included verbatim.

     (string-replace-all \"old\" \"The old old way\" \"new\"
                         :start 3 :end 6)
     => \"The new old way\"

STREAM can be used to specify a stream to write to. It is resolved
like the first argument to `format'."
  (declare (array-length start)
           ((or array-length null) end))
  (check-type old string)
  (check-type new string)
  (check-type string string)
  (unless (search old string :start2 start :end2 end)
    (return-from string-replace-all string))
  (let* ((end (or end (length string)))
         (len (length old)))
    (declare (array-length len))
    (with-string (s stream)
      (unless (zerop start)
        (write-string string s :start 0 :end start))
      (nlet rep ((start start))
        (declare (array-length start))
        (let ((match (search old string :start2 start :end2 end)))
          (declare ((or array-length null) match))
          (if (not match)
              ;; No end, because we want the whole remainder of the
              ;; string.
              (write-string string s :start start)
              (progn
                (write-string string s :start start :end match)
                (write-string new s)
                (rep (+ match len)))))))))
