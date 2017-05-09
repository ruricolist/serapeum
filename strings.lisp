(in-package :serapeum)

(deftype string-offset ()
  '(integer #.(- array-dimension-limit) #.array-dimension-limit))

(defconst no-break-space
  #-(or abcl lispworks) #\No-break_space
  #+(or abcl lispworks) (code-char 160))

(defconst whitespace
  #.(remove-duplicates
     (coerce (list #\Space #\Tab #\Linefeed #\Return #\Newline #\Page no-break-space)
             'string))
  "Whitespace characters.")

(defsubst whitespacep (char)
  "Is CHAR whitespace?

Spaces, tabs, any kind of line break, page breaks, and no-break spaces
are considered whitespace."
  (case (char-code char)
    (#.(map 'list #'char-code whitespace) t)))

(defsubst trim-whitespace (string)
  "STRING without whitespace at ends."
  (string-trim whitespace string))

(defsubst ascii-char-p (char)
  "Is CHAR an ASCII char?"
  (and (< (char-code char) 128)
       char))

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
  (when (constantp stream)
    (let ((stream (eval stream)))
      (cond ((eql stream t)
             (return-from with-string
               `(let ((,var *standard-output*))
                  ,@body)))
            ((null stream)
             (return-from with-string
               `(with-output-to-string (,var)
                  ,@body))))))

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
        (with-string-dispatch () string
          (write-char (vref string 0) s)
          (loop for i of-type array-length from 0
                for j of-type array-length from 1
                  below (length string)
                for c1 = (vref string i)
                for c2 = (vref string j)
                do (if (whitespacep c2)
                       (unless (whitespacep c1)
                         (write-char #\Space s))
                       (write-char c2 s)))))))

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
  (if (emptyp seq)
      (make-string 0)
      (let ((i 0)
            (ult (1- (length seq))))
        (declare (type array-index i ult))
        (map nil
             (lambda (elt)
               (write-string (funcall fun elt) stream)
               (unless (= (prog1 i (incf i)) ult)
                 (write-string sep stream)))
             seq))))

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
    (with-string-dispatch () string
      (when (= (length string) 0)
        (return-from nstring-upcase-initials
          string))
      (setf (vref string 0)
            (char-upcase (vref string 0)))
      (loop for i from 0
            for j from 1 below (length string)
            for x = (vref string i)
            for y = (vref string j)
            when (and (not (alphanumericp x))
                      (alphanumericp y))
              do (setf (vref string j)
                       (char-upcase y))))))

;;; https://groups.google.com/d/msg/comp.lang.lisp/EO1mZBtiXX0/JuuhKJ6eMHIJ
;;; https://groups.google.com/d/msg/comp.lang.lisp/0CSkbAd8NTg/UnHQf9YIf8kJ
(defun same-case-p (string)
  "Every character with case in STRING has the same case.
Return `:upper' or `:lower' as appropriate."
  (let ((string (string string)))
    (with-string-dispatch () string
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
              (let ((char (vref string i)))
                (cond ((upper-case-p char)
                       (invert (1+ i) t lcp))
                      ((lower-case-p char)
                       (invert (1+ i) ucp t))
                      (t (invert (1+ i) ucp lcp))))))))))

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

(defun word-wrap (string &key (column 80) stream)
  "Return a word-wrapped version of STRING that breaks at COLUMN.

Note that this is not a general-purpose word-wrapping routine like you
would find in a text editor: in particular, any existing whitespace is
removed."
  (let ((col 0))
    (with-string (s stream)
      (loop for (token . more) on (tokens string) do
        (flet ((reset ()
                 (setq col 0)
                 (terpri s))
               (output-word (word)
                 (write-string word s)
                 (incf col (length word))
                 (when more
                   (write-char #\Space s)
                   (incf col))))
          (let ((projected-length (+ col (length token))))
            (if (<= projected-length column)
                (output-word token)
                (progn
                  (reset)
                  (output-word token)))))))))

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

(-> fmt ((or string function) &rest t) string)
(defun fmt (control-string &rest args)
  "A cousin of `format` expressly for fast formatting of strings.

Like (format nil ...), binding `*pretty-pretty*' to `nil', which in
some Lisps means a significant increase in speed.

Has a compiler macro with `formatter'."
  (declare (dynamic-extent args))
  (let ((*print-pretty* nil))
    (the string (format nil "~?" control-string args))))

(define-compiler-macro fmt (control-string &rest args)
  ;; NB We want to expand into a call to `format' whenever possible,
  ;; so Lisp can check the number of arguments.
  `(the string
        ,(if (stringp control-string)
             ;; Optimize some trivial control strings. You wouldn't
             ;; necessarily write these, but it's common for a complex
             ;; control string to "erode" over time into something
             ;; trivial.
             (cond
               ;; No directives.
               ((not (find #\~ control-string))
                `(copy-seq ,control-string))
               ;; Same as `princ'.
               ((member control-string '("~a" "~d" "~f" "~g")
                        :test #'equalp)
                (destructuring-bind (arg) args
                  `(let (*print-pretty*)
                     (princ-to-string ,arg))))
               ;; Same as `prin1'.
               ((equalp control-string "~s")
                (destructuring-bind (arg) args
                  `(let (*print-pretty*)
                     (prin1-to-string ,arg))))
               (t
                `(let (*print-pretty*)
                   (format nil (formatter ,control-string) ,@args))))
             `(let (*print-pretty*)
                (format nil ,control-string ,@args)))))

(defun escape (string table &key (start 0) end stream)
  "Write STRING to STREAM, escaping with TABLE.

TABLE should be either a hash table, with characters for keys and
strings for values, or a function that takes a character and
returns (only) either a string or null.

That is, the signature of TABLE should be:

    (function (character) (or string null))

where `nil' means to pass the character through unchanged.

STREAM can be used to specify a stream to write to, like the first
argument to `format'. The default behavior, with no stream specified,
is to return a string."
  (declare (string string)
           ((or function hash-table) table)
           (array-index start)
           ((or array-index null) end)
           (optimize (safety 1) (debug 0)))
  (unless end
    (setf end (length string)))
  (with-string (stream stream)
    (with-type-dispatch (function hash-table) table
      (with-string-dispatch () string
        (flet ((rep (c)
                 (etypecase table
                   (function (funcall table c))
                   (hash-table (gethash c table)))))
          (declare (inline rep))
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
                        (escape (1+ next)))))))))))))

(-> ellipsize (string array-length &key (:ellipsis string)) string)
(defun ellipsize (string n &key (ellipsis "..."))
  "If STRING is longer than N, truncate it and append ELLIPSIS.

Note that the resulting string is longer than N by the length of
ELLIPSIS, so if N is very small the string may come out longer than it
started.

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
                           (declare (array-length start1 start2))
                           (let* ((,s1 (string ,s1))
                                  (,s2 (string ,s2))
                                  (end1 (or end1 (length ,s1)))
                                  (end2 (or end2 (length ,s2))))
                             (declare (array-length end1 end2))
                             (declare (string ,s1 ,s2))
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

  (defcmp (string^= string-prefix-p) (prefix string)
    "Is PREFIX a prefix of STRING?"
    (let ((ms (call mismatch prefix string)))
      (or (not ms) (= ms end1))))

  ;; Optimization: the prefix is a single character.
  (macrolet ((dcm (name test)
               `(define-compiler-macro ,name (&whole call prefix string &rest args)
                  (if args call
                      (if (and (stringp prefix)
                               (= (length prefix) 1))
                          `((lambda (s)
                              (and (plusp (length s))
                                   (,',test ,(character prefix)
                                     (aref s 0))))
                            (string ,string))
                          call)))))
    (dcm string^= char=)
    (dcm string-prefix-p char-equal))

  (defcmp (string$= string-suffix-p) (suffix string)
    "Is SUFFIX a suffix of STRING?"
    (let ((ms (call mismatch suffix string :from-end t)))
      (or (not ms) (= ms start1))))

  ;; Optimization: the suffix is a single character.
  (macrolet ((dcm (name test)
               `(define-compiler-macro ,name (&whole call suffix string &rest args)
                  (if args call
                      (if (and (stringp suffix)
                               (= (length suffix) 1))
                          `((lambda (s)
                              (and (plusp (length s))
                                   (,',test ,(character suffix)
                                     (aref s (1- (length s))))))
                            (string ,string))
                          call)))))
    (dcm string$= char=)
    (dcm string-suffix-p char-equal))

  (defcmp (string*= string-contains-p) (substring string)
    "Is SUBSTRING a substring of STRING?

This is similar, but not identical, to SEARCH.

     (search nil \"foo\") => 0
     (search \"nil\" \"nil\") => 0
     (string*= nil \"foo\") => NIL
     (string*= nil \"nil\") => T"
    (call search substring string))

  ;; Optimization: the substring is a single character.
  (macrolet ((dcm (name test)
               `(define-compiler-macro ,name (&whole call substring string &rest args)
                  (if args call
                      (if (and (stringp substring)
                               (= (length substring) 1))
                          `(position ,(character substring) (string ,string) :test #',',test)
                          call)))))
    (dcm string*= char=)
    (dcm string-contains-p char-equal))

  (defcmp (string~= string-token-p) (token string)
    "Does TOKEN occur in STRING as a token?

Equivalent to
     (find TOKEN (tokens STRING) :test #'string=),
but without consing."
    ;; Adapted from split-sequence.
    (let ((len (length string))
          (end end2))
      (declare (array-length len end))
      (macrolet ((compare-segment (left right)
                   `(not (mismatch token string :start1 start1 :end1 end1
                                                :start2 ,left :end2 ,right
                                                :test test))))
        (loop for left of-type array-length
                = start2
                  then (+ right 1)
              for right of-type array-length
                = (min (or (position-if #'whitespacep string :start left) len)
                       end)
                  thereis (and (not (= right left))
                               (compare-segment left right))
              until (>= right end))))))

(defun string-replace (old string new &key (start 0) end stream)
  "Like `string-replace-all', but only replace the first match."
  (string-replace-all old string new
                      :start start
                      :end end
                      :stream stream
                      :count 1))

(defun string-replace-all (old string new &key (start 0) end stream count)
  "Do search-and-replace for constant strings.

Note that START and END only affect where the replacements are made:
the part of the string before START, and the part after END, are
always included verbatim.

     (string-replace-all \"old\" \"The old old way\" \"new\"
                         :start 3 :end 6)
     => \"The new old way\"

COUNT can be used to limit the maximum number of occurrences to
replace. If COUNT is not specified, every occurrence of OLD between
START and END is replaced with NEW.

    (string-replace-all \"foo\" \"foo foo foo\" \"quux\")
    => \"quux quux quux\"

    (string-replace-all \"foo\" \"foo foo foo\" \"quux\" :count 2)
    => \"quux quux foo\"

STREAM can be used to specify a stream to write to. It is resolved
like the first argument to `format'."
  (declare (array-length start)
           ((or array-length null) end)
           ;; Can't be more matches than characters.
           ((or array-length null) count))
  (check-type old string)
  (check-type new string)
  (check-type string string)
  (let ((new (simplify-string new))
        (old (simplify-string old)))
    (with-string-dispatch () string
      (cond
        ((not (search old string :start2 start :end2 end))
         string)
        ((and count (zerop count))
         string)
        ;; The use case in mind here is one where you have a list of
        ;; substitutions.
        ((and (= (length new) 1)
              (= (length old) 1))
         (substitute new old string :start start :end end :count count))
        (t
         (let* ((end (or end (length string)))
                (len (length old)))
           (declare (array-length len))
           (with-string (s stream)
             (unless (zerop start)
               (write-string string s :start 0 :end start))
             (nlet rep ((start start)
                        (count (or count (1- array-dimension-limit))))
               (declare (array-length start count))
               (let ((match (search old string :start2 start :end2 end)))
                 (declare ((or array-length null) match))
                 (if (or (not match) (zerop count))
                     ;; No end, because we want the whole remainder of the
                     ;; string.
                     (write-string string s :start start)
                     (progn
                       (write-string string s :start start :end match)
                       (write-string new s)
                       (rep (+ match len)
                            (1- count)))))))))))))

(defun chomp (string
              &optional
                (suffixes '#.(sort
                              (remove-duplicates
                               (list (string #\Newline)
                                     (string #\Linefeed)
                                     (string #\Return)
                                     (coerce (list #\Return #\Linefeed) 'string))
                               :test 'equal)
                              #'> :key #'length)
                          suffixes-supplied-p))
  "If STRING ends in one of SUFFIXES, remove that suffix.

SUFFIXES defaults to a Lisp newline, a literal line feed, a literal
carriage return, or a literal carriage return followed by a literal
line feed.

Takes care that the longest suffix is always removed first."
  (check-type string string)
  (check-type suffixes list)
  (reduce (lambda (string sep)
            (if (string$= sep string)
                (subseq string 0 (- (length string) (length sep)))
                string))
          (if suffixes-supplied-p
              (sort (copy-seq suffixes) #'> :key #'length)
              suffixes)
          :initial-value string))

(defun string-count (substring string &key (start 0) end)
  "Count how many times SUBSTRING appears in STRING."
  (declare (array-length start)
           ((or array-length null) end))
  (let* ((substring (simplify-string (string substring)))
         (string (string string))
         (end (or end (length string)))
         (len (length substring)))
    (with-string-dispatch () string
      (nlet rec ((start start)
                 (hits 0))
        ;; There can't be more hits than characters.
        (declare (type array-length hits))
        (let ((match (search substring string :start2 start :end2 end)))
          (if (not match)
              hits
              (rec (+ match len) (1+ hits))))))))

(declaim (ftype (function (&rest t) string) string+))
(defun string+ (&rest args)
  "Optimized function for building small strings.

Roughly equivalent to

    (let ((*print-pretty* nil))
     (format nil \"~@{~a}\" args...))

But with a compiler macro that can sometimes result in more efficient
code."
  (declare (dynamic-extent args))
  (let ((*print-pretty* nil))
    (with-output-to-string (s)
      (dolist (arg args)
        (typecase arg
          (string (write-string arg s))
          (character (write-char arg s))
          (symbol (write-string (symbol-name arg) s))
          (t (princ arg s)))))))

(defun simplify-string-plus-args (args)
  (reduce (lambda (x args)
            (if (and (stringp x)
                     (stringp (car args)))
                (cons (concat x (car args))
                      (cdr args))
                (cons x args)))
          args
          :from-end t
          :key (lambda (arg)
                 (trivia:match arg
                   ((and arg (type character))
                    (string arg))
                   ((and arg (type keyword))
                    (symbol-name arg))
                   ((eql t) #.(string 't))
                   ((eql nil) #.(string 'nil))
                   ;; The smallest base is 2, so these are always the
                   ;; same regardless of `*print-base*'.
                   ((eql 0) "0")
                   ((eql 1) "1")
                   ((list 'quote (and s (type symbol)))
                    (symbol-name s))
                   ((list 'quote (and s (type string)))
                    s)
                   ((list 'quote (and c (type character)))
                    (string c))
                   (otherwise arg)))
          :initial-value '()))

(define-compiler-macro string+ (&whole call &rest args)
  (if (null args)
      `(make-string 0)
      (let ((args (simplify-string-plus-args args)))
        (if (> (length args) 20) call
            (if (= (length args) 1)
                (if (stringp (first args))
                    `(copy-seq ,(first args))
                    `(princ-to-string ,(first args)))
                ;; If the arguments are reasonably few, unroll the
                ;; loop.
                (with-unique-names (stream)
                  `(let ((*print-pretty* nil))
                     (with-output-to-string (,stream)
                       ,@(loop for arg in args
                               if (stringp arg)
                                 collect `(write-string ,arg ,stream)
                               else
                                 collect `(princ ,arg ,stream))))))))))
