(in-package :serapeum)

(deftype string-offset ()
  '(integer #.(- array-dimension-limit) #.array-dimension-limit))

(defconst no-break-space
  #-(or abcl lispworks) #\No-break_space
  #+(or abcl lispworks) (code-char 160))

(defconst whitespace
  (remove-duplicates
   (coerce (list #\Space #\Tab #\Linefeed #\Return #\Newline #\Page
                 #\Vt                   ;Vertical tab.
                 no-break-space)
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
    (etypecase stream
      ((eql t) (fn *standard-output*))
      ((eql nil)
       (with-output-to-string (s)
         (fn s)))
      ((and string (not simple-string))
       (with-output-to-string (s stream)
         (fn s)))
      (output-stream (fn stream)))))

(defmacro with-string ((var &optional stream
                        &key (element-type nil element-type-supplied?))
                       &body body)
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
  (declare #+sbcl (sb-ext:muffle-conditions style-warning))
  (when (constantp stream)
    (let ((stream (eval stream)))
      (cond ((eql stream t)
             (return-from with-string
               `(let ((,var *standard-output*))
                  ,@body)))
            ((null stream)
             (return-from with-string
               `(with-output-to-string (,var
                                        ,@(and element-type-supplied?
                                               `(:element-type ,element-type)))
                  ,@body))))))

  (with-thunk ((body :name with-string) var)
    `(call/string #',body ,stream)))

(defsubst blankp (seq)
  "SEQ is either empty, or consists entirely of characters that
satisfy `whitespacep'."
  (every #'whitespacep seq))

(defun collapse-whitespace (string &key (space #\Space) stream)
  "Collapse runs of whitespace in STRING.
Each run of space, newline, and other whitespace characters is
replaced by a single space character (or SPACE, if that is specified)."
  (declare (inline position whitespacep))
  (check-type string string)
  (check-type space character)
  (with-string (s stream)
    (with-string-dispatch () string
      (let ((len (length string)))
        (nlet rec ((i 0))
          (unless (= i len)
            (let ((char (vref string i)))
              (cond ((whitespacep char)
                     (write-char space s)
                     (rec (or (position-if-not #'whitespacep string
                                               :start i)
                              len)))
                    (t
                     (let ((j (or (position-if #'whitespacep string
                                               :start i)
                                  len)))
                       (write-string string s :start i :end j)
                       (rec j)))))))))))

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

(defun mapconcat/list (fun list sep stream end?)
  (declare (list list) (function fun) (string sep) (optimize speed))
  (if end?
      (loop for elt in list
            do (write-string (funcall fun elt) stream)
               (write-string sep stream))
      (loop for (elt . more?) on list
            do (write-string (funcall fun elt) stream)
            if more?
              do (write-string sep stream))))

(defun mapconcat/seq (fun seq sep stream end?)
  (declare (function fun) (string sep))
  (if end?
      (if (emptyp seq)
          (copy-seq sep)
          (do-each (elt seq)
            (write-string (funcall fun elt) stream)
            (write-string sep stream)))
      (if (emptyp seq)
          (make-string 0)
          (let ((i 0)
                (ult (1- (length seq))))
            (declare (type array-index i ult))
            (do-each (elt seq)
              (write-string (funcall fun elt) stream)
              (unless (= (prog1 i (incf i)) ult)
                (write-string sep stream)))))))

(defun mapconcat (fun seq separator &key stream end)
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
       (if end
           (copy-seq (string separator))
           (make-string 0))
       (let ((fun (ensure-function fun))
             (separator (string separator)))
         (with-string (stream stream)
           (seq-dispatch seq
             (mapconcat/list fun seq separator stream end)
             (mapconcat/seq fun seq separator stream end)))))))

(defun string-join (strings separator &key stream end)
  "Join strings in STRINGS, separated by SEPARATOR.

SEPARATOR can be any string designator.

If STREAM is provided, write to STREAM rather than returning a string.

If END is provided, then insert SEPARATOR after the last string, as
well as between strings.

Equivalent to `(mapconcat #'string STRINGS SEPARATOR)'."
  (if stream
      (with-string (s stream)
        (mapconcat #'string strings separator :stream s)
        (when end
          (write-string (string separator) s)))
      (let* ((separator (coerce (string separator)
                                '(simple-array character (*))))
             (sep-len (length separator))
             (separator? (not (zerop sep-len))))
        (macrolet ((do-strings ((s more?) &body body)
                     (with-unique-names (i last)
                       `(if (listp strings)
                            (loop for (,s . ,more?) on strings
                                  do (progn ,@body))
                            (let ((,last (max 0 (1- (length strings))))
                                  (,i 0))
                              (declare (array-index ,i ,last))
                              (do-each (,s strings)
                                (let ((,more? (< ,i ,last)))
                                  (declare (ignorable ,more?))
                                  ,@body)
                                (incf ,i)))))))
          (with-boolean (separator?)
            (let ((len 0))
              (declare (array-length len))
              (locally (declare (optimize speed))
                (do-strings (s more?)
                  (etypecase s
                    (string (incf len (length s)))
                    (character (incf len 1))
                    (symbol (incf len (length (symbol-name s)))))
                  (boolean-when separator?
                    (when more?
                      (incf len sep-len))))
                (boolean-when separator?
                  (when end
                    (incf len sep-len)))
                (lret ((start 0)
                       (result (make-array len :element-type 'character)))
                  (declare (array-index start)
                           ((simple-array character (*)) result))
                  (do-strings (s more?)
                    (nlet rec (s)
                      (etypecase s
                        (string
                         (with-string-dispatch () s
                           (replace result s :start1 start)
                           (incf start (length s))))
                        (character
                         (setf (schar result start) s)
                         (incf start))
                        (symbol (rec (symbol-name s)))))
                    (boolean-when separator?
                      (when more?
                        (replace result separator :start1 start)
                        (incf start sep-len))))
                  (boolean-when separator?
                    (when end
                      (replace result separator :start1 start)))))))))))

(-> string-upcase-initials (string-designator) (values string &optional))
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
This does the same thing as a case-inverting readtable:
- If the string is uppercase, downcase the string.
- If the string is lowercase, upcase the string.
- If the string is mixed-case, leave it alone."
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
           (optimize speed))
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
               (output-word (word space?)
                 (write-string word s)
                 (incf col (length word))
                 (when space?
                   (write-char #\Space s)
                   (incf col))))
          (let ((projected-length (+ col (length token))))
            (cond ((= projected-length column)
                   (output-word token nil))
                  ((< projected-length column)
                   (output-word token t))
                  (t
                   (reset)
                   (output-word token t)))))))))

(-> lines ((or null string)
           &key
           (:eol-style (or (member nil :cr :lf :crlf :ascii :unicode)
                           function))
           (:honor-crlf t)
           (:keep-eols t)
           (:count (or null (integer 0 *))))
    (values (or null list) &optional))
(defun lines (string &key eol-style (honor-crlf nil honor-crlf-p) keep-eols count)
  "Return a list of the lines in STRING, stripped of any EOL characters
and including the last nonempty line even if it has no EOL characters,
or NIL if STRING is empty or NIL.

If COUNT is provided, only the first COUNT lines are returned.

EOL-STYLE can be one of the following:

- NIL, the default, which means split on #\\Newline.
- :CR, which means split on CR, i.e., #\\Return.
- :LF, which means split on LF, i.e., #\\Linefeed.
- :CRLF, which means split on CRLF, i.e., #\\Return followed by
  #\\Linefeed.
- :ASCII, which means split on any of CR, LF, and CRLF.
- :UNICODE, which means split on any of the newlines described in
  Section 5.8, \"Newline Guidelines\", of the Unicode Standard,
  available at http://www.unicode.org/versions/latest/.
  These newlines are CR, LF, CRLF, next line, vertical tab, form feed,
  line separator, and paragraph separator.
- A predicate that accepts one CHARACTER and returns non-NIL if the
  CHARACTER should be split on, NIL otherwise.

:CR, :LF, :CRLF, and :ASCII assume that the Common Lisp implementation
represents CHARACTERs internally as ASCII or one of its supersets
\(e.g., extended ASCII), and :UNICODE assumes that it represents them
internally as Unicode \(which is also a superset of ASCII).
Additionally, all of the EOL-STYLEs just mentioned assume that #\\Newline
is either #\\Return or #\\Linefeed \(which can be reasonably expected).

If HONOR-CRLF is supplied, it overrides EOL-STYLE's interpretation of
CRLF except if EOL-STYLE is NIL or :CRLF, in which case HONOR-CRLF has
no effect.
\(The :CRLF, :ASCII and :UNICODE EOL-STYLEs honor CRLF by default; the
rest do not.)

If KEEP-EOLS is non-NIL, LINES does not strip the EOL characters from
the lines.

Note that Common Lisp implementations may convert some or all of CR, LF,
and CRLF to #\\Newline when reading from file streams, which causes
LINES to split the contents of files differently across implementations.
:CR, :LF, and :CRLF are suitable only when STRING's lines certainly end
with the corresponding EOL character, but if STRING originates from a
file stream, LINES splits nothing unless the corresponding EOL character
is the same as #\\Newline, in which case LINES behaves as if EOL-STYLE
were NIL \(and indeed NIL is preferable to :CR, :LF, and :CRLF, though
not to :ASCII and :UNICODE).

:UNICODE and :ASCII are the preferred EOL-STYLEs, the former to be
maximally portable and correct, and the latter when Unicode is inapt.
With either EOL-STYLE, LINES splits the entire contents of files
correctly only when the Common Lisp implementation converts only CR,
only LF, or all of CR, LF, and CRLF, to #\\Newline \(and when it
converts only CR or only LF, #\\Newline must the same as the EOL
character in question).
Again with either EOL-STYLE, LINES splits the lines of files, read with
READ-LINE, correctly only when the implementation converts only LF or
all of CR, LF, and CRLF to #\\Newline \(which must be #\\Linefeed).
\(Note the lack of the only-CR case when reading files line by line.)
However, any incorrect behavior with :ASCII and :UNICODE is limited to
LINES returning too many or too few empty lines.
The former -- which is uncorrectable -- can occur when CR and LF are
converted, but not CRLF, and the latter -- which can be corrected by
supplying HONOR-CRLF as NIL -- when CR and CRLF are converted \(to
#\\Return), but not LF, or when LF and CRLF are converted \(to
#\\Linefeed), but not CR.

For example, to split lines on LF and CRLF \(eschewing the recommended
:ASCII and :UNICODE) when the Common Lisp implementation converts only
LF to #\\Newline \(which must be #\\Linefeed), which is the same
behavior as Rust's std::io::BufRead.lines
\(https://doc.rust-lang.org/std/io/trait.BufRead.html#method.lines) and
Go's bufio.ScanLines \(https://golang.org/pkg/bufio/#ScanLines):

    #.(ecase #\\Newline (#\\Linefeed))
    (let ((string (coerce '(#\\a #\\Return
                            #\\b #\\Linefeed
                            #\\c #\\Return #\\Linefeed
                            #\\d)
                          'string)))
      (serapeum:lines string :eol-style :lf :honor-crlf t))
    => (\"a^Mb\" \"c\" \"d\")
    ;; where ^M is #\\Return.

\(EOL-STYLE cannot be NIL here because otherwise HONOR-CRLF would have
no effect.)

To split lines in the same way as Python's str.splitlines
\(https://docs.python.org/3/library/stdtypes.html#str.splitlines) when
the Common Lisp implementation converts only CR, only LF, or all of CR,
LF, and CRLF, to #\\Newline \(as previously described), but also keeping
the EOL characters in order to know what they were:

    #.(ecase #\\Newline ((#\\Return #\\Linefeed)))
    ;; Omit file separator from the example because its textual
    ;; representation (^\\) can confuse documentation browsers.
    (let ((string (coerce '(#\\a #.(code-char #x001D)
                            #\\b #.(code-char #x001E)
                            #\\c)
                          'string)))
      (serapeum:lines
       string
       :eol-style (lambda (c)
                    (serapeum:in
                     c #\\Return #\\Linefeed
                     #.(code-char #x000B)   ; #\\Vt (vertical tab)
                     #\\Page                 ; Form feed
                     #.(code-char #x001C)   ; #\\Fs (file separator)
                     #.(code-char #x001D)   ; #\\Gs (group separator)
                     #.(code-char #x001E)   ; #\\Rs (record separator)
                     #.(code-char #x0085)   ; Next line
                     #.(code-char #x2028)   ; #\\Line_Separator
                     #.(code-char #x2029))) ; #\\Paragraph_Separator
       :honor-crlf t
       :keep-eols t))
    => (\"a^]\" \"b^^\" \"c\")
    ;; where ^] is group separator and ^^ is record separator.

To omit empty lines \(thus uniformizing LINES's behavior across Common
Lisp implementations):

    #.(ecase #\\Newline ((#\\Return #\\Linefeed)))
    (let ((string (coerce '(#\\a #\\b #\\c
                            #\\Return #\\Return #\\Linefeed #\\Linefeed
                            #\\z)
                          'string)))
      (delete-if #'uiop:emptyp (serapeum:lines string :eol-style :unicode)))
    => (\"abc\" \"z\")

To additionally omit lines consisting only of whitespace:

    #.(ecase #\\Newline ((#\\Return #\\Linefeed)))
    (let ((string (coerce '(#\\a #\\b #\\c
                            #\\Return #\\Return #\\Linefeed #\\Linefeed
                            #\\Space #\\Linefeed
                            #\\Tab #\\Linefeed
                            #\\z)
                          'string)))
      (delete-if #'uiop:emptyp
                 (mapcar #'serapeum:trim-whitespace
                         (serapeum:lines string :eol-style :unicode))))
    => (\"abc\" \"z\")"
  (let* ((honor-crlf (cond ((not eol-style) nil)
                           ((eql eol-style :crlf))
                           (honor-crlf-p honor-crlf)
                           ;; If HONOR-CRLF was supplied, it takes
                           ;; precedence over these.
                           ((in eol-style :ascii :unicode))))
         ;; To honor CRLF, we must search for #\Return, but that does
         ;; not necessarily mean that we should split on it.
         (ignore-cr (in eol-style :lf :crlf))
         (cr-p (lambda (c) (eql c #\Return)))
         (cr-or-lf-p (lambda (c) (in c #\Return #\Linefeed)))
         (eolp (etypecase eol-style
                 ((or null keyword)
                  (ecase eol-style
                    ((nil) (lambda (c) (eql c #\Newline)))
                    ((:cr :crlf) cr-p)
                    (:lf (if honor-crlf
                             cr-or-lf-p
                             (lambda (c) (eql c #\Linefeed))))
                    (:ascii cr-or-lf-p)
                    (:unicode
                     (lambda (c)
                       (in c #\Return #\Linefeed
                           #.(code-char #x0085) ; Next line
                           #.(code-char #x000B) ; #\Vt (vertical tab)
                           #\Page               ; Form feed
                           #.(code-char #x2028) ; #\Line_Separator
                           #.(code-char #x2029)))))) ; #\Paragraph_Separator
                 (function
                  (if (and honor-crlf
                           ;; Do not split on #\Return if EOL-STYLE does
                           ;; not already include it.
                           (setf ignore-cr (not (funcall eol-style #\Return))))
                      (disjoin eol-style cr-p)
                      eol-style)))))
    (flet ((next-eol (start) (position-if eolp string :start start)))
      (do* ((length (length string))
            (line nil (subseq string start
                              (if keep-eols (1+ end) (- end crlf-offset))))
            (lines nil (push line lines))
            (line-count 0 (1+ line-count))
            (start 0 (1+ end))
            (end (next-eol start) (next-eol start))
            (crlf-offset 0 0))
           ((or (not end)
                (eql line-count count))
            (if (eql line-count count)
                (nreverse lines)
                (nreverse (if (emptyp (setf line (subseq string start)))
                              lines
                              (cons line lines)))))
       again
        (when (and (eql (char string end) #\Return)
                   honor-crlf)
          (let ((end+1 (1+ end)))
            (if (< end+1 length)
                (if (eql (char string end+1) #\Linefeed)
                    (setf end end+1
                          crlf-offset 1)
                    (when ignore-cr
                      (if (setf end (next-eol end+1))
                          (go again)
                          (return (nreverse (push (subseq string start)
                                                  lines))))))
                (when ignore-cr
                  (return (nreverse (push (subseq string start)
                                          lines)))))))))))

(-> fmt ((or string function) &rest t) string)
(defun fmt (control-string &rest args)
  "A cousin of `format` expressly for fast formatting of strings.

Like (format nil ...), binding `*print-pretty*' to `nil', which in
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
               ((equalp control-string "~a")
                (destructuring-bind (arg) args
                  `(write-to-string
                    ,arg
                    :pretty nil
                    :escape nil
                    :readably nil)))
               ;; Same as `prin1'.
               ((equalp control-string "~s")
                (destructuring-bind (arg) args
                  `(write-to-string
                    ,arg
                    :pretty nil
                    :escape t)))
               ((equalp control-string "~s")
                (destructuring-bind (arg) args
                  `(write-to-string
                    ,arg
                    :pretty nil
                    :escape nil
                    :radix nil
                    :base 10
                    :readably nil)))
               (t
                `(let (*print-pretty*)
                   (format nil ,control-string ,@args))))
             `(let (*print-pretty*)
                (format nil ,control-string ,@args)))))

(defun escape/no-arg-parsing (string table start end stream)
  (declare (string string)
           ((or function hash-table) table)
           ((or array-index null) start)
           ((or array-length null) end)
           (optimize (debug 0) (safety 1)
                     (compilation-speed 0)
                     (space 0))
           ;; Suppress unreachable code warnings.
           #+sbcl (sb-ext:muffle-conditions sb-ext:code-deletion-note))
  (let ((start (or start 0))
        (end (or end (length string))))
    (declare (array-index start)
             (array-length end))
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
                        (let ((escape (rep (vref string next))))
                          (unless (emptyp escape)
                            (write-string escape stream))
                          (escape (1+ next))))))))))))))

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
  (escape/no-arg-parsing string table
                         start
                         end
                         stream))

(define-compiler-macro escape (string table &key (start 0) end stream)
  `(escape/no-arg-parsing ,string
                          ,table
                          ,start
                          ,end
                          ,stream))

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

;;; TODO These are frequently used; it might be worth rewriting them
;;; to use `with-string-dispatch' instead of mismatch.

(macrolet ((defcmp ((name1 name2) (s1 s2 &rest keys) &body body)
             (let ((docstring (if (stringp (car body)) (pop body))))
               (flet ((mkdef (name &key docstring)
                        `(defsubst ,name (,s1 ,s2 &key (start1 0) end1 (start2 0) end2 ,@keys)
                           ,@(unsplice docstring)
                           (declare (array-length start1 start2))
                           (declare (inline mismatch))
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
    (assert (not (or (whitespacep (first-elt token))
                     (whitespacep (last-elt token))))
      () "Token must not begin or end with whitespace: ~a" token)
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

(-> string-replace-all
  (string string string
          &key (:start array-index)
          (:end (or null array-index))
          (:stream t)
          (:count (or null array-index)))
  (values string &optional))
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
  (declare ((or null array-index) start)
           ((or array-length null) end)
           ;; Can't be more matches than characters.
           ((or array-length null) count)
           (string old new string))
  (let ((start (or start 0))
        (new (simplify-string new))
        (old (simplify-string old)))
    (declare (array-index start))
    (with-string (s stream)
      (with-string-dispatch () string
        (let* ((end (or end (length string)))
               (len (length old)))
          (declare (array-length len))
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
                         (1- count)))))))))))

(defun string-replace (old string new &key (start 0) end stream)
  "Like `string-replace-all', but only replace the first match."
  (string-replace-all old string new
                      :start start
                      :end end
                      :stream stream
                      :count 1))

(-> chomp (string &optional list) string)
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
  (declare (string string)
           (list suffixes))
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

(deftype fixed-print-length-type ()
  '(or string character symbol pathname fixnum))

(declaim (ftype (function ((integer 0 #.most-positive-fixnum)
                           &optional (integer 2 36))
                          (integer 0 #.(integer-length most-positive-fixnum)))))
(defun digit-length (n &optional (base *print-base*))
  (declare ((integer 0 #.most-positive-fixnum) n)
           (optimize speed (safety 1)))
  ;; TODO Optimize for powers of 2.
  (let ((len 0))
    (declare ((integer 0 #.(integer-length most-positive-fixnum)) len))
    (loop for q = (truncate n base)
          do (incf len)
          until (zerop (setf n q)))
    len))

(declaim (ftype (function (&rest t) (values simple-string &optional))
                string+))
(defun string+ (&rest args)
  "Optimized function for building small strings.

Roughly equivalent to

    (let ((*print-pretty* nil))
     (format nil \"~@{~a}\" args...))

But may be more efficient when the arguments of certain simple
types (such as strings, characters, symbols, pathnames, and fixnums).

Note that unlike `princ', `string+' treats `nil' as the same as the
empty string:

    (string+ nil)
    => \"\"

    (string+ \"x\" nil)
    => \"x\"

This utility is inspired by the utility of the same name in Allegro."
  (declare (dynamic-extent args))
  (if (null args) ""
      (let ((*print-pretty* nil))
        (locally (declare (optimize (speed 3) (safety 1)))
          (tagbody
           :use-concat
             ;; Based on the implementation of concatenate 'string in
             ;; SBCL (also integer printing).
             (let ((len 0)
                   (print-base *print-base*)
                   (int-chars "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
               (declare (array-length len)
                        ((integer 2 36) print-base))
               (dolist (x args)
                 (typecase-of fixed-print-length-type x
                   (string (incf len (length x)))
                   (character (incf len))
                   (pathname
                    (when-let (namestring (namestring x))
                      (incf len (length namestring))))
                   (null)
                   (symbol (incf len (length (symbol-name x))))
                   (fixnum
                    (when (minusp x)
                      (incf len))
                    (incf len (digit-length (abs x))))
                   (otherwise (go :use-string-stream))))
               (let ((result (make-array len :element-type 'character))
                     (start 0)
                     (print-case *print-case*))
                 (declare (array-index start)
                          ((simple-array character (*)) result))
                 (dolist (x args)
                   (flet ((add-char (c)
                            (declare (character c))
                            (setf (schar result start) c)
                            (incf start))
                          (add-string (s)
                            (declare (string s))
                            (with-string-dispatch () s
                              (replace result s :start1 start)
                              (incf start (length s)))))
                     (etypecase-of fixed-print-length-type x
                       (string (add-string x))
                       (character (add-char x))
                       (pathname
                        (when-let (namestring (namestring x))
                          (add-string namestring)))
                       (null)
                       (symbol
                        ;; Case might be affected by print-case.
                        (if (eql print-case :upcase)
                            (add-string (symbol-name x))
                            (add-string (princ-to-string x))))
                       (fixnum
                        (when (minusp x)
                          (add-char #\-))
                        (let* ((x (abs x))
                               (ptr (+ start (digit-length x))))
                          (declare (array-length ptr))
                          (cond
                            ((eql x 0) (add-char #\0))
                            ((eql x 1) (add-char #\1))
                            (t
                             (loop (multiple-value-bind (q r)
                                       (truncate x print-base)
                                     ;; Write chars backwards.
                                     (decf ptr)
                                     (setf (aref result ptr) (schar int-chars r))
                                     (incf start)
                                     (when (zerop (setq x q))
                                       (return)))))))))))
                 (return-from string+ result)))
           :use-string-stream
             (return-from string+
               (with-output-to-string (s)
                 (dolist (arg args)
                   (typecase arg
                     (string (write-string arg s))
                     (character (write-char arg s))
                     (null)
                     (t (princ arg s)))))))))))

(defun simplify-args-for-string-plus (args &optional env)
  (reduce (lambda (x args)
            ;; Merge together runs of strings.
            (if (and (stringp x)
                     (stringp (car args)))
                (cons (concat x (car args))
                      (cdr args))
                (cons x args)))
          args
          :from-end t
          :key (named-lambda stringify (arg)
                 ;; Stringify constant arguments when possible.
                 (trivia:match arg
                   ((eql nil) "")
                   ((and arg (type character))
                    (string arg))
                   ((or (and sym (type keyword))
                        (list 'quote (and sym (type symbol))))
                    ;; Note that `*print-case*' may affect how symbols
                    ;; are printed, so even if the symbol is constant
                    ;; we can only be sure of the printed
                    ;; representation if there are no uppercase
                    ;; characters.
                    (let ((s (symbol-name sym)))
                      (if (notany #'upper-case-p s) s
                          arg)))
                   ;; The smallest base is 2, so these are always the
                   ;; same regardless of `*print-base*'.
                   ((eql 0) "0")
                   ((eql 1) "1")
                   ((list 'quote (and s (type string)))
                    s)
                   ((list 'quote (and c (type character)))
                    (string c))
                   (otherwise
                    (multiple-value-bind (val evaluated?)
                        (eval-if-constant arg env)
                      (if (equal arg val) arg
                          (if evaluated?
                              (stringify `(quote ,val))
                              arg))))))
          :initial-value '()))

(define-compiler-macro string+ (&whole call
                                       &environment env
                                       &rest orig-args)
  (if (null orig-args) ""
      (let ((args (simplify-args-for-string-plus orig-args env)))
        (if (= (length args) 1)
            (if (stringp (first args))
                `(copy-seq ,(first args))
                `(princ-to-string ,(first args)))
            (if (equal args orig-args) call
                `(string+ ,@args))))))
