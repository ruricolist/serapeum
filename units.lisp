(in-package :serapeum)

(defconst si-prefixes
  '((-30 "quecto" "q")
    (-27 "ronto"  "r")
    (-24 "yocto"  "y")
    (-21 "zepto"  "z")
    (-18 "atto"   "a")
    (-15 "femto"  "f")
    (-12 "pico"   "p")
    ( -9 "nano"   "n")
    ( -6 "micro"  "μ")
    ( -3 "milli"  "m")
    ( -2 "centi"  "c")
    ( -1 "deci"   "d")
    (  0 ""       "" )
    (  1 "deca"   "da")
    (  2 "hecto"  "h")
    (  3 "kilo"   "k")
    (  6 "mega"   "M")
    (  9 "giga"   "G")
    ( 12 "tera"   "T")
    ( 15 "peta"   "P")
    ( 18 "exa"    "E")
    ( 21 "zetta"  "Z")
    ( 24 "yotta"  "Y")
    ( 27 "ronna"  "R")
    ( 30 "quetta" "Q"))
  "List as SI prefixes: power of ten, long form, short form.")

(defconst si-prefixes-base-1000
  (loop for (pow long short) in si-prefixes
        unless (< 0 (abs pow) 3)
          collect (list (truncate pow 3) long short))
  "The SI prefixes as powers of 1000, with centi, deci, deca and hecto omitted.")

(defconst iec-prefixes
  '(( 0 ""     "")
    (10 "kibi" "Ki")
    (20 "mebi" "Mi")
    (30 "gibi" "Gi")
    (40 "tebi" "Ti")
    (50 "pebi" "Pi")
    (60 "exbi" "Ei"))
  "The IEC binary prefixes, as powers of 2.")

(defmacro si-prefix-rec (n base prefixes)
  (cond ((null prefixes) (error "No prefixes!"))
        ((single prefixes)
         (destructuring-bind ((power long short)) prefixes
           `(values ,long ,short ,(expt base power))))
        (t
         (multiple-value-bind (lo hi) (halves prefixes)
           (let ((split (* (expt base (caar hi)))))
             `(if (< ,n ,split)
                  (si-prefix-rec ,n ,base ,lo)
                  (si-prefix-rec ,n ,base ,hi)))))))

(defun si-prefix (n &key (base 1000))
  "Given a number, return the prefix of the nearest SI unit.

Three values are returned: the long form, the short form, and the
multiplying factor.

    (si-prefix 1001) => \"kilo\", \"k\", 1000d0

BASE can be 1000, 10, 1024, or 2. 1000 is the default, and prefixes
start at kilo and milli. Base 10 is mostly the same, except the
prefixes centi, deci, deca and hecto are also used. Base 1024 uses the
same prefixes as 1000, but with 1024 as the base, as in vulgar file
sizes. Base 2 uses the IEC binary prefixes."
  (if (zerop n) (values "" "" 1d0)
      (let ((n (abs (coerce n 'double-float))))
        (ecase base
          (2 (si-prefix-rec n 2d0 #.iec-prefixes))
          (10 (si-prefix-rec n 10d0 #.si-prefixes))
          (1000 (si-prefix-rec n 1000d0 #.si-prefixes-base-1000))
          (1024 (si-prefix-rec n 1024d0 #.si-prefixes-base-1000))))))

(defun human-size-formatter (size &key (flavor :si)
                                       (space (eql flavor :si)))
  "Auxiliary function for formatting quantities human-readably.
Returns two values: a format control and a list of arguments.

This can be used to integrate the human-readable printing of
quantities into larger format control strings using the recursive
processing format directive (~?):

    (multiple-value-bind (control args)
        (human-size-formatter size)
      (format t \"~?\" control args))"
  (let ((size (coerce size 'double-float))
        ;; Avoid printing exponent markers.
        (*read-default-float-format* 'double-float)
        (base (ecase flavor
                (:file 1024)
                (:si   1000)
                (:iec  2))))
    (multiple-value-bind (long short factor)
        (si-prefix size :base base)
      (declare (ignore long))
      (let* ((size (/ size factor))
             (int (round size))
             (size
               (if (> (abs (- size int))
                      0.05d0)
                   size
                   int)))
        (values (formatter "~:[~d~;~,1f~]~:[~; ~]~a")
                (list (floatp size) size space short))))))

(defun format-human-size (stream size
                          &key (flavor :si)
                               (space (eql flavor :si)))
  "Write SIZE to STREAM, in human-readable form.

STREAM is interpreted as by `format'.

If FLAVOR is `:si' (the default) the base is 1000 and SI prefixes are used.

If FLAVOR is `:file', the base is 1024 and SI prefixes are used.

If FLAVOR is `:iec', the base is 1024 bytes and IEC prefixes (Ki, Mi,
etc.) are used.

If SPACE is non-nil, include a space between the number and the
prefix. (Defaults to T if FLAVOR is `:si'.)"
  (if (zerop size)
      (format stream "0")
      (multiple-value-bind (formatter args)
          (human-size-formatter size :flavor flavor :space space)
        (format stream "~?" formatter args))))
