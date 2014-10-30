(in-package #:serapeum)

(assert (eql 1 (let ((fn (lambda (x) (1+ x))))
                 (fbind fn
                   (fn 0)))))
(assert (eql 1 (fbind ((fn (lambda (x) (1+ x))))
                 (fn 0))))
(assert
 (equal (list t nil)
        (fbindrec* ((vowelp (lambda (c) (find c "aeiou")))
                    (consonantp (complement #'vowelp)))
          (list (consonantp #\j) (consonantp #\a)))))
(fbind ((string-or-null (rcurry #'typep '(or string null))))
  (string-or-null "f"))
(fbind ((string-or-null (disjoin #'stringp #'null)))
  (assert (string-or-null "foo"))
  (assert (not (string-or-null 'foo)))
  (assert (not (string-or-null t))))
(fbind ((singleton-string (conjoin #'stringp #'single)))
  (assert (singleton-string "f"))
  (assert (not (singleton-string '(#\f)))))
(let ((fold-case (plusp (random 2))))
  (fbindrec ((char-test
              (if fold-case
                  #'char-equal
                  #'char=)))
    (assert
     (if fold-case
         (every #'char-test "foo" "FOO")
         (not (every #'char-test "foo" "FOO"))))))
(let ((fold-case (plusp (random 2))))
  (fbindrec* ((fold-case?
               (lambda () fold-case))
              (char-test
               (if (fold-case?)
                   #'char-equal
                   #'char=)))
    (assert
     (if (fold-case?)
         (every #'char-test "foo" "FOO")
         (not (every #'char-test "foo" "FOO"))))))

;; TODO These should be errors.
#+ () (progn (fbindrec ((make-adder (lambda (x)
                                (lambda (y)
                                  (+ y x))))
                  (add1 (make-adder 1)))
         (add1 1))
       (fbindrec* ((a (lambda () #'c))
                   (b (a))
                   (c (constantly 7)))
         (b)))
