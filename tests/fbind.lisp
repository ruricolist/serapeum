(in-package :serapeum.tests)

(def-suite fbind :in serapeum)
(in-suite fbind)

(test fbind
  (is (eql 1 (let ((fn (lambda (x) (1+ x))))
               (fbind fn
                 (fn 0)))))
  (is (eql 1 (fbind ((fn (lambda (x) (1+ x))))
               (fn 0))))
  (fbind ((string-or-null (rcurry #'typep '(or string null))))
    (is (string-or-null "f")))
  (fbind ((string-or-null (disjoin #'stringp #'null)))
    (is (string-or-null "foo"))
    (is (not (string-or-null 'foo)))
    (is (not (string-or-null t))))
  (fbind ((singleton-string (conjoin #'stringp #'single)))
    (is (singleton-string "f"))
    (is (not (singleton-string '(#\f))))))

(test fbindrec
  (let ((fold-case (plusp (random 2))))
    (fbindrec ((char-test
                (if fold-case
                    #'char-equal
                    #'char=)))
      (is
       (if fold-case
           (every #'char-test "foo" "FOO")
           (not (every #'char-test "foo" "FOO"))))))

  #+(or) (signals error
           (fbindrec ((make-adder (lambda (x)
                                    (lambda (y)
                                      (+ y x))))
                      (add1 (make-adder 1)))
             (add1 1))))

(test fbindrec*
  (is
   (equal (list t nil)
          (fbindrec* ((vowelp (lambda (c) (find c "aeiou")))
                      (consonantp (complement #'vowelp)))
            (list (consonantp #\j) (consonantp #\a)))))

  (let ((fold-case (plusp (random 2))))
    (fbindrec* ((fold-case?
                 (lambda () fold-case))
                (char-test
                 (if (fold-case?)
                     #'char-equal
                     #'char=)))
      (is
       (if (fold-case?)
           (every #'char-test "foo" "FOO")
           (not (every #'char-test "foo" "FOO"))))))

  #+(or) (signals error
           (fbindrec* ((a (lambda () #'c))
                       (b (a))
                       (c (constantly 7)))
             (b))))
