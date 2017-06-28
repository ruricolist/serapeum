(in-package :serapeum.tests)

(def-suite tree-case :in serapeum)
(in-suite tree-case)

(test tree-case
  (is (null (tree-case 0)))
  (is (= 0
         (tree-case 0
           (0 0))))
  (is (= 0
         (tree-case 0
           (0 0)
           (1 1))))
  (is (= 0
         (tree-case 0
           (0 0)
           (-1 -1))))
  (is (= 0
         (tree-case 0
           (-1 -1)
           (0 0)
           (1 1))))
  (is (= 0
         (tree-case 0
           ((-1 -2) -1)
           ((0 1 2) 0)
           ((3 4 5) 3))))
  (is (eql 'otherwise
           (tree-case 0
             (1 1)
             (otherwise 'otherwise)))))

(test tree-ecase
  (signals error
    (tree-ecase 0))
  (signals error
    (tree-ecase 0
      (1 1))))

(test char-case
  (is (null (char-case #\a)))
  (is (eql #\a
           (char-case #\a
             (#\a #\a))))
  (is (null
       (char-case #\a
         (#\b #\b))))
  (is (eql #\a
           (char-case #\a
             ((#\a #\b #\c) #\a))))
  (is (eql #\a
           (char-case #\a
             ("abcd" #\a)))))

(test char-ecase
  (signals case-failure
    (char-ecase #\a))
  (is (eql #\a
           (char-ecase #\a
             (#\a #\a))))
  (signals case-failure
    (char-ecase #\a
      (#\b #\b)))
  (is (eql #\a
           (char-ecase #\a
             ((#\a #\b #\c) #\a))))
  (is (eql #\a
           (char-ecase #\a
             ("abcd" #\a)))))

(test char-case-error
  (signals type-error
    (char-case 2))

  (signals type-error
    (char-ecase 2)))
