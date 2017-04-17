(in-package :serapeum.tests)

(def-suite trees :in serapeum)
(in-suite trees)

(test leaf-map
  (is (equal (leaf-map (compose #'round #'sqrt) '(((4 1) 25) (9 100) 64))
             '(((2 1) 5) (3 10) 8))))

(test map-tree
  (is (equal (map-tree (lambda (subtree)
                         (if (and (consp subtree)
                                  (eql (car subtree) 'skip-me))
                             (throw 'skip 'skipped)
                             subtree))
                       '((a (b) (c (skip-me d (e f)))))
                       'skip)
             '((a (b) (c skipped))))))
