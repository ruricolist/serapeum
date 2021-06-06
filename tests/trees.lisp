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
                       :tag 'skip)
             '((a (b) (c skipped))))))

(test traversal
  (let ((tree '(a . (b . c))))
    (flet ((sequentialize (tree traversal)
             (collecting
               (walk-tree #'collect
                          tree
                          :traversal traversal))))
      (is (equal
           '((a . (b . c)) a (b . c) b c)
           (sequentialize tree :preorder)))
      (is (equal
           '(a (a . (b . c)) b (b . c) c)
           (sequentialize tree :inorder)))
      (is (equal
           '(a b c (b . c) (a . (b . c)))
           (sequentialize tree :postorder))))))

(def prune-trees
  '((((2 1) 5) (3 10) 8)
    (((4 1) 25) (9 100) 64)
    ((a (b) (c (skip-me d (e f)))))
    ((a (b) (c skipped)))
    (a (b c))
    ((a (b c)) a (b c) b c)
    (a (a (b c)) b (b c) c)
    (a b c (b c) (a (b c)))))

(test prune-flatten
  (dolist (tree prune-trees)
    (let ((elt (random-elt (flatten tree))))
      (is (equal (remove elt (flatten tree))
                 (flatten (prune elt tree)))))))

(test prune-null
  (is (equal '(2) (prune-if #'null '(2 (nil))))))
