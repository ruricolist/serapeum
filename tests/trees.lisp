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
