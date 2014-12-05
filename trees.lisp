(in-package :serapeum)

(export '(occurs occurs-if
          prune prune-if
          walk-tree map-tree
          leaf-walk leaf-map))

;; Ensure TCO when possible.
(declaim (optimize (speed 3) (debug 1)))

(defun reuse-cons (x y x-y)
  "If X and Y are the car and cdr of X-Y, return X-Y.

Otherwise, return a fresh cons of X and Y."
  (if (and (eq x (car x-y))
           (eq y (cdr x-y)))
      x-y
      (cons x y)))

(defun walk-tree (fun tree &optional (tag nil tagp))
  "Call FUN in turn over each atom and cons of TREE.

FUN can skip the current subtree with (throw TAG nil)."
  (let ((fun (ensure-function fun)))
    (labels ((walk-tree (tree)
               (cond ((atom tree) (funcall fun tree))
                     (t (funcall fun tree)
                        (walk-tree (car tree))
                        (walk-tree (cdr tree)))))
             (walk-tree/tag (tree tag)
               (catch tag
                 (cond ((atom tree) (funcall fun tree))
                       (t (funcall fun tree)
                          (walk-tree (car tree))
                          (walk-tree (cdr tree)))))))
      (if tagp
          (walk-tree/tag tree tag)
          (walk-tree tree))))
  (values))

(defun map-tree (fun tree &optional (tag nil tagp))
  "Walk FUN over TREE and build a tree from the results.

The new tree may share structure with the old tree.

     (eq tree (map-tree #'identity tree)) => T

FUN can skip the current subtree with (throw TAG SUBTREE), in which
case SUBTREE will be used as the value of the subtree."
  (let ((fun (ensure-function fun)))
    (labels ((map-tree (tree)
               (let ((tree2 (funcall fun tree)))
                 (if (atom tree2)
                     tree2
                     (reuse-cons (map-tree (car tree2))
                                 (map-tree (cdr tree2))
                                 tree2))))
             (map-tree/tag (tree tag)
               (catch tag
                 (let ((tree2 (funcall fun tree)))
                   (if (atom tree2)
                       tree2
                       (reuse-cons (map-tree (car tree2))
                                   (map-tree (cdr tree2))
                                   tree2))))))
      (if tagp
          (map-tree/tag tree tag)
          (map-tree tree)))))

(defun leaf-walk (fun tree)
  "Call FUN on each leaf of TREE."
  (declare (optimize speed (debug 1)))
  (let ((fun (ensure-function fun)))
    (cond ((atom tree)
           (funcall fun tree))
          (t (leaf-walk fun (car tree))
             (leaf-walk fun (cdr tree)))))
  (values))

;;; https://code.google.com/p/sparser/source/browse/trunk/util/util.lisp?spec=svn737&r=737
(defun leaf-map (fn tree)
  "Call FN on each leaf of TREE.
Return a new tree possibly sharing structure with TREE."
  (let ((fn (ensure-function fn)))
    (map-tree (lambda (x)
                (if (listp x)
                    x
                    (funcall fn x)))
              tree)))

(assert (equal (leaf-map (compose #'round #'sqrt) '(((4 1) 25) (9 100) 64))
               '(((2 1) 5) (3 10) 8)))

(defun occurs-if (test tree &key (key #'identity))
  "Is there a node (leaf or cons) in TREE that satisfies TEST?"
  (ensuring-functions (key test)
    ;; SBCL wants the walker to be fbound and dynamic-extent.
    (flet ((walker (node)
             (when (funcall test (funcall key node))
               (return-from occurs-if (values node t)))))
      (declare (dynamic-extent #'walker))
      (walk-tree #'walker tree))))

(defun prune-if (test tree &key (key #'identity))
  "Remove any atoms satisfying TEST from TREE."
  (ensuring-functions (key test)
    (labels ((prune (tree acc)
               (cond ((null tree)
                      (nreverse acc))
                     ((consp (car tree))
                      (prune (cdr tree)
                             (cons (prune (car tree) nil) acc)))
                     (t (prune (cdr tree)
                               (if (funcall test (funcall key (car tree)))
                                   acc
                                   (cons (car tree) acc)))))))
      (prune tree nil))))

(defun occurs (leaf tree &key (key #'identity) (test #'eql))
  "Is LEAF present in TREE?"
  (nth-value 1
    (occurs-if (curry test leaf) tree :key key)))

(defun prune (leaf tree &key (key #'identity) (test #'eql))
  "Remove LEAF from TREE wherever it occurs."
  (prune-if (curry test leaf) tree :key key))
