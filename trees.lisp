(in-package :serapeum)

(declaim (inline reuse-cons))
(defun reuse-cons (x y x-y)
  "If X and Y are the car and cdr of X-Y, return X-Y.

Otherwise, return a fresh cons of X and Y."
  (if (and (eq x (car x-y))
           (eq y (cdr x-y)))
      x-y
      (cons x y)))

(declaim (ftype
          (function (list) (values t t &optional))
          car+cdr))
(defun car+cdr (list)
  "Given LIST, return its car and cdr as two values."
  (values (car list)
          (cdr list)))

(defun walk-tree (fun tree &key (tag nil tagp) (traversal :preorder))
  "Call FUN in turn over each atom and cons of TREE.

FUN can skip the current subtree with (throw TAG nil)."
  (let ((fun (ensure-function fun)))
    ;; NB map-tree only conses if you change something.
    (multiple-value-call #'map-tree
      (lambda (tree)
        (funcall fun tree)
        tree)
      tree
      :traversal traversal
      (if tagp (values :tag tag) (values)))
    (values)))

(defun map-tree (fun tree &key (tag nil tagp)
                               (traversal :preorder))
  "Walk FUN over TREE and build a tree from the results.

The new tree may share structure with the old tree.

     (eq tree (map-tree #'identity tree)) => T

FUN can skip the current subtree with (throw TAG SUBTREE), in which
case SUBTREE will be used as the value of the subtree.

TRAVERSE can be one of `:preorder', `:postorder', or `:inorder'. The
default is `:preorder'."
  (ecase traversal
    (:preorder (map-tree/preorder fun tree tag tagp))
    (:postorder (map-tree/postorder fun tree tag tagp))
    (:inorder (map-tree/inorder fun tree tag tagp))))

(defun map-tree/preorder (fun tree tag tagp)
  #.+merge-tail-calls+
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
                       (reuse-cons (map-tree/tag (car tree2) tag)
                                   (map-tree/tag (cdr tree2) tag)
                                   tree2))))))
      (if tagp
          (map-tree/tag tree tag)
          (map-tree tree)))))

(defun map-tree/postorder (fun tree tag tagp)
  #.+merge-tail-calls+
  (let ((fun (ensure-function fun)))
    (labels ((map-tree (tree)
               (if (atom tree)
                   (funcall fun tree)
                   (let* ((left (map-tree (car tree)))
                          (right (map-tree (cdr tree)))
                          (tree2 (reuse-cons left right tree)))
                     (funcall fun tree2))))
             (map-tree/tag (tree tag)
               (catch tag
                 (if (atom tree)
                     (funcall fun tree)
                     (let* ((left (map-tree/tag (car tree) tag))
                            (right (map-tree/tag (cdr tree) tag))
                            (tree2 (reuse-cons left right tree)))
                       (funcall fun tree2))))))
      (if tagp
          (map-tree/tag tree tag)
          (map-tree tree)))))

(defun map-tree/inorder (fun tree tag tagp)
  #.+merge-tail-calls+
  (let ((fun (ensure-function fun)))
    (labels ((map-tree (tree)
               (if (atom tree)
                   (funcall fun tree)
                   (let* ((left (map-tree (car tree)))
                          (tree2 (funcall fun (reuse-cons left (cdr tree) tree))))
                     (reuse-cons (car tree2)
                                 (map-tree (cdr tree2))
                                 tree2))))
             (map-tree/tag (tree tag)
               (catch tag
                 (if (atom tree)
                     (funcall fun tree)
                     (let* ((left (map-tree/tag (car tree) tag))
                            (tree2 (funcall fun (reuse-cons left (cdr tree) tree))))
                       (reuse-cons (car tree2)
                                   (map-tree/tag (cdr tree2) tag)
                                   tree2))))))
      (if tagp
          (map-tree/tag tree tag)
          (map-tree tree)))))

(defun leaf-walk (fun tree)
  "Call FUN on each leaf of TREE."
  (let ((fun (ensure-function fun)))
    (labels ((leaf-walk (fun tree)
               (declare (function fun))
               (cond ((atom tree)
                      (funcall fun tree))
                     (t (leaf-walk fun (car tree))
                        (leaf-walk fun (cdr tree))))))
      #.+merge-tail-calls+
      (leaf-walk fun tree)))
  (values))

;;; https://code.google.com/p/sparser/source/browse/trunk/util/util.lisp?spec=svn737&r=737
(defun leaf-map (fn tree)
  "Call FN on each leaf of TREE.
Return a new tree possibly sharing structure with TREE."
  (let ((fn (ensure-function fn)))
    (flet ((map-fn (x)
             (if (listp x)
                 x
                 (funcall fn x))))
      #.+merge-tail-calls+
      (declare (dynamic-extent #'map-fn))
      (map-tree #'map-fn tree))))

(defun occurs-if (test tree &key (key #'identity) (traversal :preorder))
  "Is there a node (leaf or cons) in TREE that satisfies TEST?"
  (ensuring-functions (key test)
    ;; SBCL wants the walker to be fbound and dynamic-extent.
    (flet ((walker (node)
             (when (funcall test (funcall key node))
               (return-from occurs-if (values node t)))))
      (declare (dynamic-extent #'walker))
      (walk-tree #'walker tree :traversal traversal))))

(defun prune-if (test tree &key (key #'identity))
  "Remove any atoms satisfying TEST from TREE.

Pruning is defined \"modulo flatten\": you should get the same result
from pruning, and then flattening, that you would get from flattening,
and then filtering.

Also note that pruning is not defined for trees containing improper
lists."
  (ensuring-functions (key test)
    (labels ((cons* (car cdr)
               (if (funcall test (funcall key car))
                   cdr
                   (cons car cdr)))
             (prune (tree acc)
               (cond ((null tree)
                      (nreverse acc))
                     ((consp (car tree))
                      (prune (cdr tree)
                             (cons* (prune (car tree) nil) acc)))
                     (t (prune (cdr tree)
                               (cons* (car tree) acc))))))
      #.+merge-tail-calls+
      (prune tree nil))))

(defun occurs (node tree &key (key #'identity) (test #'eql) (traversal :preorder))
  "Is NODE present in TREE?"
  (nth-value 1
    (ensuring-functions (test)
      (flet ((test (x) (funcall test node x)))
        (declare (dynamic-extent #'test))
        (occurs-if #'test tree :key key :traversal traversal)))))

(defun prune (leaf tree &key (key #'identity) (test #'eql))
  "Remove LEAF from TREE wherever it occurs.
See `prune-if' for more information."
  (ensuring-functions (test)
    (flet ((test (x) (funcall test leaf x)))
      (declare (dynamic-extent #'test))
      (prune-if #'test tree :key key))))
