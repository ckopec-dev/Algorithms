# Morris Traversal in Lisp

Morris traversal is an algorithm for traversing binary trees without using recursion or a stack. Here's an implementation in Common Lisp:

```lisp
;; Define a binary tree node structure
(defstruct tree-node
  value
  left
  right)

;; Morris Inorder Traversal
(defun morris-traversal (root)
  "Perform inorder traversal using Morris algorithm"
  (let ((current root)
        (result '()))
    (loop while current do
      (if (null (tree-node-left current))
          ;; If no left child, visit current node and move to right
          (progn
            (push (tree-node-value current) result)
            (setf current (tree-node-right current)))
          ;; If left child exists, find the inorder predecessor
          (let ((predecessor (tree-node-left current)))
            ;; Find the rightmost node in left subtree
            (loop while (tree-node-right predecessor) do
              (setf predecessor (tree-node-right predecessor)))
            ;; Make current as right child of predecessor
            (setf (tree-node-right predecessor) current)
            ;; Move to left child
            (let ((temp (tree-node-left current)))
              (setf (tree-node-left current) nil)
              (setf current temp)))))
    (nreverse result)))

;; Alternative implementation with explicit tree construction
(defun morris-traversal-2 (root)
  "Alternative Morris traversal with clearer structure"
  (let ((current root)
        (result '()))
    (loop while current do
      (if (null (tree-node-left current))
          ;; No left child - process current node
          (progn
            (push (tree-node-value current) result)
            (setf current (tree-node-right current)))
          ;; Has left child - find predecessor
          (let ((predecessor (tree-node-left current)))
            ;; Find the rightmost node in left subtree
            (loop while (tree-node-right predecessor) do
              (setf predecessor (tree-node-right predecessor)))
            ;; Create temporary link
            (setf (tree-node-right predecessor) current)
            ;; Move to left subtree
            (let ((temp (tree-node-left current)))
              (setf (tree-node-left current) nil)
              (setf current temp))))))
    (nreverse result)))

;; Example usage:
;; Create a sample binary tree:
;;       1
;;      / \
;;     2   3
;;    / \
;;   4   5

(let ((node4 (make-tree-node :value 4))
      (node5 (make-tree-node :value 5))
      (node2 (make-tree-node :value 2 :left node4 :right node5))
      (node3 (make-tree-node :value 3))
      (node1 (make-tree-node :value 1 :left node2 :right node3)))
  
  ;; Perform Morris traversal
  (morris-traversal node1))
;; Returns: (4 2 5 1 3)

;; Morris Preorder Traversal
(defun morris-preorder (root)
  "Morris traversal for preorder traversal"
  (let ((current root)
        (result '()))
    (loop while current do
      (if (null (tree-node-left current))
          ;; No left child - process current node and move right
          (progn
            (push (tree-node-value current) result)
            (setf current (tree-node-right current)))
          ;; Has left child - find predecessor
          (let ((predecessor (tree-node-left current)))
            ;; Find the rightmost node in left subtree
            (loop while (tree-node-right predecessor) do
              (setf predecessor (tree-node-right predecessor)))
            ;; Create temporary link
            (setf (tree-node-right predecessor) current)
            ;; Process current node (preorder)
            (push (tree-node-value current) result)
            ;; Move to left subtree
            (let ((temp (tree-node-left current)))
              (setf (tree-node-left current) nil)
              (setf current temp))))))
    (nreverse result)))
```

## Key Features of this Implementation:

1. **Space Complexity**: O(1) - No recursion or stack usage
2. **Time Complexity**: O(n) - Each edge visited at most twice
3. **Inorder Traversal**: Visits nodes in left-root-right order
4. **Preorder Extension**: Can be easily modified for preorder traversal
5. **Temporary Links**: Uses existing right pointers to create temporary links

## How it works:

1. Start with the root node
2. If current node has no left child, visit it and move to right
3. If left child exists, find the inorder predecessor (rightmost node in left subtree)
4. Create a temporary link from predecessor to current node
5. Move to left subtree
6. When returning to a node via the temporary link, remove the link and continue

This algorithm is particularly useful when memory is constrained and recursion is not preferred.

