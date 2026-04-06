# Treap Implementation in Lisp

Here's an example implementation of a Treap data structure in Common Lisp:

```lisp
(defstruct treap-node
  "A node in the treap data structure"
  key
  value
  priority
  left
  right)

(defclass treap ()
  "A treap data structure combining BST and heap properties"
  ((root :initform nil :accessor treap-root)
   (size :initform 0 :accessor treap-size)))

(defun treap-insert (treap key value)
  "Insert a key-value pair into the treap"
  (let ((new-node (make-treap-node :key key
                                   :value value
                                   :priority (random 1000)
                                   :left nil
                                   :right nil)))
    (setf (treap-root treap) 
          (treap-insert-node (treap-root treap) new-node))
    (incf (treap-size treap))))

(defun treap-insert-node (node new-node)
  "Helper function to insert a node recursively"
  (cond
    ((null node) new-node)
    ((< (treap-node-key new-node) (treap-node-key node))
     (setf (treap-node-left node) 
           (treap-insert-node (treap-node-left node) new-node))
     (treap-rotate-right node))
    ((> (treap-node-key new-node) (treap-node-key node))
     (setf (treap-node-right node) 
           (treap-insert-node (treap-node-right node) new-node))
     (treap-rotate-left node))
    (t
     (setf (treap-node-value node) value)
     node)))

(defun treap-rotate-right (node)
  "Right rotation to maintain heap property"
  (let ((left-child (treap-node-left node)))
    (when (and left-child 
               (> (treap-node-priority left-child) 
                  (treap-node-priority node)))
      (setf (treap-node-left node) (treap-node-right left-child)
            (treap-node-right left-child) node)
      left-child)))

(defun treap-rotate-left (node)
  "Left rotation to maintain heap property"
  (let ((right-child (treap-node-right node)))
    (when (and right-child 
               (> (treap-node-priority right-child) 
                  (treap-node-priority node)))
      (setf (treap-node-right node) (treap-node-left right-child)
            (treap-node-left right-child) node)
      right-child)))

(defun treap-search (treap key)
  "Search for a key in the treap"
  (treap-search-node (treap-root treap) key))

(defun treap-search-node (node key)
  "Helper function to search recursively"
  (cond
    ((null node) nil)
    ((< key (treap-node-key node))
     (treap-search-node (treap-node-left node) key))
    ((> key (treap-node-key node))
     (treap-search-node (treap-node-right node) key))
    (t (treap-node-value node))))

(defun treap-inorder-traversal (node)
  "Perform inorder traversal of the treap"
  (when node
    (treap-inorder-traversal (treap-node-left node))
    (format t "~A: ~A~%" (treap-node-key node) (treap-node-value node))
    (treap-inorder-traversal (treap-node-right node))))

;; Example usage:
(defun example-treap ()
  "Demonstrate treap operations"
  (let ((my-treap (make-instance 'treap)))
    ;; Insert some key-value pairs
    (treap-insert my-treap 10 "ten")
    (treap-insert my-treap 5 "five")
    (treap-insert my-treap 15 "fifteen")
    (treap-insert my-treap 3 "three")
    (treap-insert my-treap 7 "seven")
    
    (format t "Treap size: ~A~%" (treap-size my-treap))
    (format t "Inorder traversal:~%")
    (treap-inorder-traversal (treap-root my-treap))
    
    (format t "Search for key 7: ~A~%" (treap-search my-treap 7))
    (format t "Search for key 12: ~A~%" (treap-search my-treap 12))))

;; Run the example
(example-treap)
```

## Key Features of this Treap Implementation:

1. **Node Structure**: Each node contains key, value, priority, and left/right pointers
2. **Insertion**: Maintains both BST property (keys) and heap property (priorities)
3. **Rotations**: Automatic balancing through left and right rotations
4. **Search**: Efficient O(log n) search operation
5. **Traversal**: Inorder traversal to display elements in sorted order

## Time Complexities:
- **Insertion**: O(log n) average case
- **Search**: O(log n) average case  
- **Deletion**: O(log n) average case
- **Space**: O(n)

The treap combines the benefits of binary search trees (efficient searching) and heaps (randomized balancing) to provide a self-balancing data structure.

