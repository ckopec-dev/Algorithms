# Binary Search Tree in Lisp

Here's an implementation of a Binary Search Tree algorithm in Common Lisp:

```lisp
;; Define a node structure for the BST
(defstruct bst-node
  key
  value
  left
  right)

;; Insert a key-value pair into the BST
(defun bst-insert (node key value)
  "Insert a key-value pair into the binary search tree"
  (cond
    ;; If tree is empty, create new node
    ((null node)
     (make-bst-node :key key :value value))
    
    ;; If key already exists, update the value
    ((= key (bst-node-key node))
     (setf (bst-node-value node) value)
     node)
    
    ;; If key is less than current node's key, go left
    ((< key (bst-node-key node))
     (setf (bst-node-left node) 
           (bst-insert (bst-node-left node) key value))
     node)
    
    ;; If key is greater than current node's key, go right
    (t
     (setf (bst-node-right node) 
           (bst-insert (bst-node-right node) key value))
     node)))

;; Search for a key in the BST
(defun bst-search (node key)
  "Search for a key in the binary search tree and return its value"
  (cond
    ;; If tree is empty, key not found
    ((null node) nil)
    
    ;; If key matches current node's key, return the value
    ((= key (bst-node-key node))
     (bst-node-value node))
    
    ;; If key is less than current node's key, search left subtree
    ((< key (bst-node-key node))
     (bst-search (bst-node-left node) key))
    
    ;; If key is greater than current node's key, search right subtree
    (t
     (bst-search (bst-node-right node) key))))

;; In-order traversal to print the tree
(defun bst-inorder-traversal (node)
  "Perform in-order traversal of the BST"
  (when node
    (bst-inorder-traversal (bst-node-left node))
    (format t "~A: ~A~%" (bst-node-key node) (bst-node-value node))
    (bst-inorder-traversal (bst-node-right node))))

;; Example usage
(defun example-bst ()
  "Demonstrate BST operations"
  (let ((root nil))
    ;; Insert some key-value pairs
    (setf root (bst-insert root 50 "Fifty"))
    (setf root (bst-insert root 30 "Thirty"))
    (setf root (bst-insert root 70 "Seventy"))
    (setf root (bst-insert root 20 "Twenty"))
    (setf root (bst-insert root 40 "Forty"))
    (setf root (bst-insert root 60 "Sixty"))
    (setf root (bst-insert root 80 "Eighty"))
    
    ;; Print the tree in-order
    (format t "BST in-order traversal:~%")
    (bst-inorder-traversal root)
    
    ;; Search for values
    (format t "~%Searching for key 40: ~A~%" 
            (bst-search root 40))
    (format t "Searching for key 25: ~A~%" 
            (bst-search root 25))
    
    root))
```

## Usage Example:

```lisp
;; Run the example
(example-bst)
```

## Output:
```
BST in-order traversal:
20: Twenty
30: Thirty
40: Forty
50: Fifty
60: Sixty
70: Seventy
80: Eighty

Searching for key 40: Forty
Searching for key 25: NIL
```

## Key Features:

1. **Node Structure**: Uses `defstruct` to create BST nodes with key, value, left, and right pointers
2. **Insert Operation**: Maintains BST property (left subtree < node < right subtree)
3. **Search Operation**: Efficient O(log n) search using BST property
4. **Traversal**: In-order traversal prints keys in sorted order
5. **Duplicate Handling**: Updates existing key values rather than creating duplicates

This implementation demonstrates the core BST operations: insertion, searching, and traversal, all using recursive approaches typical in functional programming languages like Lisp.

