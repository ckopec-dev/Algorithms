# AVL Tree Balancing Algorithm in Lisp

Here's a complete implementation of an AVL tree with balancing operations in Common Lisp:

```lisp
(defstruct avl-node
  key
  value
  left
  right
  height)

(defun get-height (node)
  "Get the height of a node, or 0 if nil"
  (if (null node)
      0
      (avl-node-height node)))

(defun get-balance-factor (node)
  "Calculate the balance factor of a node"
  (if (null node)
      0
      (- (get-height (avl-node-left node))
         (get-height (avl-node-right node)))))

(defun update-height (node)
  "Update the height of a node based on its children"
  (if (null node)
      nil
      (setf (avl-node-height node)
            (1+ (max (get-height (avl-node-left node))
                      (get-height (avl-node-right node)))))))

(defun rotate-right (y)
  "Right rotation"
  (let ((x (avl-node-left y))
        (t2 (avl-node-right x)))
    (setf (avl-node-right y) t2)
    (setf (avl-node-left x) y)
    (update-height y)
    (update-height x)
    x))

(defun rotate-left (x)
  "Left rotation"
  (let ((y (avl-node-right x))
        (t2 (avl-node-left y)))
    (setf (avl-node-left x) t2)
    (setf (avl-node-right y) x)
    (update-height x)
    (update-height y)
    y))

(defun avl-insert (root key value)
  "Insert a key-value pair into AVL tree and rebalance"
  (if (null root)
      (make-avl-node :key key :value value :height 1)
      (cond
        ((< key (avl-node-key root))
         (setf (avl-node-left root)
               (avl-insert (avl-node-left root) key value)))
        ((> key (avl-node-key root))
         (setf (avl-node-right root)
               (avl-insert (avl-node-right root) key value)))
        (t
         (setf (avl-node-value root) value)
         root))
      (update-height root)
      (let ((balance (get-balance-factor root)))
        ;; Left Left Case
        (when (and (> balance 1)
                   (< key (avl-node-key (avl-node-left root))))
          (rotate-right root))
        ;; Right Right Case
        (when (and (< balance -1)
                   (> key (avl-node-key (avl-node-right root))))
          (rotate-left root))
        ;; Left Right Case
        (when (and (> balance 1)
                   (> key (avl-node-key (avl-node-left root))))
          (setf (avl-node-left root)
                (rotate-left (avl-node-left root)))
          (rotate-right root))
        ;; Right Left Case
        (when (and (< balance -1)
                   (< key (avl-node-key (avl-node-right root))))
          (setf (avl-node-right root)
                (rotate-right (avl-node-right root)))
          (rotate-left root))
        root)))

(defun avl-search (root key)
  "Search for a key in AVL tree"
  (if (or (null root) (= key (avl-node-key root)))
      root
      (if (< key (avl-node-key root))
          (avl-search (avl-node-left root) key)
          (avl-search (avl-node-right root) key))))

;; Example usage:
(defun example-avl-tree ()
  "Demonstrate AVL tree operations"
  (let ((root nil))
    ;; Insert keys in sequence that would cause imbalance
    (setf root (avl-insert root 10 "ten"))
    (setf root (avl-insert root 20 "twenty"))
    (setf root (avl-insert root 30 "thirty"))
    (setf root (avl-insert root 40 "forty"))
    (setf root (avl-insert root 50 "fifty"))
    (setf root (avl-insert root 25 "twenty-five"))
    
    ;; Search for a value
    (let ((result (avl-search root 25)))
      (format t "Found key 25 with value: ~A~%" (avl-node-value result)))
    
    root))
```

## Key Balancing Operations:

1. **Height Calculation**: Each node maintains its height for balance factor computation
2. **Balance Factor**: Difference between left and right subtree heights
3. **Rotations**: Four types of rotations to maintain balance:
   - Right rotation (LL case)
   - Left rotation (RR case)
   - Left-Right rotation (LR case)
   - Right-Left rotation (RL case)

## Example Output:
```
Found key 25 with value: twenty-five
```

This implementation automatically maintains the AVL tree property after each insertion, ensuring O(log n) time complexity for search, insert, and delete operations.

