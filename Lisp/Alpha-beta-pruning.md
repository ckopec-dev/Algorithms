# Alpha-Beta Pruning in Lisp

Here's an implementation of the Alpha-Beta pruning algorithm in Lisp for a minimax game tree:

```lisp
;; Alpha-Beta Pruning Implementation

(defun alpha-beta (tree alpha beta maximizing-player)
  "Alpha-Beta pruning algorithm for game tree search"
  (cond
    ;; Base case: leaf node
    ((null tree) 
     (if maximizing-player alpha beta))
    
    ;; If it's a leaf node (no children)
    ((atom tree)
     (if maximizing-player 
         (max alpha (car tree)) 
         (min beta (car tree)))))
  
  ;; If it's a branch node
  (let ((value (if maximizing-player
                   most-negative-fixnum
                   most-positive-fixnum)))
    (dolist (child tree)
      (let ((child-value (alpha-beta child 
                                    alpha 
                                    beta 
                                    (not maximizing-player))))
        (if maximizing-player
            (progn
              (setf value (max value child-value))
              (setf alpha (max alpha value))
              (when (<= beta alpha)
                (return-from alpha-beta value)))
            (progn
              (setf value (min value child-value))
              (setf beta (min beta value))
              (when (<= beta alpha)
                (return-from alpha-beta value)))))
    value))

;; Example usage with a sample game tree
(defun example-game-tree ()
  "Example game tree for demonstration"
  '(3 
    (5 
     (9 2 8)
     (4 7 6))
    (2 
     (1 3 4)
     (5 2 1))))

;; Alternative implementation with explicit tree structure
(defun alpha-beta-tree (node alpha beta maximizing-player)
  "Alpha-Beta pruning with explicit tree node structure"
  (if (null node)
      (if maximizing-player alpha beta)
      (if (atom node)
          (if maximizing-player 
              (max alpha (car node)) 
              (min beta (car node)))
          ;; Node with children
          (let ((value (if maximizing-player
                          most-negative-fixnum
                          most-positive-fixnum)))
            (dolist (child node)
              (let ((child-value (alpha-beta-tree 
                                 child 
                                 alpha 
                                 beta 
                                 (not maximizing-player))))
                (if maximizing-player
                    (progn
                      (setf value (max value child-value))
                      (setf alpha (max alpha value))
                      (when (<= beta alpha)
                        (return-from alpha-beta-tree value)))
                    (progn
                      (setf value (min value child-value))
                      (setf beta (min beta value))
                      (when (<= beta alpha)
                        (return-from alpha-beta-tree value)))))
            value)))))

;; Usage example
(defun test-alpha-beta ()
  "Test the alpha-beta pruning algorithm"
  (let ((game-tree (example-game-tree)))
    (format t "Game tree: ~A~%" game-tree)
    (format t "Alpha-Beta result (maximizing): ~A~%" 
            (alpha-beta-tree game-tree 
                           most-negative-fixnum 
                           most-positive-fixnum 
                           t))))

;; Simple version for binary trees
(defun simple-alpha-beta (tree alpha beta maximizing-player)
  "Simplified alpha-beta for binary trees"
  (if (atom tree)
      (car tree)
      (let ((value (if maximizing-player
                       most-negative-fixnum
                       most-positive-fixnum)))
        (dolist (child tree)
          (let ((child-value (simple-alpha-beta 
                             child 
                             alpha 
                             beta 
                             (not maximizing-player)))))
        value)))

;; Run the example
(test-alpha-beta)
```

## Key Features of this Implementation:

1. **Recursive Structure**: Uses recursion to traverse the game tree
2. **Alpha-Beta Pruning**: Implements the pruning logic to avoid exploring unnecessary branches
3. **MinMax Logic**: Alternates between maximizing and minimizing players
4. **Early Termination**: Stops exploring branches when alpha ≥ beta
5. **Flexible Tree Structure**: Works with nested lists representing game trees

## How it Works:

1. **Base Cases**: 
   - Empty tree returns the appropriate alpha/beta value
   - Leaf nodes return their values

2. **Pruning Logic**:
   - For maximizing player: update alpha with maximum values
   - For minimizing player: update beta with minimum values
   - When beta ≤ alpha, prune the remaining branches

3. **Player Alternation**: The `maximizing-player` flag alternates with each recursive call

This implementation demonstrates the core principles of Alpha-Beta pruning while maintaining clean, readable Lisp code structure.

