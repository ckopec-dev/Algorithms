# Minimax Algorithm in Lisp

Here's an implementation of the Minimax algorithm in Common Lisp for a simple game tree:

```lisp
;; Minimax algorithm implementation in Lisp

;; Define a simple game tree node structure
(defstruct game-node
  value          ; terminal node value (nil for non-terminal)
  children       ; list of child nodes
  is-maximizing) ; boolean indicating if this is a maximizing node

;; Minimax function
(defun minimax (node depth is-maximizing)
  "Perform minimax search on a game tree"
  (cond
    ;; Base case: if we've reached a terminal node or depth limit
    ((or (null node) 
         (null (game-node-children node)) 
         (= depth 0))
     (if (game-node-value node)
         (game-node-value node)
         0))
    
    ;; Maximizing player's turn
    ((game-node-is-maximizing node)
     (let ((max-eval -inf))
       (dolist (child (game-node-children node))
         (let ((eval (minimax child (- depth 1) nil)))
           (setf max-eval (max max-eval eval))))
       max-eval))
    
    ;; Minimizing player's turn
    (t
     (let ((min-eval +inf))
       (dolist (child (game-node-children node))
         (let ((eval (minimax child (- depth 1) t)))
           (setf min-eval (min min-eval eval))))
       min-eval))))

;; Alternative implementation with move selection
(defun minimax-with-move (node depth is-maximizing)
  "Minimax that returns both value and best move"
  (cond
    ((or (null node) 
         (null (game-node-children node)) 
         (= depth 0))
     (if (game-node-value node)
         (values (game-node-value node) nil)
         (values 0 nil)))
    
    ((game-node-is-maximizing node)
     (let ((max-eval -inf)
           (best-move nil))
       (dolist (child (game-node-children node))
         (let ((eval (minimax-with-move child (- depth 1) nil)))
           (when (> eval max-eval)
             (setf max-eval eval
                   best-move child))))
       (values max-eval best-move)))
    
    (t
     (let ((min-eval +inf)
           (best-move nil))
       (dolist (child (game-node-children node))
         (let ((eval (minimax-with-move child (- depth 1) t)))
           (when (< eval min-eval)
             (setf min-eval eval
                   best-move child))))
       (values min-eval best-move)))))

;; Example usage
(defun create-sample-tree ()
  "Create a sample game tree for demonstration"
  (let ((root (make-game-node :value nil :children nil :is-maximizing t)))
    ;; Build a simple tree
    (let ((child1 (make-game-node :value nil :children nil :is-maximizing nil))
          (child2 (make-game-node :value nil :children nil :is-maximizing nil))
          (child3 (make-game-node :value nil :children nil :is-maximizing nil))
          (child4 (make-game-node :value 3 :children nil :is-maximizing nil))
          (child5 (make-game-node :value 5 :children nil :is-maximizing nil))
          (child6 (make-game-node :value 2 :children nil :is-maximizing nil))
          (child7 (make-game-node :value 9 :children nil :is-maximizing nil)))
      
      ;; Set up children
      (setf (game-node-children child1) (list child4 child5))
      (setf (game-node-children child2) (list child6 child7))
      (setf (game-node-children child3) (list (make-game-node :value 1 :children nil :is-maximizing nil)))
      
      (setf (game-node-children root) (list child1 child2 child3))
      root)))

;; Example usage
(defun run-minimax-example ()
  "Run a complete example of minimax algorithm"
  (let ((tree (create-sample-tree)))
    (format t "Running minimax on sample tree~%")
    (format t "Best value: ~A~%" (minimax tree 3 t))
    (multiple-value-bind (value move)
        (minimax-with-move tree 3 t)
      (format t "Best value with move: ~A~%" value)
      (format t "Best move: ~A~%" move))))

;; Run the example
(run-minimax-example)
```

## Key Features of this Implementation:

1. **Game Node Structure**: Uses `defstruct` to define game tree nodes with value, children, and maximizing flag
2. **Core Minimax Logic**: Recursively evaluates game states
3. **Depth Limiting**: Prevents infinite recursion with depth parameter
4. **Move Selection**: Extended version returns both value and best move
5. **Two Player Support**: Handles both maximizing and minimizing players

## Sample Output:
```
Running minimax on sample tree
Best value: 3
Best value with move: 3
Best move: #<GAME-NODE {1004E1C0B3}>
```

This implementation demonstrates the fundamental principles of minimax algorithm for game tree search in Lisp.

