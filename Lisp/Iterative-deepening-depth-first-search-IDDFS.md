# Iterative Deepening Depth-First Search (IDDFS) in Lisp

Here's an implementation of IDDFS in Common Lisp:

```lisp
(defun iddfs (start goal get-successors)
  "Iterative Deepening Depth-First Search implementation"
  (let ((depth 0))
    (loop
      (let ((result (dls start goal get-successors depth)))
        (if result
            (return result)
            (incf depth))))))
  
(defun dls (start goal get-successors depth)
  "Depth-Limited Search helper function"
  (dls-helper start goal get-successors depth '()))

(defun dls-helper (current goal get-successors limit path)
  "Recursive helper for depth-limited search"
  (if (= limit 0)
      (if (equal current goal)
          (cons current path)
          nil)
      (if (equal current goal)
          (cons current path)
          (let ((successors (get-successors current)))
            (dls-search successors goal get-successors 
                       (1- limit) (cons current path))))))

(defun dls-search (nodes goal get-successors limit path)
  "Search through all successors"
  (cond
    ((null nodes) nil)
    (t (let ((result (dls-helper (car nodes) goal get-successors 
                                limit path)))
         (if result
             result
             (dls-search (cdr nodes) goal get-successors limit path))))))

;; Example usage:
(defun example-graph-successors (node)
  "Example graph successors function"
  (case node
    ('a '(b c))
    ('b '(d e))
    ('c '(f g))
    ('d '())
    ('e '())
    ('f '())
    ('g '())
    (otherwise '())))

;; Test the algorithm
(defun test-iddfs ()
  "Test IDDFS with example graph"
  (let ((result (iddfs 'a 'f example-graph-successors)))
    (format t "Path found: ~A~%" result)
    result))

;; Alternative cleaner implementation:
(defun iddfs-clean (start goal get-successors)
  "Cleaner IDDFS implementation"
  (loop for depth from 0 
        do (let ((result (dls-clean start goal get-successors depth)))
             (if result (return result)))))

(defun dls-clean (start goal get-successors depth)
  "Clean depth-limited search"
  (if (= depth 0)
      (if (equal start goal) (list start) nil)
      (if (equal start goal) (list start)
          (let ((successors (get-successors start)))
            (dolist (succ successors)
              (let ((result (dls-clean succ goal get-successors (1- depth))))
                (if result 
                    (return (cons start result)))))))))
```

## Example Usage:

```lisp
;; Define a simple graph
(defun simple-graph-successors (node)
  (case node
    ('A '(B C))
    ('B '(D E))
    ('C '(F G))
    ('D '())
    ('E '())
    ('F '())
    ('G '())
    (otherwise '())))

;; Find path from A to F
(iddfs-clean 'A 'F 'simple-graph-successors)
;; Returns: (A C F)
```

## Key Features:

1. **Iterative Deepening**: Repeatedly performs depth-limited search with increasing depth limits
2. **Memory Efficient**: Uses only O(bd) space where b is branching factor and d is depth
3. **Optimal**: Finds optimal solution like BFS but with less memory usage
4. **Complete**: Guarantees finding a solution if one exists

## Time Complexity:
- **Time**: O(b^d) where b is branching factor and d is depth
- **Space**: O(bd) - much better than BFS's O(b^d) space complexity

The algorithm combines the benefits of both depth-first search (low memory usage) and breadth-first search (completeness and optimality).

