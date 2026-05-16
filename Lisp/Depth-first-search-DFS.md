# Depth-First Search (DFS) in Lisp

Here's an implementation of the Depth-First Search algorithm in Lisp:

```lisp
;; Define a simple graph representation as an adjacency list
(defparameter *graph*
  '((A (B C))
    (B (A D E))
    (C (A F))
    (D (B))
    (E (B F))
    (F (C E))))

;; DFS algorithm implementation
(defun dfs (start target graph visited nil)
  "Perform depth-first search from start node to target node"
  (if (equal start target)
      (list start)
      (progn
        (push start visited)
        (let ((neighbors (cdr (assoc start graph))))
          (loop for node in neighbors
                when (not (member node visited))
                do (let ((result (dfs node target graph (cons start visited))))
                      (if result
                          (return (cons start result))
                          (progn
                            (push node visited)
                            (push node visited)))))))))

;; Alternative DFS implementation using a stack
(defun dfs-stack (start target graph)
  "DFS using explicit stack - returns path from start to target"
  (let ((stack (list start))
        (visited nil)
        (parent-map (make-hash-table :test 'equal)))
    (push start visited)
    
    (loop while stack do
      (let ((current (pop stack)))
        (if (equal current target)
            (return (reconstruct-path parent-map start target))
            (let ((neighbors (cdr (assoc current graph))))
              (loop for node in neighbors
                    when (not (member node visited))
                    do (progn
                         (push node visited)
                         (push node stack)
                         (setf (gethash node parent-map) current)))))))
    nil))

;; Helper function to reconstruct path from parent map
(defun reconstruct-path (parent-map start target)
  "Reconstruct path from parent map"
  (let ((path nil)
        (current target))
    (loop while current do
      (push current path)
      (setf current (gethash current parent-map))
      (if (equal current start)
          (progn
            (push current path)
            (return path))
          (if (null current)
              (return nil)))))

;; Simple DFS without target (visits all nodes)
(defun dfs-traverse (start graph visited nil)
  "Traverse graph using DFS starting from given node"
  (format t "Visiting ~A~%" start)
  (push start visited)
  (let ((neighbors (cdr (assoc start graph))))
    (loop for node in neighbors
          when (not (member node visited))
          do (dfs-traverse node graph (cons start visited)))))
```

## Usage Examples:

```lisp
;; Example 1: Find path from A to F
(dfs-stack 'A 'F *graph*)
;; Returns: (A C F)

;; Example 2: Traverse entire graph
(dfs-traverse 'A *graph* nil)
;; Visits nodes in order: A, B, D, E, F, C

;; Example 3: Find path from B to D
(dfs-stack 'B 'D *graph*)
;; Returns: (B D)
```

## Key Features:

1. **Graph Representation**: Uses adjacency list format
2. **Path Finding**: Returns the actual path from start to target
3. **Cycle Prevention**: Uses visited set to avoid cycles
4. **Multiple Implementations**: Stack-based and recursive approaches
5. **Flexible**: Can be easily modified for different graph representations

The algorithm explores as far as possible along each branch before backtracking, which is the defining characteristic of depth-first search.

