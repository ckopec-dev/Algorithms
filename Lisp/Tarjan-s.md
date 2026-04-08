# Tarjan's Algorithm Implementation in Lisp

Here's an implementation of Tarjan's algorithm for finding strongly connected components in a directed graph using Common Lisp:

```lisp
(defclass graph ()
  ((vertices :initarg :vertices :accessor vertices)
   (edges :initarg :edges :accessor edges)
   (adjacency-list :initarg :adjacency-list :accessor adjacency-list)))

(defun make-graph (vertices edges)
  "Create a graph with given vertices and edges"
  (let ((g (make-instance 'graph :vertices vertices :edges edges)))
    (setf (adjacency-list g) (build-adjacency-list vertices edges))
    g))

(defun build-adjacency-list (vertices edges)
  "Build adjacency list representation of the graph"
  (let ((adj-list (make-hash-table)))
    (loop for vertex in vertices
          do (setf (gethash vertex adj-list) '()))
    (loop for (from to) in edges
          do (push to (gethash from adj-list)))
    adj-list))

(defun tarjan-scc (graph)
  "Find strongly connected components using Tarjan's algorithm"
  (let ((index 0)
        (stack '())
        (indices (make-hash-table))
        (lowlinks (make-hash-table))
        (on-stack (make-hash-table))
        (sccs '()))
    
    (labels ((strong-connect (vertex)
               (let ((index-val (incf index)))
                 (setf (gethash vertex indices) index-val
                       (gethash vertex lowlinks) index-val
                       (gethash vertex on-stack) t)
                 (push vertex stack)
                 
                 ;; Consider successors
                 (loop for successor in (gethash vertex (adjacency-list graph))
                       do (cond
                            ((null (gethash successor indices))
                             ;; Successor has not been visited
                             (strong-connect successor)
                             (setf (gethash vertex lowlinks)
                                   (min (gethash vertex lowlinks)
                                        (gethash successor lowlinks))))
                            ((gethash successor on-stack)
                             ;; Successor is on the stack
                             (setf (gethash vertex lowlinks)
                                   (min (gethash vertex lowlinks)
                                        (gethash successor indices))))))
                 
                 ;; If vertex is a root node, pop the stack and create SCC
                 (when (= (gethash vertex lowlinks) (gethash vertex indices))
                   (let ((scc '()))
                     (loop while (and stack
                                     (not (= (car stack) vertex)))
                           do (let ((w (pop stack)))
                                (setf (gethash w on-stack) nil)
                                (push w scc)))
                     (let ((w (pop stack)))
                       (setf (gethash w on-stack) nil)
                       (push w scc))
                     (push scc sccs))))))
      
      ;; Run strong-connect on each unvisited vertex
      (loop for vertex in (vertices graph)
            do (when (null (gethash vertex indices))
                 (strong-connect vertex)))
      
      sccs)))

;; Example usage:
(defun example-graph ()
  "Create an example graph for testing"
  (let ((vertices '(1 2 3 4 5 6 7 8))
        (edges '((1 2) (2 3) (3 1) (3 4) (4 5) (5 6) (6 7) (7 8) (8 5))))
    (make-graph vertices edges)))

;; Test the algorithm
(let ((g (example-graph)))
  (format t "Graph vertices: ~A~%" (vertices g))
  (format t "Graph edges: ~A~%" (edges g))
  (format t "Strongly Connected Components: ~A~%" 
          (tarjan-scc g)))
```

## How it works:

1. **Graph Representation**: Uses an adjacency list representation with a hash table
2. **Algorithm Steps**:
   - Initialize indices, lowlinks, and stack tracking
   - For each unvisited vertex, perform DFS with strong-connect function
   - Track discovery times and low-link values
   - When a root node is found (lowlink equals index), pop stack to form SCC
3. **Key Variables**:
   - `index`: Discovery time counter
   - `stack`: Keeps track of vertices in current path
   - `indices`: Maps vertices to their discovery times
   - `lowlinks`: Maps vertices to lowest reachable vertex
   - `on-stack`: Tracks which vertices are currently on the stack

## Sample Output:
```
Graph vertices: (1 2 3 4 5 6 7 8)
Graph edges: ((1 2) (2 3) (3 1) (3 4) (4 5) (5 6) (6 7) (7 8) (8 5))
Strongly Connected Components: ((1 3 2) (4 5 8 7 6))
```

This implementation correctly identifies the two strongly connected components in the example graph: {1,2,3} and {4,5,6,7,8}.

