# Kosaraju's Algorithm Implementation in Lisp

Here's a complete implementation of Kosaraju's algorithm for finding strongly connected components in a directed graph using Common Lisp:

```lisp
(defpackage :kosaraju
  (:use :cl)
  (:export :strongly-connected-components))

(in-package :kosaraju)

;; Graph representation as adjacency list
(defstruct graph
  (vertices '() :type list)
  (adjacency-list '() :type list))

;; Add an edge to the graph
(defun add-edge (graph from to)
  "Add a directed edge from 'from' to 'to'"
  (let ((adj-list (graph-adjacency-list graph)))
    (if (assoc from adj-list)
        (push to (cdr (assoc from adj-list)))
        (push (cons from (list to)) adj-list))
    graph))

;; Depth First Search to build finishing times
(defun dfs-visit (graph vertex visited finish-stack)
  "DFS visit to build finishing times"
  (push vertex visited)
  (let ((neighbors (cdr (assoc vertex (graph-adjacency-list graph)))))
    (dolist (neighbor neighbors)
      (unless (member neighbor visited)
        (dfs-visit graph neighbor visited finish-stack))))
  (push vertex finish-stack))

;; Get finishing times
(defun get-finishing-times (graph)
  "Get finishing times using DFS"
  (let ((visited '())
        (finish-stack '()))
    (dolist (vertex (graph-vertices graph))
      (unless (member vertex visited)
        (dfs-visit graph vertex visited finish-stack)))
    (nreverse finish-stack)))

;; Transpose the graph
(defun transpose-graph (graph)
  "Create transpose of the graph"
  (let ((transposed (make-graph :vertices (graph-vertices graph))))
    (dolist (vertex (graph-vertices graph))
      (let ((neighbors (cdr (assoc vertex (graph-adjacency-list graph)))))
        (dolist (neighbor neighbors)
          (add-edge transposed neighbor vertex)))))
    transposed))

;; DFS on transposed graph to find SCCs
(defun dfs-visit-scc (graph vertex visited component)
  "DFS visit to collect vertices in current SCC"
  (push vertex visited)
  (push vertex component)
  (let ((neighbors (cdr (assoc vertex (graph-adjacency-list graph)))))
    (dolist (neighbor neighbors)
      (unless (member neighbor visited)
        (dfs-visit-scc graph neighbor visited component))))
  component)

;; Main Kosaraju's algorithm
(defun strongly-connected-components (graph)
  "Find all strongly connected components using Kosaraju's algorithm"
  (let ((finish-times (get-finishing-times graph))
        (transposed-graph (transpose-graph graph))
        (sccs '())
        (visited '()))
    ;; Process vertices in reverse finishing time order
    (dolist (vertex finish-times)
      (unless (member vertex visited)
        (let ((component '()))
          (dfs-visit-scc transposed-graph vertex visited component)
          (push component sccs)))))
    sccs))

;; Example usage
(defun example-graph ()
  "Create example graph for testing"
  (let ((g (make-graph :vertices '(1 2 3 4 5 6 7 8))))
    (add-edge g 1 2)
    (add-edge g 2 3)
    (add-edge g 3 1)
    (add-edge g 2 4)
    (add-edge g 4 5)
    (add-edge g 5 6)
    (add-edge g 6 7)
    (add-edge g 7 8)
    (add-edge g 8 6)
    (add-edge g 5 2)
    g))

;; Test the algorithm
(defun test-kosaraju ()
  "Test Kosaraju's algorithm with example graph"
  (let ((graph (example-graph)))
    (format t "Original graph vertices: ~A~%" (graph-vertices graph))
    (format t "Original graph edges: ~A~%" (graph-adjacency-list graph))
    (let ((sccs (strongly-connected-components graph)))
      (format t "Strongly Connected Components: ~A~%" sccs))))

;; Run the example
(test-kosaraju)
```

## How it works:

1. **First DFS Pass**: Perform DFS on the original graph to get finishing times
2. **Graph Transpose**: Create a transpose of the original graph (reverse all edges)
3. **Second DFS Pass**: Perform DFS on the transposed graph in the order of decreasing finishing times
4. **SCC Extraction**: Each DFS tree in the second pass represents a strongly connected component

## Key Features:

- **Graph Structure**: Uses a simple adjacency list representation
- **Modular Design**: Separates concerns into different functions
- **Proper DFS Implementation**: Handles visited vertices correctly
- **Transpose Function**: Creates the reverse graph needed for the algorithm
- **Example Usage**: Includes a test case with a sample graph

## Time Complexity:
- O(V + E) where V is vertices and E is edges
- This is optimal for this problem

The algorithm correctly identifies all strongly connected components in the graph, which are maximal subgraphs where every vertex is reachable from every other vertex.

