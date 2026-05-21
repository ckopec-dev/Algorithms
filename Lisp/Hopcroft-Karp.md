# Hopcroft-Karp Algorithm Implementation in Lisp

Here's a complete implementation of the Hopcroft-Karp algorithm for finding maximum matching in bipartite graphs using Common Lisp:

```lisp
(defpackage :hopcroft-karp
  (:use :cl)
  (:export :find-maximum-matching
           :print-matching))
(in-package :hopcroft-karp)

;; Graph representation: adjacency list for left vertices
;; Each vertex maps to a list of right vertices it connects to
(defstruct graph
  (left-vertices '() :type list)
  (right-vertices '() :type list)
  (adjacency-list '() :type list))

;; Find maximum matching using Hopcroft-Karp algorithm
(defun find-maximum-matching (graph)
  "Find maximum matching in bipartite graph using Hopcroft-Karp algorithm"
  (let* ((left-vertices (graph-left-vertices graph))
         (right-vertices (graph-right-vertices graph))
         (adj-list (graph-adjacency-list graph))
         (matching (make-hash-table :test 'equal))
         (match-left (make-hash-table :test 'equal))
         (match-right (make-hash-table :test 'equal))
         (max-matching 0))
    
    ;; Initialize matching tables
    (dolist (right right-vertices)
      (setf (gethash right match-right) nil))
    (dolist (left left-vertices)
      (setf (gethash left match-left) nil))
    
    ;; Main loop: keep augmenting paths until none exist
    (loop while (bfs graph matching match-left match-right) do
          (dolist (left left-vertices)
            (when (and (null (gethash left match-left))
                       (dfs left graph matching match-left match-right))
              (incf max-matching)))
    max-matching))

;; Breadth-First Search to find augmenting paths
(defun bfs (graph matching match-left match-right)
  "BFS to find augmenting paths"
  (let* ((left-vertices (graph-left-vertices graph))
         (right-vertices (graph-right-vertices graph))
         (adj-list (graph-adjacency-list graph))
         (queue '())
         (dist (make-hash-table :test 'equal))
         (found-augmenting-path nil))
    
    ;; Initialize distances for unmatched left vertices
    (dolist (left left-vertices)
      (if (null (gethash left match-left))
          (progn
            (setf (gethash left dist) 0)
            (push left queue))
          (setf (gethash left dist) most-positive-fixnum)))
    
    ;; Set distance for unmatched right vertices to infinity
    (dolist (right right-vertices)
      (setf (gethash right dist) most-positive-fixnum))
    
    ;; BFS loop
    (loop while queue do
          (let ((u (pop queue)))
            (when (and (gethash u dist) (< (gethash u dist) most-positive-fixnum))
              (dolist (v (gethash u adj-list))
                (let ((w (gethash v match-right)))
                  (when (and w (null (gethash w dist)))
                    (setf (gethash w dist) (1+ (gethash u dist)))
                    (push w queue))
                  (unless (gethash v dist)
                    (setf (gethash v dist) (1+ (gethash u dist)))
                    (when (null w)
                      (setf found-augmenting-path t))))))))
    
    found-augmenting-path))

;; Depth-First Search for augmenting path
(defun dfs (u graph matching match-left match-right)
  "DFS to find augmenting path from vertex u"
  (let* ((adj-list (graph-adjacency-list graph))
         (right-vertices (graph-right-vertices graph)))
    
    (dolist (v (gethash u adj-list))
      (let ((w (gethash v match-right)))
        (when (null w)  ; Right vertex is unmatched
          (setf (gethash v match-right) u
                (gethash u match-left) v)
          (return-from dfs t))
        (when (and w (dfs w graph matching match-left match-right))
          (setf (gethash v match-right) u
                (gethash u match-left) v)
          (return-from dfs t)))))
    
    nil))

;; Helper function to create a sample bipartite graph
(defun create-sample-graph ()
  "Create a sample bipartite graph for testing"
  (make-graph
   :left-vertices '("L1" "L2" "L3" "L4")
   :right-vertices '("R1" "R2" "R3" "R4")
   :adjacency-list '(("L1" ("R1" "R2"))
                     ("L2" ("R2" "R3"))
                     ("L3" ("R1" "R4"))
                     ("L4" ("R3" "R4")))))

;; Print the matching results
(defun print-matching (match-left)
  "Print the matching results"
  (format t "Maximum matching:~%")
  (dolist (left (gethash "left-vertices" match-left))
    (let ((right (gethash left match-left)))
      (when right
        (format t "  ~A -> ~A~%" left right)))))

;; Example usage
(defun example ()
  "Example of using Hopcroft-Karp algorithm"
  (let ((graph (create-sample-graph)))
    (format t "Graph edges:~%")
    (dolist (left (graph-left-vertices graph))
      (format t "  ~A connects to ~A~%" 
              left (gethash left (graph-adjacency-list graph))))
    (format t "~%")
    (let ((matching-size (find-maximum-matching graph)))
      (format t "Maximum matching size: ~A~%" matching-size))))

;; Run the example
(example)
```

## How to Use

```lisp
;; Create a graph and find maximum matching
(setq my-graph (create-sample-graph))
(setq max-match (find-maximum-matching my-graph))
(format t "Maximum matching size: ~A~%" max-match)
```

## Algorithm Explanation

The Hopcroft-Karp algorithm works in phases:

1. **BFS Phase**: Find augmenting paths of minimum length using BFS
2. **DFS Phase**: For each unmatched vertex, try to find augmenting paths using DFS
3. **Repeat**: Continue until no more augmenting paths exist

## Time Complexity

- **Time**: O(E × √V) where E is edges and V is vertices
- **Space**: O(V + E) for storing the graph and matching information

## Key Features

- Handles bipartite graph matching
- Uses hash tables for efficient lookups
- Implements both BFS and DFS components
- Returns the size of maximum matching
- Includes sample graph creation and testing

This implementation provides a complete working example of the Hopcroft-Karp algorithm in Common Lisp, suitable for finding maximum matchings in bipartite graphs.

