# Borůvka's Algorithm Implementation in Lisp

Here's an implementation of Borůvka's algorithm for finding Minimum Spanning Tree (MST) in Lisp:

```lisp
;; Borůvka's Algorithm for Minimum Spanning Tree
;; Assumes we have a graph represented as an adjacency list

(defun find-min-edge (graph components)
  "Find the minimum weight edge connecting each component to another component"
  (let ((min-edges (make-hash-table :test 'equal)))
    (dolist (component components)
      (dolist (node component)
        (let ((neighbors (gethash node graph)))
          (dolist (edge neighbors)
            (let ((to (first edge))
                  (weight (second edge)))
              (unless (member to component)
                (let ((current-min (gethash to min-edges)))
                  (if (or (null current-min) (< weight (second current-min)))
                      (puthash to (list node weight) min-edges)))))))))
    min-edges))

(defun boruvka-mst (graph vertices)
  "Implementation of Borůvka's algorithm for MST"
  (let ((components (mapcar (lambda (v) (list v)) vertices))
        (mst-edges '())
        (total-cost 0))
    (format t "Starting Borůvka's algorithm~%")
    (format t "Initial components: ~A~%" components)
    
    (loop while (> (length components) 1) do
      (let ((min-edges (find-min-edge graph components)))
        (format t "Minimum edges found: ~A~%" min-edges)
        
        ;; Collect edges to add to MST
        (let ((edges-to-add '()))
          (maphash (lambda (node edge)
                      (push edge edges-to-add))
                    min-edges)
          
          ;; Add edges to MST and merge components
          (dolist (edge edges-to-add)
            (let ((from (first edge))
                  (weight (second edge)))
              (format t "Adding edge from ~A to ~A with weight ~A~%" 
                      from (gethash from min-edges) weight)
              (push (list from (gethash from min-edges) weight) mst-edges)
              (incf total-cost weight)
              
              ;; Merge components (simplified approach)
              (let ((comp1 (find (gethash from min-edges) components :key #'first))
                    (comp2 (find from components :key #'first)))
                (when (and comp1 comp2)
                  (let ((new-comp (append comp1 comp2)))
                    (setf components (remove comp1 components))
                    (setf components (remove comp2 components))
                    (push new-comp components)))))))
        
        (format t "Components after merge: ~A~%" components)
        (format t "Total cost so far: ~A~%" total-cost)))
    
    (format t "Final MST edges: ~A~%" mst-edges)
    (format t "Total MST cost: ~A~%" total-cost)
    (list mst-edges total-cost)))

;; Example usage:
;; Define a graph as adjacency list
(defvar *example-graph*
  '((1 ((2 4) (3 2)))
    (2 ((1 4) (3 1) (4 5)))
    (3 ((1 2) (2 1) (4 8) (5 10)))
    (4 ((2 5) (3 8) (5 2)))
    (5 ((3 10) (4 2)))))

(defvar *example-vertices* '(1 2 3 4 5))

;; Run Borůvka's algorithm
(boruvka-mst *example-graph* *example-vertices*)
```

## How it works:

1. **Initialization**: Each vertex starts as its own component
2. **Find minimum edges**: For each component, find the minimum weight edge connecting it to another component
3. **Merge components**: Add the minimum edges to the MST and merge the components they connect
4. **Repeat**: Continue until all vertices are in one component

## Key Features:

- Uses hash tables for efficient edge lookups
- Tracks component merging throughout the algorithm
- Provides detailed output showing the progress
- Handles the core logic of Borůvka's algorithm efficiently

## Time Complexity:
- O(E log V) where E is the number of edges and V is the number of vertices
- More efficient than Kruskal's or Prim's for sparse graphs

This implementation demonstrates the fundamental approach of Borůvka's algorithm, which processes components in rounds, finding the minimum edge for each component in each round.

