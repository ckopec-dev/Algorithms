# Edmonds-Karp Algorithm in Lisp

Here's an implementation of the Edmonds-Karp algorithm (maximum flow) in Common Lisp:

```lisp
(defpackage :edmonds-karp
  (:use :cl)
  (:export :max-flow :print-graph))
(in-package :edmonds-karp)

;; Graph representation as adjacency list with capacity information
(defstruct edge
  from to capacity flow)

(defstruct graph
  (vertices 0)
  (edges '())
  (adjacency-list (make-hash-table)))

;; Add edge to the graph
(defun add-edge (graph from to capacity)
  "Add a directed edge with given capacity"
  (let ((edge (make-edge :from from :to to :capacity capacity :flow 0)))
    (push edge (graph-edges graph))
    (push edge (gethash from (graph-adjacency-list graph) '()))
    (gethash from (graph-adjacency-list graph))))

;; Create a new graph
(defun make-graph (vertices)
  "Create a new graph with specified number of vertices"
  (make-graph :vertices vertices
              :edges '()
              :adjacency-list (make-hash-table)))

;; BFS to find augmenting path
(defun bfs-find-path (graph source sink)
  "Find augmenting path using BFS, return path and minimum capacity"
  (let ((visited (make-array (graph-vertices graph) :element-type 'bit :initial-element 0))
        (queue '())
        (parent (make-array (graph-vertices graph) :initial-element -1))
        (min-capacity most-positive-fixnum))
    (setf (aref visited source) 1)
    (push source queue)
    
    (loop while queue do
      (let ((current (pop queue)))
        (dolist (edge (gethash current (graph-adjacency-list graph)))
          (let ((to (edge-to edge))
                (capacity (edge-capacity edge))
                (flow (edge-flow edge)))
            (when (and (= (aref visited to) 0)
                       (> (- capacity flow) 0))
              (setf (aref visited to) 1)
              (setf (aref parent to) current)
              (push to queue)
              (let ((new-capacity (min min-capacity (- capacity flow))))
                (when (= to sink)
                  (return-from bfs-find-path 
                    (values (list sink) new-capacity)))
                (setf min-capacity new-capacity)))))))
    (values nil nil)))

;; Find augmenting path using BFS
(defun find-augmenting-path (graph source sink)
  "Find an augmenting path using BFS"
  (let ((visited (make-array (graph-vertices graph) :element-type 'bit :initial-element 0))
        (queue '())
        (parent (make-array (graph-vertices graph) :initial-element -1))
        (path '()))
    (setf (aref visited source) 1)
    (push source queue)
    
    (loop while queue do
      (let ((current (pop queue)))
        (dolist (edge (gethash current (graph-adjacency-list graph)))
          (let ((to (edge-to edge))
                (capacity (edge-capacity edge))
                (flow (edge-flow edge)))
            (when (and (= (aref visited to) 0)
                       (> (- capacity flow) 0))
              (setf (aref visited to) 1)
              (setf (aref parent to) current)
              (push to queue)
              (when (= to sink)
                (return-from find-augmenting-path 
                  (let ((path '()))
                    (loop for node = sink then (aref parent node)
                          while (>= node 0)
                          do (push node path))
                    (values path (aref visited sink)))))))))))
    (values nil nil)))

;; Get minimum capacity along path
(defun get-min-capacity (graph path)
  "Get minimum capacity along the given path"
  (let ((min-capacity most-positive-fixnum))
    (loop for i from 0 to (1- (length path))
          for current = (nth i path)
          for next = (nth (1+ i) path)
          when next do
            (let ((edges (gethash current (graph-adjacency-list graph))))
              (dolist (edge edges)
                (when (and (= (edge-to edge) next)
                           (< min-capacity (edge-capacity edge)))
                  (setf min-capacity (edge-capacity edge)))))
    min-capacity))

;; Augment flow along path
(defun augment-flow (graph path min-capacity)
  "Augment flow along the given path"
  (loop for i from 0 to (1- (length path))
        for current = (nth i path)
        for next = (nth (1+ i) path)
        when next do
          (let ((edges (gethash current (graph-adjacency-list graph))))
            (dolist (edge edges)
              (when (= (edge-to edge) next)
                (incf (edge-flow edge) min-capacity)
                (decf (edge-capacity edge) min-capacity))))))

;; Main Edmonds-Karp algorithm
(defun max-flow (graph source sink)
  "Compute maximum flow using Edmonds-Karp algorithm"
  (let ((total-flow 0))
    (loop while (let ((path (find-augmenting-path graph source sink)))
                  (when path
                    (let ((min-capacity (get-min-capacity graph path)))
                      (augment-flow graph path min-capacity)
                      (incf total-flow min-capacity)
                      t)))
          do (format t "Found augmenting path, flow increased by ~A~%" 
                     (get-min-capacity graph path)))
    total-flow))

;; Simplified version with clearer structure
(defun edmonds-karp (graph source sink)
  "Simplified Edmonds-Karp implementation"
  (let ((total-flow 0))
    (loop
      (let ((path (bfs-augmenting-path graph source sink)))
        (if (null path)
            (return total-flow)
            (let ((min-capacity (get-path-capacity graph path)))
              (augment-path graph path min-capacity)
              (incf total-flow min-capacity)))))))

;; Helper function to find path using BFS
(defun bfs-augmenting-path (graph source sink)
  "Find augmenting path using BFS"
  (let ((visited (make-array (graph-vertices graph) :element-type 'bit :initial-element 0))
        (queue (list source))
        (parent (make-array (graph-vertices graph) :initial-element -1)))
    (setf (aref visited source) 1)
    
    (loop while queue do
      (let ((current (pop queue)))
        (dolist (edge (gethash current (graph-adjacency-list graph)))
          (let ((to (edge-to edge))
                (capacity (edge-capacity edge))
                (flow (edge-flow edge)))
            (when (and (= (aref visited to) 0)
                       (> (- capacity flow) 0))
              (setf (aref visited to) 1)
              (setf (aref parent to) current)
              (push to queue)
              (when (= to sink)
                (return-from bfs-augmenting-path 
                  (let ((path '()))
                    (loop for node = sink then (aref parent node)
                          while (>= node 0)
                          do (push node path))
                    (nreverse path)))))))))
    nil))

;; Get capacity of a path
(defun get-path-capacity (graph path)
  "Get minimum capacity along the path"
  (let ((min-cap 1000000))
    (loop for i from 0 to (1- (length path))
          for current = (nth i path)
          for next = (nth (1+ i) path)
          when next do
            (let ((edges (gethash current (graph-adjacency-list graph))))
              (dolist (edge edges)
                (when (= (edge-to edge) next)
                  (setf min-cap (min min-cap 
                                     (- (edge-capacity edge) 
                                        (edge-flow edge))))))))
    min-cap))

;; Augment flow along path
(defun augment-path (graph path min-capacity)
  "Augment flow along the given path"
  (loop for i from 0 to (1- (length path))
        for current = (nth i path)
        for next = (nth (1+ i) path)
        when next do
          (let ((edges (gethash current (graph-adjacency-list graph))))
            (dolist (edge edges)
              (when (= (edge-to edge) next)
                (incf (edge-flow edge) min-capacity)
                (decf (edge-capacity edge) min-capacity))))))

;; Example usage
(defun example-graph ()
  "Create example graph for testing"
  (let ((g (make-graph 6)))
    ;; Add edges: from, to, capacity
    (add-edge g 0 1 10)
    (add-edge g 0 2 10)
    (add-edge g 1 2 2)
    (add-edge g 1 3 4)
    (add-edge g 2 1 6)
    (add-edge g 2 4 10)
    (add-edge g 3 2 6)
    (add-edge g 3 5 10)
    (add-edge g 4 3 6)
    (add-edge g 4 5 10)
    g))

;; Test the algorithm
(defun test-edmonds-karp ()
  "Test the Edmonds-Karp implementation"
  (let ((graph (example-graph)))
    (format t "Maximum flow from vertex 0 to vertex 5: ~A~%" 
            (edmonds-karp graph 0 5))))

;; Print graph structure
(defun print-graph (graph)
  "Print the graph structure"
  (format t "Graph with ~A vertices~%" (graph-vertices graph))
  (format t "Edges:~%")
  (dolist (edge (graph-edges graph))
    (format t "  ~A -> ~A (capacity: ~A, flow: ~A)~%" 
            (edge-from edge) (edge-to edge) 
            (edge-capacity edge) (edge-flow edge))))

;; Run example
(test-edmonds-karp)
```

## Example Usage

```lisp
;; Create a sample graph
(let ((g (make-graph 6)))
  ;; Add edges with capacities
  (add-edge g 0 1 10)  ; 0 -> 1, capacity 10
  (add-edge g 0 2 10)  ; 0 -> 2, capacity 10
  (add-edge g 1 2 2)   ; 1 -> 2, capacity 2
  (add-edge g 1 3 4)   ; 1 -> 3, capacity 4
  (add-edge g 2 1 6)   ; 2 -> 1, capacity 6
  (add-edge g 2 4 10)  ; 2 -> 4, capacity 10
  (add-edge g 3 2 6)   ; 3 -> 2, capacity 6
  (add-edge g 3 5 10)  ; 3 -> 5, capacity 10
  (add-edge g 4 3 6)   ; 4 -> 3, capacity 6
  (add-edge g 4 5 10)  ; 4 -> 5, capacity 10
  
  ;; Compute maximum flow from vertex 0 to vertex 5
  (let ((max-flow (edmonds-karp g 0 5)))
    (format t "Maximum flow: ~A~%" max-flow)))
```

## Key Features

1. **Graph Representation**: Uses adjacency list with edge structures
2. **BFS Path Finding**: Finds augmenting paths using breadth-first search
3. **Flow Augmentation**: Updates flow along found paths
4. **Time Complexity**: O(VE²) where V is vertices and E is edges
5. **Space Complexity**: O(V + E) for storing the graph

This implementation demonstrates the core concepts of the Edmonds-Karp algorithm for finding maximum flow in a flow network.

