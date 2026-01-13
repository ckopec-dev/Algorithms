# Edmonds-Karp Algorithm Implementation in Lisp

Here's a complete implementation of the Edmonds-Karp algorithm for finding maximum flow in a flow network using Lisp:

```lisp
(defparameter *inf* 1000000)

;; Graph representation as adjacency list
(defun make-graph (vertices)
  "Create an empty graph with given number of vertices"
  (make-array vertices :initial-element nil))

(defun add-edge (graph from to capacity)
  "Add an edge to the graph with given capacity"
  (let ((edges (aref graph from)))
    (setf (aref graph from) 
          (cons (list to capacity 0) edges)) ; to, capacity, flow
    ;; Add reverse edge (residual)
    (let ((reverse-edges (aref graph to)))
      (setf (aref graph to) 
            (cons (list from 0 0) reverse-edges)))))

(defun bfs (graph source sink parent)
  "Breadth-first search to find augmenting path"
  (let ((visited (make-array (length graph) :initial-element nil))
        (queue '()))
    (setf (aref visited source) t)
    (push source queue)
    
    (loop while queue do
      (let ((u (pop queue)))
        (loop for (v cap flow) in (aref graph u) do
          (when (and (not (aref visited v))
                     (> cap flow))
            (setf (aref visited v) t
                  (aref parent v) u)
            (if (= v sink)
                (return-from bfs t)
                (push v queue)))))))
  nil)

(defun find-min-capacity (graph source sink parent)
  "Find minimum capacity along the path from source to sink"
  (let ((min-cap *inf*))
    (loop while (and (not (= source sink))
                     (aref parent sink))
          do
      (let ((u (aref parent sink))
            (v sink))
        (loop for (to cap flow) in (aref graph u) do
          (when (= to v)
            (setf min-cap (min min-cap (- cap flow))
                  sink u)
            (return)))))
    min-cap))

(defun update-flow (graph source sink parent min-cap)
  "Update flow along the path"
  (loop while (and (not (= source sink))
                   (aref parent sink))
        do
    (let ((u (aref parent sink))
          (v sink))
      (loop for (to cap flow) in (aref graph u) do
        (when (= to v)
          (setf (caddr (assoc to (aref graph u))) 
                (+ flow min-cap)
                (caddr (assoc u (aref graph v))) 
                (- flow min-cap)
                sink u)
          (return)))))
  min-cap)

(defun edmonds-karp (graph source sink)
  "Main Edmonds-Karp algorithm"
  (let ((parent (make-array (length graph) :initial-element -1))
        (max-flow 0))
    (loop while (bfs graph source sink parent) do
      (let ((min-cap (find-min-capacity graph source sink parent)))
        (setf max-flow (+ max-flow min-cap))
        (update-flow graph source sink parent min-cap)
        (loop for i from 0 below (length parent)
              do (setf (aref parent i) -1))))
    max-flow))

;; Example usage
(defun example ()
  "Example: Find maximum flow from vertex 0 to vertex 5"
  (let* ((vertices 6)
         (graph (make-graph vertices)))
    
    ;; Add edges with capacities
    (add-edge graph 0 1 10)
    (add-edge graph 0 2 10)
    (add-edge graph 1 2 2)
    (add-edge graph 1 3 4)
    (add-edge graph 1 4 8)
    (add-edge graph 2 4 9)
    (add-edge graph 3 5 10)
    (add-edge graph 4 5 10)
    
    (let ((max-flow (edmonds-karp graph 0 5)))
      (format t "Maximum flow: ~A~%" max-flow)
      max-flow)))

;; Run the example
(example)
```

## Algorithm Explanation

The Edmonds-Karp algorithm is a specific implementation of the Ford-Fulkerson method that uses BFS to find augmenting paths, ensuring polynomial time complexity.

### Key Components:

1. **Graph Representation**: Adjacency list with edges storing (to, capacity, flow)
2. **BFS Function**: Finds augmenting path using breadth-first search
3. **Minimum Capacity**: Determines bottleneck capacity along the path
4. **Flow Update**: Updates forward and reverse edges in the residual graph
5. **Main Loop**: Repeatedly finds augmenting paths and increases flow

### Time Complexity:
- **O(VE²)** where V is vertices and E is edges
- Better than basic Ford-Fulkerson in worst case

### Example Network:
```
    0 ----10---- 1 ----4---- 3
    |           |           |
   10          2           10
    |           |           |
    2 ----9---- 4 ----10---- 5
```

This implementation demonstrates the core concepts of maximum flow algorithms and can be easily extended for various network flow problems.

