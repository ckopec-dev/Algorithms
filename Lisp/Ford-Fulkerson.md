# Ford-Fulkerson Algorithm in Lisp

Here's an implementation of the Ford-Fulkerson algorithm for finding the maximum flow in a flow network using Lisp:

```lisp
(defstruct vertex
  (name nil)
  (visited nil))

(defstruct edge
  (from nil)
  (to nil)
  (capacity 0)
  (flow 0)
  (residual nil))

(defun create-graph (vertices edges)
  "Create a graph representation with vertices and edges"
  (let ((graph (make-hash-table :test 'equal)))
    ;; Initialize vertices
    (dolist (vertex vertices)
      (setf (gethash vertex graph) (list)))
    ;; Add edges to the graph
    (dolist (edge edges)
      (let ((from (first edge))
            (to (second edge))
            (capacity (third edge)))
        (let ((forward-edge (make-edge :from from :to to :capacity capacity))
              (backward-edge (make-edge :from to :to from :capacity capacity)))
          (setf (edge-residual forward-edge) backward-edge)
          (setf (edge-residual backward-edge) forward-edge)
          (push forward-edge (gethash from graph))
          (push backward-edge (gethash to graph)))))
    graph))

(defun bfs-find-path (graph source sink)
  "Find an augmenting path using BFS"
  (let ((queue (list source))
        (visited (make-hash-table :test 'equal))
        (parent (make-hash-table :test 'equal))
        (path nil))
    ;; Mark source as visited
    (setf (gethash source visited) t)
    
    ;; BFS traversal
    (loop while queue do
      (let ((current (pop queue)))
        (dolist (edge (gethash current graph))
          (let ((to (edge-to edge))
                (capacity (edge-capacity edge))
                (flow (edge-flow edge)))
            (when (and (not (gethash to visited))
                       (> capacity flow))
              (setf (gethash to visited) t)
              (setf (gethash to parent) edge)
              (push to queue)
              (when (string= to sink)
                (return-from bfs-find-path
                  (construct-path parent sink))))))))
    nil))

(defun construct-path (parent sink)
  "Construct the path from parent links"
  (let ((path nil)
        (current sink))
    (loop while current do
      (let ((edge (gethash current parent)))
        (when edge
          (push edge path)
          (setf current (edge-from edge)))
        (unless edge
          (return))))
    path))

(defun find-min-capacity (path)
  "Find the minimum capacity along the path"
  (reduce #'min (mapcar (lambda (edge) 
                         (- (edge-capacity edge) (edge-flow edge)))
                       path)))

(defun update-flow (path flow)
  "Update the flow along the path"
  (dolist (edge path)
    (incf (edge-flow edge) flow)
    (incf (edge-flow (edge-residual edge)) (- flow))))

(defun ford-fulkerson (graph source sink)
  "Main Ford-Fulkerson algorithm implementation"
  (let ((max-flow 0)
        (path nil))
    (loop while (setf path (bfs-find-path graph source sink)) do
      (let ((min-capacity (find-min-capacity path)))
        (update-flow path min-capacity)
        (incf max-flow min-capacity)))
    max-flow))

;; Example usage
(defun example ()
  "Example: Find maximum flow from 's' to 't'"
  (let* ((vertices '("s" "a" "b" "c" "t"))
         (edges '(("s" "a" 10) ("s" "c" 10) ("a" "b" 4)
                  ("a" "c" 2) ("b" "t" 10) ("c" "b" 6) ("c" "t" 10)))
         (graph (create-graph vertices edges)))
    (format t "Maximum flow from 's' to 't': ~A~%" 
            (ford-fulkerson graph "s" "t"))))

;; Run the example
(example)
```

## How it works:

1. **Graph Representation**: Uses hash tables to store vertices and their adjacent edges
2. **Edge Structure**: Each edge has capacity, flow, and residual edge pointers
3. **BFS Path Finding**: Finds augmenting paths from source to sink
4. **Flow Updates**: Updates flows along the found paths
5. **Maximum Flow Calculation**: Continues until no more augmenting paths exist

## Example Output:
```
Maximum flow from 's' to 't': 16
```

This implementation demonstrates the core Ford-Fulkerson algorithm with BFS for finding augmenting paths, which is essentially the Edmonds-Karp algorithm variant.

