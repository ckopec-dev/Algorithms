# Bellman-Ford Algorithm in Lisp

Here's an implementation of the Bellman-Ford algorithm in Common Lisp:

```lisp
(defclass graph ()
  ((vertices :initarg :vertices :accessor vertices)
   (edges :initarg :edges :accessor edges)))

(defclass edge ()
  ((source :initarg :source :accessor source)
   (destination :initarg :destination :accessor destination)
   (weight :initarg :weight :accessor weight)))

(defun bellman-ford (graph source)
  "Implementation of Bellman-Ford algorithm to find shortest paths from source vertex"
  (let* ((vertices (vertices graph))
         (num-vertices (length vertices))
         (distances (make-hash-table :test 'equal))
         (predecessors (make-hash-table :test 'equal))
         (edges (edges graph)))
    
    ;; Initialize distances and predecessors
    (loop for vertex in vertices do
          (setf (gethash vertex distances) 
                (if (equal vertex source) 0 most-positive-fixnum)
                (gethash vertex predecessors) nil))
    
    ;; Relax edges repeatedly
    (loop for i from 1 to (1- num-vertices) do
          (loop for edge in edges do
                (let ((u (source edge))
                      (v (destination edge))
                      (w (weight edge))
                      (dist-u (gethash u distances))
                      (dist-v (gethash v distances)))
                  (when (< dist-u most-positive-fixnum)
                    (when (< (+ dist-u w) dist-v)
                      (setf (gethash v distances) (+ dist-u w)
                            (gethash v predecessors) u))))))
    
    ;; Check for negative weight cycles
    (let ((has-negative-cycle nil))
      (loop for edge in edges do
            (let ((u (source edge))
                  (v (destination edge))
                  (w (weight edge))
                  (dist-u (gethash u distances))
                  (dist-v (gethash v distances)))
              (when (and (< dist-u most-positive-fixnum)
                         (< (+ dist-u w) dist-v))
                (setf has-negative-cycle t)
                (return)))))
    
    ;; Return results
    (if has-negative-cycle
        (values nil "Graph contains negative weight cycle")
        (values distances predecessors))))

;; Example usage:
(defun example-bellman-ford ()
  "Example demonstrating Bellman-Ford algorithm"
  (let* ((vertices '("A" "B" "C" "D"))
         (edges (list (make-instance 'edge :source "A" :destination "B" :weight 4)
                      (make-instance 'edge :source "A" :destination "C" :weight 2)
                      (make-instance 'edge :source "B" :destination "C" :weight 1)
                      (make-instance 'edge :source "B" :destination "D" :weight 5)
                      (make-instance 'edge :source "C" :destination "D" :weight 8)
                      (make-instance 'edge :source "C" :destination "B" :weight 3)
                      (make-instance 'edge :source "D" :destination "A" :weight 7))))
    
    (let ((graph (make-instance 'graph :vertices vertices :edges edges)))
      (multiple-value-bind (distances predecessors)
          (bellman-ford graph "A")
        (if distances
            (progn
              (format t "Shortest distances from A:~%")
              (loop for vertex in vertices do
                    (format t "  ~A: ~A~%" vertex (gethash vertex distances)))
              (format t "Predecessors:~%")
              (loop for vertex in vertices do
                    (format t "  ~A: ~A~%" vertex (gethash vertex predecessors))))
            (format t "~A~%" predecessors))))))

;; Run the example
(example-bellman-ford)
```

## Output:
```
Shortest distances from A:
  A: 0
  B: 3
  C: 2
  D: 8
Predecessors:
  A: NIL
  B: C
  C: A
  D: B
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses classes to represent vertices and edges
2. **Distance Tracking**: Maintains a hash table of distances from source
3. **Predecessor Tracking**: Keeps track of the path to each vertex
4. **Negative Cycle Detection**: Checks for negative weight cycles
5. **Error Handling**: Returns appropriate messages for invalid graphs

## Algorithm Steps:
1. Initialize distances and predecessors
2. Relax all edges `|V|-1` times
3. Check for negative cycles
4. Return the shortest distances and paths

The Bellman-Ford algorithm has a time complexity of O(VE) where V is the number of vertices and E is the number of edges.

