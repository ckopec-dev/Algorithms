# SPFA Algorithm in Lisp

Here's an implementation of the Shortest Path Faster Algorithm (SPFA) in Common Lisp:

```lisp
(defstruct edge
  from to weight)

(defun spfa (graph source vertices)
  "SPFA algorithm to find shortest paths from source to all vertices"
  (let ((dist (make-array vertices :initial-element most-positive-fixnum))
        (in-queue (make-array vertices :initial-element nil))
        (queue '()))
    ;; Initialize source distance
    (setf (aref dist source) 0)
    (push source queue)
    (setf (aref in-queue source) t)
    
    ;; Main SPFA loop
    (loop while queue do
      (let ((u (pop queue)))
        (setf (aref in-queue u) nil)
        ;; Check all neighbors of u
        (loop for edge in graph
              when (= (edge-from edge) u) do
                (let ((v (edge-to edge))
                      (weight (edge-weight edge))
                      (alt-dist (+ (aref dist u) weight)))
                  (when (< alt-dist (aref dist v))
                    (setf (aref dist v) alt-dist)
                    (unless (aref in-queue v)
                      (push v queue)
                      (setf (aref in-queue v) t)))))))
    
    dist))

;; Example usage
(defun example-spfa ()
  "Example of using SPFA algorithm"
  (let* ((edges (list (make-edge :from 0 :to 1 :weight 4)
                      (make-edge :from 0 :to 2 :weight 2)
                      (make-edge :from 1 :to 2 :weight 1)
                      (make-edge :from 1 :to 3 :weight 5)
                      (make-edge :from 2 :to 3 :weight 8)
                      (make-edge :from 2 :to 4 :weight 10)
                      (make-edge :from 3 :to 4 :weight 2)))
         (distances (spfa edges 0 5)))
    distances))

;; Alternative implementation using hash tables
(defun spfa-hash (graph source)
  "SPFA using hash tables for sparse graphs"
  (let ((dist (make-hash-table))
        (in-queue (make-hash-table))
        (queue '()))
    ;; Initialize distances
    (loop for i from 0 to (1- (length graph)) do
      (setf (gethash i dist) most-positive-fixnum)
      (setf (gethash i in-queue) nil))
    (setf (gethash source dist) 0)
    (push source queue)
    (setf (gethash source in-queue) t)
    
    ;; Main loop
    (loop while queue do
      (let ((u (pop queue)))
        (setf (gethash u in-queue) nil)
        ;; Process neighbors
        (loop for edge in (gethash u graph)
              when edge do
                (let ((v (edge-to edge))
                      (weight (edge-weight edge))
                      (alt-dist (+ (gethash u dist) weight)))
                  (when (< alt-dist (gethash v dist))
                    (setf (gethash v dist) alt-dist)
                    (unless (gethash v in-queue)
                      (push v queue)
                      (setf (gethash v in-queue) t)))))))
    
    dist))
```

## Example Usage

```lisp
;; Create a simple graph with 5 vertices
;; Edges: 0->1(4), 0->2(2), 1->2(1), 1->3(5), 2->3(8), 2->4(10), 3->4(2)

;; Call the function
(spfa edges 0 5)

;; Expected output: [0 4 2 9 11]
;; Distances from vertex 0 to vertices 0,1,2,3,4
```

## Key Features of this Implementation

1. **Data Structures**: Uses arrays for efficient indexing and boolean flags for queue tracking
2. **Queue Management**: Maintains a queue of vertices that need to be processed
3. **Optimization**: Only adds vertices to the queue when their distance is updated
4. **Time Complexity**: O(VE) in worst case, but often much better in practice
5. **Space Complexity**: O(V) for storing distances and queue state

The SPFA algorithm is particularly useful for graphs with negative edge weights and can detect negative cycles, making it more versatile than Dijkstra's algorithm.

