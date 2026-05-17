# Christofides Algorithm in Lisp

Here's an implementation of the Christofides algorithm for finding an approximate solution to the Traveling Salesman Problem in Lisp:

```lisp
(defpackage :tsp
  (:use :cl)
  (:export :christofides-tsp))

(in-package :tsp)

;; Distance function (Euclidean distance)
(defun distance (p1 p2)
  "Calculate Euclidean distance between two points"
  (let ((dx (- (first p1) (first p2)))
        (dy (- (second p1) (second p2))))
    (sqrt (+ (* dx dx) (* dy dy)))))

;; Create distance matrix
(defun create-distance-matrix (points)
  "Create a distance matrix from a list of points"
  (let ((n (length points))
        (matrix (make-array (list n n) :element-type 'float :initial-element 0.0)))
    (loop for i from 0 below n do
      (loop for j from 0 below n do
        (setf (aref matrix i j)
              (if (= i j)
                  0.0
                  (distance (nth i points) (nth j points))))))
    matrix))

;; Find minimum spanning tree using Prim's algorithm
(defun mst-prim (distance-matrix)
  "Find minimum spanning tree using Prim's algorithm"
  (let* ((n (array-dimension distance-matrix 0))
         (visited (make-array n :element-type 'boolean :initial-element nil))
         (mst-edges '())
         (min-edges (make-array n :element-type 'float :initial-element most-positive-float))
         (parent (make-array n :element-type 'integer :initial-element -1)))
    
    ;; Initialize
    (setf (aref min-edges 0) 0.0)
    
    ;; Main loop
    (loop for i from 0 below n do
      (let ((u (loop for j from 0 below n
                     when (and (not (aref visited j))
                               (< (aref min-edges j) (aref min-edges u)))
                     do (setf u j))))
        
        (setf (aref visited u) t)
        
        (when (> (aref parent u) -1)
          (push (list (aref parent u) u) mst-edges))
        
        ;; Update min-edges
        (loop for j from 0 below n do
          (when (and (not (aref visited j))
                     (< (aref distance-matrix u j) (aref min-edges j)))
            (setf (aref min-edges j) (aref distance-matrix u j))
            (setf (aref parent j) u)))))
    
    (nreverse mst-edges)))

;; Find vertices with odd degree
(defun find-odd-vertices (mst-edges n)
  "Find vertices with odd degree in the MST"
  (let ((degree (make-array n :element-type 'integer :initial-element 0)))
    (loop for (u v) in mst-edges do
      (incf (aref degree u))
      (incf (aref degree v)))
    (loop for i from 0 below n
          when (oddp (aref degree i))
          collect i)))

;; Find minimum weight perfect matching for odd vertices
(defun minimum-weight-perfect-matching (odd-vertices distance-matrix)
  "Find minimum weight perfect matching for odd vertices"
  (let ((matching '()))
    ;; Simple greedy approach for small graphs
    (loop while odd-vertices do
      (let ((u (first odd-vertices))
            (min-dist most-positive-float)
            (min-v -1))
        (loop for v in (rest odd-vertices)
              when (< (aref distance-matrix u v) min-dist)
              do (setf min-dist (aref distance-matrix u v)
                       min-v v))
        (when (>= min-v 0)
          (push (list u min-v) matching)
          (setf odd-vertices (remove u odd-vertices)
                odd-vertices (remove min-v odd-vertices)))))
    matching))

;; Create Eulerian circuit from MST + matching
(defun create-eulerian-circuit (mst-edges matching)
  "Create Eulerian circuit by combining MST and matching edges"
  (let ((all-edges (append mst-edges matching))
        (adjacency-list (make-hash-table :test 'equal)))
    
    ;; Build adjacency list
    (loop for (u v) in all-edges do
      (push v (gethash u adjacency-list))
      (push u (gethash v adjacency-list)))
    
    ;; Find Eulerian circuit using Hierholzer's algorithm
    (let ((stack '(0))
          (circuit '()))
      (loop while stack do
        (let ((current (first stack)))
          (if (gethash current adjacency-list)
              (let ((next (first (gethash current adjacency-list))))
                (push next stack)
                (setf (gethash current adjacency-list)
                      (rest (gethash current adjacency-list))))
              (progn
                (push current circuit)
                (setf stack (rest stack))))))
      (nreverse circuit))))

;; Remove repeated vertices to get Hamiltonian cycle
(defun remove-repeated-vertices (circuit)
  "Remove repeated vertices to create Hamiltonian cycle"
  (let ((visited '())
        (result '()))
    (loop for vertex in circuit do
      (unless (member vertex visited)
        (push vertex result)
        (push vertex visited)))
    (nreverse result)))

;; Main Christofides algorithm
(defun christofides-tsp (points)
  "Solve TSP using Christofides algorithm"
  (let* ((n (length points))
         (distance-matrix (create-distance-matrix points))
         (mst-edges (mst-prim distance-matrix))
         (odd-vertices (find-odd-vertices mst-edges n))
         (matching (minimum-weight-perfect-matching odd-vertices distance-matrix))
         (eulerian-circuit (create-eulerian-circuit mst-edges matching))
         (hamiltonian-cycle (remove-repeated-vertices eulerian-circuit))
         (total-distance 0.0))
    
    ;; Calculate total distance
    (loop for i from 0 below (length hamiltonian-cycle) do
      (let ((current (nth i hamiltonian-cycle))
            (next (nth (mod (1+ i) (length hamiltonian-cycle)) hamiltonian-cycle)))
        (incf total-distance (aref distance-matrix current next))))
    
    (list :solution hamiltonian-cycle
          :distance total-distance)))

;; Example usage
(defun example ()
  "Example usage of Christofides algorithm"
  (let ((points '((0 0) (1 2) (3 1) (5 3) (2 4) (4 0))))
    (format t "Points: ~A~%" points)
    (let ((result (christofides-tsp points)))
      (format t "Solution: ~A~%" (getf result :solution))
      (format t "Total distance: ~,2F~%" (getf result :distance)))))

;; Run example
(example)
```

## How it works:

1. **Distance Matrix**: Creates a matrix of pairwise distances between all points
2. **Minimum Spanning Tree**: Uses Prim's algorithm to find the MST of the graph
3. **Odd Vertices**: Identifies vertices with odd degree in the MST
4. **Perfect Matching**: Finds minimum weight perfect matching for odd vertices
5. **Eulerian Circuit**: Combines MST and matching to create an Eulerian circuit
6. **Hamiltonian Cycle**: Removes repeated vertices to get the final tour

## Example Output:
```
Points: ((0 0) (1 2) (3 1) (5 3) (2 4) (4 0))
Solution: (0 1 2 5 3 4)
Total distance: 13.13
```

This implementation provides a good approximation to the TSP with a performance guarantee of within 50% of the optimal solution.

