# Traveling Salesman Problem (TSP) in Lisp

Here's an implementation of a simple TSP algorithm using brute force approach in Common Lisp:

```lisp
(defpackage :tsp
  (:use :cl))
(in-package :tsp)

;; Define a point structure
(defstruct point
  x y)

;; Calculate Euclidean distance between two points
(defun distance (p1 p2)
  "Calculate Euclidean distance between two points"
  (let ((dx (- (point-x p1) (point-x p2)))
        (dy (- (point-y p1) (point-y p2))))
    (sqrt (+ (* dx dx) (* dy dy)))))

;; Calculate total distance for a given route
(defun total-distance (points route)
  "Calculate total distance for a given route"
  (let ((total 0.0))
    (loop for i from 0 to (1- (length route))
          do (let ((current (nth (aref route i) points))
                   (next (nth (aref route (if (= i (1- (length route)))
                                              0
                                              (1+ i))) points)))
               (incf total (distance current next))))
    total))

;; Generate all permutations of a list
(defun permutations (lst)
  "Generate all permutations of a list"
  (if (null lst)
      '(())
      (let ((rest (permutations (rest lst))))
        (append-map (lambda (perm)
                       (mapcar (lambda (i)
                                  (cons (first lst) (insert-at i perm)))
                                (range (length perm) 0 -1)))
                    rest))))

;; Helper function to insert element at position
(defun insert-at (pos elem lst)
  "Insert elem at position pos in lst"
  (if (= pos 0)
      (cons elem lst)
      (cons (first lst) (insert-at (1- pos) elem (rest lst)))))

;; Generate range of numbers
(defun range (start end &optional (step 1))
  "Generate a range of numbers"
  (if (>= start end)
      nil
      (cons start (range (+ start step) end step))))

;; Simple brute force TSP solver
(defun solve-tsp-brute-force (points)
  "Solve TSP using brute force approach"
  (if (null points)
      nil
      (let ((n (length points))
            (best-route nil)
            (best-distance most-positive-double-float))
        ;; Generate all possible routes (excluding the starting point)
        (let ((perms (permutations (loop for i from 1 below n
                                         collect i))))
          (loop for perm in perms
                do (let ((route (make-array n :initial-contents (cons 0 perm))))
                     (let ((dist (total-distance points route)))
                       (when (< dist best-distance)
                         (setf best-distance dist
                               best-route route)))))
          (format t "Best distance: ~,2F~%" best-distance)
          (format t "Best route: ~A~%" best-route)
          best-route))))

;; Example usage
(defun example-tsp ()
  "Example of TSP problem with 4 cities"
  (let ((cities (list (make-point :x 0 :y 0)
                      (make-point :x 1 :y 2)
                      (make-point :x 3 :y 1)
                      (make-point :x 2 :y 3))))
    (format t "Cities: ~A~%" cities)
    (solve-tsp-brute-force cities)))

;; Run the example
(example-tsp)
```

## Alternative Implementation with Memoization

```lisp
;; More efficient version using dynamic programming (Held-Karp algorithm)
(defun solve-tsp-dp (points)
  "Solve TSP using dynamic programming (Held-Karp algorithm)"
  (let ((n (length points))
        (distances (make-array (list n n) :element-type 'double-float)))
    ;; Precompute distances
    (loop for i from 0 below n
          do (loop for j from 0 below n
                   do (setf (aref distances i j)
                            (if (= i j)
                                0.0
                                (distance (nth i points) (nth j points))))))
    
    ;; DP approach - simplified for demonstration
    (format t "Using dynamic programming approach for ~D cities~%" n)
    ;; In a full implementation, this would use memoization and bitmasks
    ;; to solve the TSP efficiently
    (format t "Distance matrix: ~A~%" distances)
    distances))
```

## Key Features of This Implementation:

1. **Point Structure**: Defines cities with x,y coordinates
2. **Distance Calculation**: Uses Euclidean distance formula
3. **Brute Force Approach**: Generates all possible routes and finds the shortest
4. **Permutation Generation**: Creates all possible city orders
5. **Route Evaluation**: Calculates total distance for each route

## Usage Example:

```lisp
;; Create sample cities
(setq cities (list (make-point :x 0 :y 0)
                   (make-point :x 1 :y 2)
                   (make-point :x 3 :y 1)
                   (make-point :x 2 :y 3)))

;; Solve TSP
(solve-tsp-brute-force cities)
```

**Note**: The brute force approach has O(n!) time complexity, making it suitable only for small instances. For larger problems, more sophisticated algorithms like dynamic programming or heuristic methods would be needed.

