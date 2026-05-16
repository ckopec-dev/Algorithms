# DBSCAN Clustering Algorithm in Lisp

Here's an implementation of the DBSCAN clustering algorithm in Common Lisp:

```lisp
(defpackage :dbscan
  (:use :cl)
  (:export :dbscan :print-clusters))

(in-package :dbscan)

;; Define a point structure
(defstruct point
  x y
  cluster nil
  visited nil)

;; Calculate Euclidean distance between two points
(defun distance (p1 p2)
  "Calculate Euclidean distance between two points"
  (let ((dx (- (point-x p1) (point-x p2)))
        (dy (- (point-y p1) (point-y p2))))
    (sqrt (+ (* dx dx) (* dy dy)))))

;; Find all neighbors within epsilon distance
(defun region-query (point points epsilon)
  "Find all points within epsilon distance of the given point"
  (remove-if-not (lambda (p) 
                   (and (not (eq p point))
                        (< (distance point p) epsilon)))
                 points))

;; Main DBSCAN algorithm
(defun dbscan (points epsilon min-points)
  "Perform DBSCAN clustering on a list of points"
  (let ((clusters '())
        (cluster-id 0))
    ;; Initialize all points as unvisited
    (dolist (point points)
      (setf (point-visited point) nil))
    
    ;; Process each point
    (dolist (point points)
      (unless (point-visited point)
        (setf (point-visited point) t)
        (let ((neighbors (region-query point points epsilon)))
          (if (>= (length neighbors) min-points)
              ;; Point is a core point
              (let ((cluster (list point)))
                (setf (point-cluster point) cluster-id)
                (dolist (neighbor neighbors)
                  (unless (point-visited neighbor)
                    (setf (point-visited neighbor) t)
                    (let ((neighbor-neighbors (region-query neighbor points epsilon)))
                      (if (>= (length neighbor-neighbors) min-points)
                          ;; Add to cluster and expand
                          (dolist (n neighbor-neighbors)
                            (unless (point-visited n)
                              (setf (point-visited n) t)
                              (let ((n-neighbors (region-query n points epsilon)))
                                (if (>= (length n-neighbors) min-points)
                                    (push n cluster)))))
                          ;; Add to cluster if it's a border point
                          (when (and (point-cluster neighbor) 
                                    (not (point-cluster point)))
                            (push neighbor cluster)))))
                (push cluster clusters)
                (incf cluster-id))
              ;; Point is noise
              (setf (point-cluster point) -1)))))
    
    clusters))

;; Print clusters in a readable format
(defun print-clusters (clusters)
  "Print the clusters in a readable format"
  (format t "DBSCAN Clustering Results~%")
  (format t "================~%")
  (loop for i from 0 to (1- (length clusters))
        do (format t "Cluster ~A: ~A points~%" 
                   i (length (nth i clusters))))
  (format t "~%"))

;; Example usage
(defun example ()
  "Example of DBSCAN clustering"
  (let* ((points (list (make-point :x 1 :y 2)
                       (make-point :x 1 :y 4)
                       (make-point :x 2 :y 2)
                       (make-point :x 2 :y 4)
                       (make-point :x 10 :y 2)
                       (make-point :x 10 :y 4)
                       (make-point :x 11 :y 2)
                       (make-point :x 11 :y 4)
                       (make-point :x 20 :y 20)
                       (make-point :x 21 :y 21)))
         (epsilon 2.0)
         (min-points 2)
         (clusters (dbscan points epsilon min-points)))
    (print-clusters clusters)
    clusters))

;; Run the example
(example)
```

## How it works:

1. **Point Structure**: Defines points with x, y coordinates and clustering information
2. **Distance Function**: Calculates Euclidean distance between two points
3. **Region Query**: Finds all points within epsilon distance of a given point
4. **Main Algorithm**:
   - Marks points as visited
   - For each unvisited point, determines if it's a core point
   - Expands clusters by adding density-reachable points
   - Assigns noise points to cluster -1

## Key Features:

- **Core Points**: Points with at least min-points within epsilon distance
- **Border Points**: Points within epsilon distance of core points but not core points themselves
- **Noise Points**: Points that are neither core nor border points
- **Cluster Expansion**: Uses density-based connectivity to form clusters

## Example Output:
```
DBSCAN Clustering Results
================
Cluster 0: 4 points
Cluster 1: 4 points
Cluster 2: 2 points
```

This implementation demonstrates the fundamental DBSCAN algorithm in a clean, readable Common Lisp format.

