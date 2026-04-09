# K-Medoids Clustering in Lisp

Here's an implementation of the K-Medoids clustering algorithm in Common Lisp:

```lisp
(defpackage :k-medoids
  (:use :cl)
  (:export :k-medoids :distance))
(in-package :k-medoids)

;; Define a point structure
(defstruct point
  (x 0.0 :type double-float)
  (y 0.0 :type double-float)
  (cluster -1 :type integer))

;; Euclidean distance function
(defun distance (p1 p2)
  "Calculate Euclidean distance between two points"
  (let ((dx (- (point-x p1) (point-x p2)))
        (dy (- (point-y p1) (point-y p2))))
    (sqrt (+ (* dx dx) (* dy dy)))))

;; Find the medoid (point with minimum sum of distances)
(defun find-medoid (points cluster-points)
  "Find the point in cluster-points that minimizes sum of distances to all other points"
  (let ((min-sum most-positive-double-float)
        (best-point nil))
    (dolist (candidate cluster-points)
      (let ((sum 0.0))
        (dolist (other cluster-points)
          (when (not (eq candidate other))
            (incf sum (distance candidate other))))
        (when (< sum min-sum)
          (setf min-sum sum
                best-point candidate))))
    best-point))

;; K-Medoids clustering algorithm
(defun k-medoids (points k &optional (max-iterations 100))
  "Perform K-Medoids clustering on a list of points"
  (let ((n (length points))
        (clusters (make-array k :element-type 'list))
        (medoids (make-array k :element-type 'point))
        (old-medoids (make-array k :element-type 'point))
        (iteration 0))
    
    ;; Initialize medoids randomly
    (loop for i from 0 below k do
      (let ((random-point (nth (random n) points)))
        (setf (aref medoids i) random-point)))
    
    ;; Main clustering loop
    (loop while (and (< iteration max-iterations)
                     (not (equal medoids old-medoids))) do
      (setf iteration (1+ iteration))
      
      ;; Copy current medoids for comparison
      (loop for i from 0 below k do
        (setf (aref old-medoids i) (aref medoids i)))
      
      ;; Assign points to clusters
      (loop for point in points do
        (let ((min-distance most-positive-double-float)
              (closest-medoid nil))
          (loop for i from 0 below k do
            (let ((dist (distance point (aref medoids i))))
              (when (< dist min-distance)
                (setf min-distance dist
                      closest-medoid (aref medoids i)))))
          ;; Assign point to the cluster of closest medoid
          (setf (point-cluster point) (position closest-medoid medoids))))
      
      ;; Update medoids for each cluster
      (loop for i from 0 below k do
        (let ((cluster-points (remove-if-not (lambda (p) (= (point-cluster p) i)) points)))
          (when cluster-points
            (setf (aref medoids i) (find-medoid points cluster-points)))))
      
      ;; Print progress
      (format t "Iteration ~A: ~A~%" iteration (mapcar (lambda (m) (list (point-x m) (point-y m))) (coerce medoids 'list))))
    
    ;; Return final clusters and medoids
    (values (coerce medoids 'list) 
            (loop for i from 0 below k collect
              (remove-if-not (lambda (p) (= (point-cluster p) i)) points)))))

;; Example usage
(defun example ()
  "Example of K-Medoids clustering"
  (let* ((points (list (make-point :x 1.0 :y 2.0)
                       (make-point :x 1.5 :y 1.8)
                       (make-point :x 5.0 :y 8.0)
                       (make-point :x 8.0 :y 8.0)
                       (make-point :x 1.0 :y 0.6)
                       (make-point :x 9.0 :y 11.0)
                       (make-point :x 8.0 :y 2.0)
                       (make-point :x 10.0 :y 2.0)
                       (make-point :x 9.0 :y 3.0)
                       (make-point :x 2.0 :y 1.0)))
         (result (k-medoids points 3)))
    (format t "Final medoids: ~A~%" (first result))
    (format t "Clusters: ~A~%" (second result))))

;; Run the example
(example)
```

## How it works:

1. **Point Structure**: Defines a simple 2D point with x, y coordinates and cluster assignment
2. **Distance Function**: Calculates Euclidean distance between two points
3. **Find Medoid**: For each cluster, finds the point that minimizes the sum of distances to all other points in that cluster
4. **K-Medoids Algorithm**: 
   - Initialize k random medoids
   - Assign points to the nearest medoid
   - Update medoids by finding the point with minimum sum of distances in each cluster
   - Repeat until convergence or max iterations

## Key Features:

- **Robust**: Unlike K-Means, K-Medoids uses actual data points as medoids
- **Handles outliers better**: More resistant to noise and outliers
- **Flexible**: Can work with any distance metric
- **Converges**: Guarantees convergence to a local optimum

The algorithm is particularly useful when you want to ensure that cluster centers are actual data points rather than computed averages.

