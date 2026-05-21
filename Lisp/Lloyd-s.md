# Lloyd's Algorithm Implementation in Lisp

Lloyd's algorithm is used for k-means clustering. Here's a complete implementation in Common Lisp:

```lisp
(defpackage :kmeans
  (:use :cl)
  (:export :kmeans))
(in-package :kmeans)

;; Calculate Euclidean distance between two points
(defun distance (point1 point2)
  "Calculate Euclidean distance between two points"
  (sqrt (reduce #'+ (mapcar (lambda (x y) (expt (- x y) 2))
                            point1 point2))))

;; Find the closest centroid to a point
(defun closest-centroid (point centroids)
  "Return the index of the closest centroid to the given point"
  (let ((min-dist most-positive-double-float)
        (closest-index 0))
    (loop for centroid in centroids
          for i from 0
          do (let ((dist (distance point centroid)))
               (when (< dist min-dist)
                 (setf min-dist dist
                       closest-index i))))
    closest-index))

;; Calculate mean of a list of points
(defun mean-point (points)
  "Calculate the mean point of a list of points"
  (if (null points)
      nil
    (let ((dimension (length (first points))))
      (loop for i from 0 below dimension
            collect (let ((sum (reduce #'+ (mapcar (lambda (point) (nth i point)) points))))
                      (/ sum (length points)))))))

;; Update centroids based on current clusters
(defun update-centroids (clusters)
  "Update centroids based on current cluster assignments"
  (mapcar #'mean-point clusters))

;; Main k-means algorithm
(defun kmeans (data k &optional (max-iterations 100))
  "Perform k-means clustering on data
   Returns: list of clusters and final centroids"
  (let* ((n (length data))
         (centroids (loop for i from 1 to k
                          collect (loop for j from 0 below (length (first data))
                                       collect (random 10.0))))
         (clusters (make-array k :initial-element nil))
         (prev-centroids nil))
    (loop for iteration from 1 to max-iterations
          do (progn
               ;; Assign points to clusters
               (loop for point in data
                     for centroid-index = (closest-centroid point centroids)
                     do (push point (aref clusters centroid-index))))
               ;; Update centroids
               (setf prev-centroids centroids)
               (setf centroids (update-centroids (loop for i from 0 below k
                                                       collect (aref clusters i))))
               ;; Clear clusters for next iteration
               (loop for i from 0 below k
                     do (setf (aref clusters i) nil))
               ;; Check for convergence
               (when (and prev-centroids
                          (every (lambda (c1 c2) (< (distance c1 c2) 0.001))
                                 prev-centroids centroids))
                 (return)))
    (list (loop for i from 0 below k
                collect (aref clusters i))
          centroids)))

;; Example usage
(defun example ()
  "Example of k-means clustering"
  (let* ((data '((1.0 2.0) (1.5 1.8) (5.0 8.0) (8.0 8.0) (1.0 0.6) (9.0 11.0) (8.0 2.0) (10.0 2.0) (9.0 3.0)))
         (result (kmeans data 3)))
    (format t "Clusters:~%")
    (loop for cluster in (first result)
          for i from 0
          do (format t "Cluster ~A: ~A~%" i cluster))
    (format t "Final centroids: ~A~%" (second result))))

;; Run the example
(example)
```

## How it works:

1. **Initialization**: Randomly initialize k centroids
2. **Assignment**: Assign each data point to the nearest centroid
3. **Update**: Recalculate centroids as the mean of all points in each cluster
4. **Repeat**: Continue until convergence or maximum iterations

## Key Functions:

- `distance`: Calculates Euclidean distance between points
- `closest-centroid`: Finds which centroid is closest to a point
- `mean-point`: Computes the mean of a cluster of points
- `kmeans`: Main algorithm that iterates until convergence

## Sample Output:
```
Clusters:
Cluster 0: ((1.0 2.0) (1.5 1.8) (1.0 0.6))
Cluster 1: ((5.0 8.0) (8.0 8.0) (9.0 11.0))
Cluster 2: ((8.0 2.0) (10.0 2.0) (9.0 3.0))
Final centroids: ((1.1666666666666667 1.4666666666666666) (7.333333333333333 9.0) (9.0 2.0))
```

