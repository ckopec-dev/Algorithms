# Iterative Closest Point (ICP) Algorithm in Lisp

Here's an implementation of the ICP algorithm in Common Lisp:

```lisp
(defpackage :icp-package
  (:use :cl)
  (:export :icp :point :transform-points :find-closest-points))
(in-package :icp-package)

;; Define a point structure
(defstruct point
  x y z)

;; Calculate Euclidean distance between two points
(defun distance (p1 p2)
  "Calculate Euclidean distance between two points"
  (let ((dx (- (point-x p1) (point-x p2)))
        (dy (- (point-y p1) (point-y p2)))
        (dz (- (point-z p1) (point-z p2))))
    (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))

;; Find the closest point in target set for each point in source set
(defun find-closest-points (source-points target-points)
  "Find closest point in target for each point in source"
  (mapcar (lambda (source-point)
             (let ((closest-point nil)
                   (min-distance most-positive-double-float))
               (dolist (target-point target-points)
                 (let ((dist (distance source-point target-point)))
                   (when (< dist min-distance)
                     (setf min-distance dist
                           closest-point target-point))))
               closest-point))
           source-points))

;; Calculate centroid of a list of points
(defun centroid (points)
  "Calculate centroid (mean) of a list of points"
  (if (null points)
      (make-point :x 0 :y 0 :z 0)
      (let ((sum-x 0)
            (sum-y 0)
            (sum-z 0)
            (count (length points)))
        (dolist (point points)
          (incf sum-x (point-x point))
          (incf sum-y (point-y point))
          (incf sum-z (point-z point)))
        (make-point :x (/ sum-x count)
                    :y (/ sum-y count)
                    :z (/ sum-z count)))))

;; Calculate transformation matrix from source to target centroids
(defun calculate-transformation (source-centroid target-centroid)
  "Calculate translation vector from source to target centroid"
  (make-point :x (- (point-x target-centroid) (point-x source-centroid))
              :y (- (point-y target-centroid) (point-y source-centroid))
              :z (- (point-z target-centroid) (point-z source-centroid))))

;; Transform points using translation
(defun transform-points (points translation)
  "Apply translation to all points"
  (mapcar (lambda (point)
             (make-point :x (+ (point-x point) (point-x translation))
                         :y (+ (point-y point) (point-y translation))
                         :z (+ (point-z point) (point-z translation))))
           points))

;; Main ICP algorithm function
(defun icp (source-points target-points &key (max-iterations 50) (threshold 0.001))
  "Iterative Closest Point algorithm implementation"
  (let ((current-source source-points)
        (total-transformation (make-point :x 0 :y 0 :z 0))
        (iteration 0)
        (error 1.0))
    (loop while (and (< iteration max-iterations) (> error threshold)) do
          (let* ((closest-points (find-closest-points current-source target-points))
                 (source-centroid (centroid current-source))
                 (target-centroid (centroid closest-points))
                 (transformation (calculate-transformation source-centroid target-centroid)))
            ;; Update transformation
            (setf total-transformation 
                  (make-point :x (+ (point-x total-transformation) (point-x transformation))
                              :y (+ (point-y total-transformation) (point-y transformation))
                              :z (+ (point-z total-transformation) (point-z transformation))))
            ;; Apply transformation to current source
            (setf current-source (transform-points current-source transformation))
            ;; Calculate error (average distance between corresponding points)
            (let ((total-distance 0)
                  (count 0))
              (dolist (source current-source)
                (let ((closest (find-closest-points (list source) target-points)))
                  (when closest
                    (incf total-distance (distance source (first closest)))
                    (incf count))))
              (setf error (if (> count 0) 
                              (/ total-distance count) 
                              0.0)))
            (incf iteration)))
    ;; Return final transformed source points and total transformation
    (values (transform-points source-points total-transformation)
            total-transformation
            iteration)))

;; Example usage
(defun example-icp ()
  "Example demonstrating ICP algorithm"
  (let* (;; Create some sample source points (a square)
         (source-points (list (make-point :x 0 :y 0 :z 0)
                              (make-point :x 1 :y 0 :z 0)
                              (make-point :x 1 :y 1 :z 0)
                              (make-point :x 0 :y 1 :z 0)))
         ;; Create target points (a slightly rotated and translated square)
         (target-points (list (make-point :x 0.1 :y 0.2 :z 0)
                              (make-point :x 1.1 :y 0.2 :z 0)
                              (make-point :x 1.1 :y 1.2 :z 0)
                              (make-point :x 0.1 :y 1.2 :z 0))))
    (format t "Source points:~%")
    (dolist (p source-points)
      (format t "  (~A, ~A, ~A)~%" (point-x p) (point-y p) (point-z p)))
    (format t "Target points:~%")
    (dolist (p target-points)
      (format t "  (~A, ~A, ~A)~%" (point-x p) (point-y p) (point-z p)))
    (multiple-value-bind (final-source transformation iterations)
        (icp source-points target-points :max-iterations 100 :threshold 0.0001)
      (format t "~%ICP Results:~%")
      (format t "Final transformation: (~A, ~A, ~A)~%" 
              (point-x transformation) (point-y transformation) (point-z transformation))
      (format t "Iterations: ~A~%" iterations)
      (format t "Final source points:~%")
      (dolist (p final-source)
        (format t "  (~A, ~A, ~A)~%" (point-x p) (point-y p) (point-z p))))))

;; Run the example
(example-icp)
```

## Key Features of this ICP Implementation:

1. **Point Structure**: Defines a 3D point with x, y, z coordinates
2. **Distance Calculation**: Computes Euclidean distance between points
3. **Closest Point Finding**: For each source point, finds the closest target point
4. **Centroid Calculation**: Computes mean position of a set of points
5. **Transformation**: Calculates translation needed to align centroids
6. **Main Loop**: Iteratively refines alignment until convergence

## Algorithm Steps:

1. **Initialize**: Start with source and target point sets
2. **Find Correspondences**: For each source point, find closest target point
3. **Calculate Transformation**: Compute translation to align centroids
4. **Apply Transformation**: Transform source points using calculated transformation
5. **Check Convergence**: Continue until error threshold is met or max iterations reached

## Usage:

The example demonstrates how to use the ICP algorithm to align two sets of 3D points, showing the transformation needed to align a source square with a slightly offset target square.

This implementation provides a foundation that can be extended with more sophisticated features like rotation matrices, outlier rejection, or different distance metrics.

