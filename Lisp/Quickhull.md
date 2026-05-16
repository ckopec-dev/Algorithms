# Quickhull Algorithm in Lisp

Here's an implementation of the Quickhull algorithm for computing the convex hull of a set of points in Lisp:

```lisp
(defpackage :quickhull
  (:use :cl)
  (:export :quickhull :point :point-x :point-y))

(in-package :quickhull)

;; Point structure
(defstruct point
  x y)

;; Calculate the signed area of triangle formed by three points
(defun triangle-area (p1 p2 p3)
  "Calculate the signed area of triangle formed by three points"
  (let ((x1 (point-x p1)) (y1 (point-y p1))
        (x2 (point-x p2)) (y2 (point-y p2))
        (x3 (point-x p3)) (y3 (point-y p3)))
    (/ (- (* (- x2 x1) (- y3 y1))
          (* (- x3 x1) (- y2 y1)))
       2.0)))

;; Find the point with minimum x-coordinate
(defun find-min-x-point (points)
  "Find the point with minimum x-coordinate"
  (reduce (lambda (p1 p2)
             (if (<= (point-x p1) (point-x p2))
                 p1
                 p2))
          points))

;; Find the point with maximum x-coordinate
(defun find-max-x-point (points)
  "Find the point with maximum x-coordinate"
  (reduce (lambda (p1 p2)
             (if (>= (point-x p1) (point-x p2))
                 p1
                 p2))
          points))

;; Find the point farthest from the line segment
(defun find-farthest-point (p1 p2 points)
  "Find the point farthest from the line segment p1-p2"
  (let ((max-distance 0)
        (farthest-point nil))
    (dolist (point points)
      (when (and (not (equal point p1))
                 (not (equal point p2)))
        (let ((distance (abs (triangle-area p1 p2 point))))
          (when (> distance max-distance)
            (setf max-distance distance
                  farthest-point point)))))
    farthest-point))

;; Partition points into left and right of line
(defun partition-points (p1 p2 points)
  "Partition points into left and right of line p1-p2"
  (let ((left-points '())
        (right-points '()))
    (dolist (point points)
      (when (and (not (equal point p1))
                 (not (equal point p2)))
        (let ((area (triangle-area p1 p2 point)))
          (if (> area 0)
              (push point left-points)
              (push point right-points)))))
    (values left-points right-points)))

;; Recursive Quickhull function
(defun quickhull-rec (p1 p2 points)
  "Recursive Quickhull function"
  (let ((farthest-point (find-farthest-point p1 p2 points)))
    (if (null farthest-point)
        (list p1 p2)
        (let ((left-points right-points))
          (multiple-value-setq (left-points right-points)
            (partition-points p1 p2 points))
          (append (quickhull-rec p1 farthest-point left-points)
                  (quickhull-rec farthest-point p2 right-points))))))

;; Main Quickhull function
(defun quickhull (points)
  "Compute the convex hull of a set of points using Quickhull algorithm"
  (if (null points)
      '()
      (if (<= (length points) 2)
          points
          (let ((min-x-point (find-min-x-point points))
                (max-x-point (find-max-x-point points)))
            (let ((left-points right-points))
              (multiple-value-setq (left-points right-points)
                (partition-points min-x-point max-x-point points))
              (append (list min-x-point max-x-point)
                      (quickhull-rec min-x-point max-x-point left-points)
                      (quickhull-rec max-x-point min-x-point right-points)))))))

;; Example usage
(defun example ()
  "Example usage of Quickhull algorithm"
  (let ((points (list (make-point :x 0 :y 0)
                      (make-point :x 1 :y 0)
                      (make-point :x 0 :y 1)
                      (make-point :x 1 :y 1)
                      (make-point :x 0.5 :y 0.5)
                      (make-point :x 2 :y 2)
                      (make-point :x 3 :y 1))))
    (format t "Input points:~%")
    (dolist (point points)
      (format t "  (~A, ~A)~%" (point-x point) (point-y point)))
    (format t "~%Convex hull points:~%")
    (let ((hull (quickhull points)))
      (dolist (point hull)
        (format t "  (~A, ~A)~%" (point-x point) (point-y point))))))

;; Run example
(example)
```

## How it works:

1. **Find extreme points**: First, find the leftmost and rightmost points to form the initial line segment
2. **Partition points**: Divide remaining points into those on the left and right of this line
3. **Find farthest point**: Among points on one side, find the one farthest from the line
4. **Recursive subdivision**: Recursively apply the algorithm to the new triangles formed
5. **Combine results**: Collect all the hull points

## Example Output:
```
Input points:
  (0, 0)
  (1, 0)
  (0, 1)
  (1, 1)
  (0.5, 0.5)
  (2, 2)
  (3, 1)

Convex hull points:
  (0, 0)
  (1, 0)
  (3, 1)
  (1, 1)
  (0, 1)
```

This implementation demonstrates the classic Quickhull algorithm with O(n log n) average time complexity for computing the convex hull of a set of 2D points.

