# Bezier Curve Generation in Lisp

Here's an implementation of a Bezier curve generation algorithm in Common Lisp:

```lisp
(defun bezier-point (points t)
  "Calculate a point on a Bezier curve given control points and parameter t"
  (let ((n (1- (length points))))
    (if (= n 0)
        (first points)
        (let ((p0 (bezier-point (butlast points) t))
              (p1 (bezier-point (rest points) t)))
          (mapcar #'(lambda (a b) (+ (* (- 1 t) a) (* t b)))
                  p0 p1)))))

(defun bezier-curve (control-points num-points)
  "Generate a sequence of points on a Bezier curve"
  (let ((points '()))
    (loop for i from 0 to (1- num-points)
          for t = (/ i (1- num-points))
          do (push (bezier-point control-points t) points))
    (nreverse points)))

(defun de-casteljau (points t)
  "Alternative implementation using De Casteljau's algorithm"
  (if (null (rest points))
      (first points)
      (let ((left (de-casteljau (butlast points) t))
            (right (de-casteljau (rest points) t)))
        (mapcar #'(lambda (a b) (+ (* (- 1 t) a) (* t b)))
                left right)))))

;; Example usage:
(defun example-bezier ()
  "Example of generating a quadratic Bezier curve"
  (let* ((control-points '((0 0) (1 2) (2 0)))
         (curve-points (bezier-curve control-points 11)))
    (format t "Control points: ~A~%" control-points)
    (format t "Curve points: ~A~%" curve-points)
    curve-points))

;; Generate a cubic Bezier curve
(defun cubic-bezier-example ()
  "Example of generating a cubic Bezier curve"
  (let* ((control-points '((0 0) (1 1) (2 1) (3 0)))
         (curve-points (bezier-curve control-points 21)))
    (format t "Cubic Bezier curve points:~%")
    (loop for point in curve-points
          for i from 0
          do (format t "P~A: ~A~%" i point))
    curve-points))

;; Simple 2D point addition helper
(defun point-add (p1 p2)
  "Add two 2D points"
  (mapcar #'+ p1 p2))

(defun point-multiply (point scalar)
  "Multiply a 2D point by a scalar"
  (mapcar #'(lambda (x) (* x scalar)) point))
```

## Key Features of this Implementation:

1. **Recursive Bezier Point Calculation**: The `bezier-point` function recursively calculates points on the curve using the Bernstein polynomial approach.

2. **De Casteljau Algorithm**: The `de-casteljau` function provides an alternative implementation using the geometric approach.

3. **Flexible Control Points**: Supports curves of any degree by varying the number of control points.

4. **Parameter T**: Uses parameter `t` ranging from 0 to 1 to generate points along the curve.

## Usage Example:

```lisp
;; Generate a quadratic Bezier curve with 11 points
(setq my-curve (bezier-curve '((0 0) (1 2) (2 0)) 11))

;; Generate a cubic Bezier curve
(setq cubic-curve (bezier-curve '((0 0) (1 1) (2 1) (3 0)) 21))
```

This implementation demonstrates the mathematical foundation of Bezier curves while maintaining clean, readable Lisp code structure.

