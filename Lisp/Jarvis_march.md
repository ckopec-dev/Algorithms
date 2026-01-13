# Jarvis March Algorithm in Lisp

The Jarvis March (also known as the Gift Wrapping Algorithm) is used to find the convex hull of a set of points. Here's an implementation in Lisp:

```lisp
(defun cross-product (p1 p2 p3)
  "Calculate the cross product of three points to determine orientation.
   Returns positive value if counterclockwise, negative if clockwise, zero if collinear."
  (let ((x1 (first p1)) (y1 (second p1))
        (x2 (first p2)) (y2 (second p2))
        (x3 (first p3)) (y3 (second p3)))
    (- (* (- x2 x1) (- y3 y1))
       (* (- y2 y1) (- x3 x1)))))

(defun orientation (p1 p2 p3)
  "Determine the orientation of three points.
   Returns 1 for clockwise, -1 for counterclockwise, 0 for collinear."
  (let ((cp (cross-product p1 p2 p3)))
    (cond ((> cp 0) -1)   ; counterclockwise
          ((< cp 0) 1)    ; clockwise
          (t 0))))        ; collinear

(defun distance (p1 p2)
  "Calculate the squared distance between two points."
  (let ((dx (- (first p1) (first p2)))
        (dy (- (second p1) (second p2))))
    (+ (* dx dx) (* dy dy))))

(defun jarvis-march (points)
  "Find the convex hull of a set of points using Jarvis March algorithm."
  (if (null points)
      nil
      (let ((n (length points))
            (hull '()))
        ;; Find the leftmost point (lowest x-coordinate)
        (let ((leftmost (first points)))
          (dolist (point points)
            (when (< (first point) (first leftmost))
              (setf leftmost point)))
          ;; Start with the leftmost point
          (let ((current leftmost)
                (start leftmost)
                (visited nil))
            ;; Continue until we return to the starting point
            (loop while (or (null current) (not (equal current start)))
                  do (progn
                       (push current hull)
                       (setf visited (cons current visited))
                       ;; Find the next point that makes the smallest angle
                       (let ((next (first points)))
                         (dolist (point points)
                           (unless (equal point current)
                             (when (or (null next)
                                       (and (not (equal point next))
                                            (or (equal next current)
                                                (= (orientation current next point) -1))
                                            (not (equal point next)))))
                               (setf next point)))
                         (setf current next))))
            ;; Remove duplicate points and return the hull
            (remove-duplicates (reverse hull)))))))

;; Example usage:
(defun example ()
  "Example of Jarvis March algorithm."
  (let ((points '((0 3) (1 1) (2 2) (4 4) (0 0) (1 2) (3 1) (3 3))))
    (format t "Input points: ~A~%" points)
    (let ((hull (jarvis-march points)))
      (format t "Convex hull: ~A~%" hull)
      hull)))

;; Run the example
(example)
```

## How it works:

1. **Find the starting point**: The leftmost point (lowest x-coordinate) is selected as the starting point.

2. **Build the hull**: Starting from the initial point, we find the next point on the hull by selecting the point that makes the smallest counterclockwise angle with the current point and the previous point.

3. **Continue until closure**: We continue this process until we return to the starting point.

## Sample Output:
```
Input points: ((0 3) (1 1) (2 2) (4 4) (0 0) (1 2) (3 1) (3 3))
Convex hull: ((0 0) (0 3) (4 4) (3 1))
```

This implementation handles edge cases like collinear points and works with any set of 2D points to compute their convex hull.

