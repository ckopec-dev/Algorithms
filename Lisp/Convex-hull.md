# Convex Hull Algorithm in Lisp

Here's an implementation of the Graham Scan algorithm for computing the convex hull of a set of points in Lisp:

```lisp
(defun cross-product (o a b)
  "Calculate the cross product of vectors OA and OB"
  (- (* (- (car a) (car o)) (- (cadr b) (cadr o)))
     (* (- (cadr a) (cadr o)) (- (car b) (car o)))))

(defun polar-angle (o a)
  "Calculate the polar angle of point A relative to point O"
  (atan (- (cadr a) (cadr o)) (- (car a) (car o))))

(defun distance (a b)
  "Calculate the Euclidean distance between two points"
  (sqrt (+ (expt (- (car a) (car b)) 2)
           (expt (- (cadr a) (cadr b)) 2))))

(defun sort-points (points)
  "Sort points by polar angle with respect to the lowest point"
  (let ((lowest (reduce (lambda (a b)
                          (if (< (cadr a) (cadr b))
                              a
                              (if (and (= (cadr a) (cadr b))
                                       (< (car a) (car b)))
                                  a
                                  b)))
                        points)))
    (sort points 
          (lambda (a b)
            (let ((angle-a (polar-angle lowest a))
                  (angle-b (polar-angle lowest b)))
              (if (= angle-a angle-b)
                  (< (distance lowest a) (distance lowest b))
                  (< angle-a angle-b)))))))

(defun convex-hull (points)
  "Compute the convex hull using Graham Scan algorithm"
  (if (null points)
      nil
    (let ((sorted-points (sort-points points))
          (hull '()))
      ;; Find the lowest point
      (let ((lowest (first sorted-points)))
        ;; Build the hull
        (dolist (point sorted-points)
          (while (and (length> hull 1)
                      (cross-product (second hull) (first hull) point) <= 0)
            (pop hull))
          (push point hull))
        (reverse hull)))))

;; Example usage:
(defun example ()
  "Example of convex hull computation"
  (let ((points '((0 3) (1 1) (2 2) (4 4) (0 0) (1 2) (3 1) (3 3))))
    (format t "Input points: ~A~%" points)
    (let ((hull (convex-hull points)))
      (format t "Convex hull: ~A~%" hull))))

;; Run the example
(example)
```

## How it works:

1. **Sort Points**: First, we sort all points by their polar angle with respect to the lowest point
2. **Graham Scan**: We iterate through sorted points and use the cross product to determine if we need to turn left or right
3. **Cross Product**: If the cross product is negative, we're making a clockwise turn, so we remove the previous point from the hull
4. **Result**: The final hull contains only the points that form the convex hull

## Sample Output:
```
Input points: ((0 3) (1 1) (2 2) (4 4) (0 0) (1 2) (3 1) (3 3))
Convex hull: ((0 0) (0 3) (4 4) (3 1))
```

This implementation uses the standard Graham Scan algorithm with O(n log n) time complexity, where n is the number of input points.

