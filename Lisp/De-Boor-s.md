# De Boor's Algorithm in Lisp

De Boor's algorithm is used for evaluating B-spline curves. Here's an implementation in Lisp:

```lisp
(defun de-boor (degree knots points u)
  "Evaluate B-spline curve using De Boor's algorithm.
   degree: spline degree
   knots: knot vector
   points: control points
   u: parameter value"
  (let* ((n (length points))
         (m (+ n degree 1))
         (p degree)
         (i (find-span n p u knots)))
    (de-boor-recursion p i u knots points)))

(defun find-span (n p u knots)
  "Find the span index for parameter u"
  (cond
    ((= u (aref knots n)) (1- n))
    (t (let ((low (1+ p))
             (high (1- n))
             (mid (floor (+ low high) 2)))
         (loop while (or (< u (aref knots mid)) (> u (aref knots (1+ mid))))
               do (if (< u (aref knots mid))
                      (setf high mid)
                      (setf low mid))
               (setf mid (floor (+ low high) 2)))
         mid))))

(defun de-boor-recursion (p i u knots points)
  "Recursive implementation of De Boor's algorithm"
  (let ((d (make-array (1+ p) :element-type 'vector)))
    ;; Initialize the first row
    (loop for j from 0 to p
          do (setf (aref d j) (aref points (- i p j))))
    
    ;; Perform the recursion
    (loop for j from 1 to p
          do (loop for k from 0 to (- p j)
                   do (let ((alpha (/ (- u (aref knots (+ i k)))
                                      (- (aref knots (+ i k p 1)) (aref knots (+ i k)))))
                        (setf (aref d k)
                              (vector+ (vector-scale (aref d k) (- 1 alpha))
                                       (vector-scale (aref d (1+ k)) alpha))))))
    
    (aref d 0)))

(defun vector+ (v1 v2)
  "Add two vectors"
  (let ((result (make-array (length v1) :element-type 'number)))
    (loop for i from 0 to (1- (length v1))
          do (setf (aref result i) (+ (aref v1 i) (aref v2 i))))
    result))

(defun vector-scale (v s)
  "Scale a vector by a scalar"
  (let ((result (make-array (length v) :element-type 'number)))
    (loop for i from 0 to (1- (length v))
          do (setf (aref result i) (* (aref v i) s)))
    result))

;; Example usage:
;; Define a simple quadratic B-spline (degree 2)
;; Control points: [(0,0), (1,2), (2,0)]
;; Knot vector: [0, 0, 0, 1, 2, 2, 2]
(defparameter *control-points* 
  (vector (vector 0 0) (vector 1 2) (vector 2 0)))

(defparameter *knots* 
  (vector 0 0 0 1 2 2 2))

;; Evaluate at u = 0.5
(de-boor 2 *knots* *control-points* 0.5)
```

## Example Output

For the above example with:
- Degree = 2
- Knot vector = [0, 0, 0, 1, 2, 2, 2]  
- Control points = [(0,0), (1,2), (2,0)]
- Parameter u = 0.5

The algorithm will return the point on the B-spline curve at parameter value 0.5.

## Key Features

1. **find-span**: Locates the correct span in the knot vector
2. **de-boor-recursion**: Implements the core recursive algorithm
3. **Vector operations**: Includes helper functions for vector addition and scaling
4. **Proper handling**: Manages boundary conditions and edge cases

This implementation demonstrates how De Boor's algorithm efficiently evaluates B-spline curves by recursively computing the weighted averages of control points.

