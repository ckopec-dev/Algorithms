# Z-Order Curve Generation Algorithm in Lisp

Here's an implementation of the Z-order curve (also known as Morton order) generation algorithm in Common Lisp:

```lisp
(defun z-order-curve (n)
  "Generate Z-order curve for a 2D grid of size 2^n x 2^n"
  (let ((size (expt 2 n)))
    (loop for i from 0 below size
          collect (loop for j from 0 below size
                       collect (z-value i j)))))

(defun z-value (x y)
  "Calculate Z-order value for coordinates (x, y)"
  (let ((result 0)
        (bit-position 0))
    (loop while (or (> x 0) (> y 0))
          do (progn
               (setf result (logior result 
                                   (ash (logand x 1) bit-position)
                                   (ash (logand y 1) (1+ bit-position))))
               (setf x (ash x -1))
               (setf y (ash y -1))
               (setf bit-position (+ bit-position 2))))
    result))

(defun z-order-curve-optimized (n)
  "Optimized version using bit manipulation"
  (let ((size (expt 2 n)))
    (loop for i from 0 below size
          collect (loop for j from 0 below size
                       collect (z-value-optimized i j)))))

(defun z-value-optimized (x y)
  "Optimized Z-order value calculation using bit interleaving"
  (let ((result 0))
    (loop for i from 0 below (integer-length (max x y))
          do (progn
               (setf result (logior result 
                                   (ash (logand x 1) (* i 2))
                                   (ash (logand y 1) (1+ (* i 2)))))
               (setf x (ash x -1))
               (setf y (ash y -1))))
    result))

(defun print-z-curve (n)
  "Print Z-order curve in a readable format"
  (let ((curve (z-order-curve n)))
    (format t "Z-order curve for 2^~A x 2^~A grid:~%" n n)
    (loop for row in curve
          for i from 0
          do (progn
               (format t "Row ~A: " i)
               (loop for val in row
                     do (format t "~4A " val))
               (format t "~%")))))

;; Example usage:
;; (print-z-curve 2)
;; This will generate a 4x4 Z-order curve

(defun z-curve-2d-to-1d (x y)
  "Convert 2D coordinates to 1D Z-order index"
  (z-value x y))

(defun z-curve-1d-to-2d (index n)
  "Convert 1D Z-order index back to 2D coordinates"
  (let ((size (expt 2 n))
        (x 0)
        (y 0)
        (bit-position 0))
    (loop while (< bit-position n)
          do (progn
               (setf x (logior x (ash (logand index 1) bit-position)))
               (setf y (logior y (ash (logand index 2) (1- bit-position))))
               (setf index (ash index -2))
               (setf bit-position (1+ bit-position))))
    (values x y)))

;; Test the implementation
(format t "Testing Z-order curve generation:~%")
(print-z-curve 2)
(format t "~%Testing coordinate conversion:~%")
(format t "Z-value of (0,0): ~A~%" (z-curve-2d-to-1d 0 0))
(format t "Z-value of (1,0): ~A~%" (z-curve-2d-to-1d 1 0))
(format t "Z-value of (0,1): ~A~%" (z-curve-2d-to-1d 0 1))
(format t "Z-value of (1,1): ~A~%" (z-curve-2d-to-1d 1 1))
```

## Key Features of this Implementation:

1. **`z-order-curve`**: Generates the complete Z-order curve for a 2^n × 2^n grid
2. **`z-value`**: Calculates the Z-order value for given (x,y) coordinates
3. **`z-value-optimized`**: More efficient bit manipulation version
4. **`print-z-curve`**: Utility function to display the results in a readable format
5. **Coordinate conversion functions**: Convert between 2D coordinates and 1D Z-order indices

## Example Output:
For a 2×2 grid, the Z-order curve would be:
```
Row 0:    0    1 
Row 1:    2    3 
```

The algorithm works by interleaving the bits of the x and y coordinates to create a single Z-order index, which preserves spatial locality in the 1D representation.

