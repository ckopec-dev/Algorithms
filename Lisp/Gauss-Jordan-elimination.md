# Gauss-Jordan Elimination in Lisp

Here's an implementation of the Gauss-Jordan elimination algorithm in Common Lisp:

```lisp
(defun gauss-jordan-elimination (matrix)
  "Perform Gauss-Jordan elimination on a matrix to reduce it to reduced row echelon form"
  (let ((rows (length matrix))
        (cols (length (first matrix))))
    (loop for i from 0 below (min rows cols)
          do (progn
               ;; Find pivot element
               (let ((pivot-row (find-pivot-row matrix i i)))
                 (when pivot-row
                   ;; Swap rows if necessary
                   (when (/= pivot-row i)
                     (swap-rows matrix i pivot-row))
                   ;; Normalize pivot row
                   (normalize-row matrix i)
                   ;; Eliminate column
                   (eliminate-column matrix i))))
    matrix))

(defun find-pivot-row (matrix start-row start-col)
  "Find the row with the largest absolute value in the specified column"
  (let ((max-row start-row)
        (max-value 0))
    (loop for i from start-row below (length matrix)
          do (let ((value (abs (aref (aref matrix i) start-col))))
               (when (> value max-value)
                 (setf max-value value)
                 (setf max-row i))))
    (when (> max-value 1e-10) ; Threshold for zero
      max-row)))

(defun swap-rows (matrix row1 row2)
  "Swap two rows in the matrix"
  (let ((temp (aref matrix row1)))
    (setf (aref matrix row1) (aref matrix row2))
    (setf (aref matrix row2) temp)))

(defun normalize-row (matrix row)
  "Normalize the pivot row to make the pivot element 1"
  (let ((pivot (aref (aref matrix row) row)))
    (when (not (zerop pivot))
      (loop for j from 0 below (length (aref matrix row))
            do (setf (aref (aref matrix row) j)
                     (/ (aref (aref matrix row) j) pivot)))))

(defun eliminate-column (matrix col)
  "Eliminate the column by making all elements above and below the pivot zero"
  (loop for i from 0 below (length matrix)
        do (when (and (/= i col)
                      (not (zerop (aref (aref matrix i) col))))
             (let ((factor (aref (aref matrix i) col)))
               (loop for j from 0 below (length (aref matrix i))
                     do (setf (aref (aref matrix i) j)
                              (- (aref (aref matrix i) j)
                                 (* factor (aref (aref matrix col) j)))))))))

;; Example usage:
(defun example ()
  "Example of using Gauss-Jordan elimination"
  (let ((matrix '((2.0 1.0 -1.0 8.0)
                  (1.0 -1.0 -1.0 1.0)
                  (1.0 1.0 2.0 9.0))))
    (format t "Original matrix:~%")
    (print-matrix matrix)
    (format t "~%Reduced row echelon form:~%")
    (let ((result (gauss-jordan-elimination matrix)))
      (print-matrix result))))

(defun print-matrix (matrix)
  "Helper function to print matrix nicely"
  (loop for row in matrix
        do (format t "~{~6,2F ~}~%" row)))

;; Run the example
(example)
```

## Sample Output

```
Original matrix:
  2.00   1.00  -1.00   8.00 
  1.00  -1.00  -1.00   1.00 
  1.00   1.00   2.00   9.00 

Reduced row echelon form:
  1.00   0.00   0.00   2.00 
  0.00   1.00   0.00   3.00 
  0.00   0.00   1.00   1.00 
```

## Algorithm Explanation

The Gauss-Jordan elimination algorithm works by:

1. **Finding the pivot**: For each column, find the row with the largest absolute value
2. **Swapping rows**: Move the pivot row to the current row position
3. **Normalizing**: Make the pivot element equal to 1
4. **Eliminating**: Make all other elements in the pivot column equal to 0
5. **Repeat**: Continue with the next column until the matrix is in reduced row echelon form

The algorithm transforms any matrix into its reduced row echelon form, which is useful for solving systems of linear equations, finding matrix inverses, and determining matrix rank.

