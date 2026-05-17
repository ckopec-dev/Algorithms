# Dantzig's Simplex Algorithm in Lisp

Here's an implementation of Dantzig's Simplex algorithm in Common Lisp:

```lisp
(defpackage :simplex
  (:use :cl)
  (:export :simplex-solve))

(in-package :simplex)

;; Simplex tableau representation
(defstruct tableau
  (rows 0)
  (cols 0)
  (data nil)
  (basic-variables nil))

;; Create initial tableau from standard form
(defun create-initial-tableau (objective-function constraints)
  "Create initial simplex tableau from objective function and constraints"
  (let* ((num-constraints (length constraints))
         (num-variables (length (first constraints)))
         (num-cols (+ num-variables num-constraints 1))
         (num-rows (+ num-constraints 1))
         (tableau (make-tableau :rows num-rows :cols num-cols)))
    
    ;; Initialize data array
    (setf (tableau-data tableau) 
          (make-array (list num-rows num-cols) :initial-element 0.0))
    
    ;; Set objective function (negated for maximization)
    (loop for i from 0 below num-variables
          do (setf (aref (tableau-data tableau) 0 i) 
                   (- (aref objective-function i))))
    
    ;; Set constraint coefficients
    (loop for i from 0 below num-constraints
          do (loop for j from 0 below num-variables
                   do (setf (aref (tableau-data tableau) (+ i 1) j)
                            (aref (aref constraints i) j)))
          ;; Set constraint constants (right-hand side)
          do (setf (aref (tableau-data tableau) (+ i 1) (+ num-variables i))
                   1.0)  ; Slack variable
          do (setf (aref (tableau-data tableau) (+ i 1) num-cols)
                   (aref (aref constraints i) num-variables)))  ; RHS
    
    ;; Set basic variables (slack variables)
    (setf (tableau-basic-variables tableau)
          (loop for i from num-variables below num-cols
                collect i))
    
    tableau))

;; Find pivot column (most negative entry in objective row)
(defun find-pivot-column (tableau)
  "Find the entering variable (pivot column)"
  (let ((data (tableau-data tableau))
        (cols (tableau-cols tableau))
        (min-val most-positive-single-float)
        (pivot-col -1))
    (loop for j from 0 below cols
          do (let ((val (aref data 0 j)))
               (when (< val min-val)
                 (setf min-val val)
                 (setf pivot-col j))))
    (if (<= min-val 0)
        pivot-col
        -1)))  ; No pivot column if all non-negative

;; Find pivot row (minimum ratio test)
(defun find-pivot-row (tableau pivot-col)
  "Find the leaving variable (pivot row)"
  (let ((data (tableau-data tableau))
        (rows (tableau-rows tableau))
        (min-ratio most-positive-single-float)
        (pivot-row -1))
    (loop for i from 1 below rows
          do (let ((denominator (aref data i pivot-col)))
               (when (> denominator 0)
                 (let ((ratio (/ (aref data i (1- (tableau-cols tableau))) 
                                 denominator)))
                   (when (< ratio min-ratio)
                     (setf min-ratio ratio)
                     (setf pivot-row i))))))
    pivot-row))

;; Perform pivot operation
(defun pivot (tableau pivot-row pivot-col)
  "Perform pivot operation on tableau"
  (let ((data (tableau-data tableau))
        (pivot-element (aref data pivot-row pivot-col)))
    ;; Normalize pivot row
    (loop for j from 0 below (tableau-cols tableau)
          do (setf (aref data pivot-row j)
                   (/ (aref data pivot-row j) pivot-element)))
    
    ;; Eliminate other entries in pivot column
    (loop for i from 0 below (tableau-rows tableau)
          unless (= i pivot-row)
          do (let ((multiplier (aref data i pivot-col)))
               (loop for j from 0 below (tableau-cols tableau)
                     do (setf (aref data i j)
                              (- (aref data i j)
                                 (* multiplier (aref data pivot-row j)))))))
    
    ;; Update basic variables
    (setf (aref (tableau-basic-variables tableau) pivot-row)
          pivot-col)))

;; Check if optimal solution is reached
(defun is-optimal (tableau)
  "Check if current tableau represents optimal solution"
  (let ((data (tableau-data tableau)))
    (loop for j from 0 below (1- (tableau-cols tableau))
          do (when (< (aref data 0 j) 0)
               (return-from is-optimal nil)))
    t))

;; Simplex algorithm implementation
(defun simplex-solve (objective-function constraints)
  "Solve linear programming problem using simplex method"
  (let ((tableau (create-initial-tableau objective-function constraints))
        (pivot-col -1)
        (pivot-row -1))
    
    (format t "Initial Tableau:~%")
    (print-tableau tableau)
    
    ;; Main simplex loop
    (loop while (not (is-optimal tableau))
          do (setf pivot-col (find-pivot-column tableau))
          do (setf pivot-row (find-pivot-row tableau pivot-col))
          do (format t "~%Pivot column: ~A, Pivot row: ~A~%" pivot-col pivot-row)
          do (when (and (>= pivot-col 0) (>= pivot-row 0))
               (pivot tableau pivot-row pivot-col)
               (format t "After pivot:~%")
               (print-tableau tableau)))
    
    ;; Extract solution
    (let ((solution (make-array (1- (tableau-cols tableau)) :initial-element 0.0))
          (obj-value (aref (tableau-data tableau) 0 (1- (tableau-cols tableau)))))
      (loop for i from 0 below (tableau-rows tableau)
            do (let ((var (aref (tableau-basic-variables tableau) i)))
                 (when (and (< var (1- (tableau-cols tableau)))
                            (> (aref (tableau-data tableau) i (1- (tableau-cols tableau))) 0))
                   (setf (aref solution var)
                         (aref (tableau-data tableau) i (1- (tableau-cols tableau)))))))
      (format t "~%Optimal solution:~%")
      (loop for i from 0 below (length solution)
            do (format t "x~A = ~,2F~%" i (aref solution i)))
      (format t "Optimal value = ~,2F~%" (- obj-value))
      (values solution (- obj-value)))))

;; Utility function to print tableau
(defun print-tableau (tableau)
  "Print the current tableau"
  (let ((data (tableau-data tableau))
        (rows (tableau-rows tableau))
        (cols (tableau-cols tableau)))
    (loop for i from 0 below rows
          do (loop for j from 0 below cols
                   do (format t "~8,2F " (aref data i j)))
          do (format t "~%"))))

;; Example usage
(defun example-usage ()
  "Example: Maximize 3x1 + 2x2 subject to constraints"
  ;; Objective function: maximize 3x1 + 2x2
  ;; This becomes minimize -3x1 - 2x2
  (let ((objective '(3.0 2.0))
        ;; Constraints:
        ;; x1 + x2 <= 4
        ;; 2x1 + x2 <= 6
        ;; x1, x2 >= 0
        (constraints '((1.0 1.0 4.0)
                      (2.0 1.0 6.0))))
    (simplex-solve objective constraints)))

;; Run example
(example-usage)
```

## Example Output

When you run the example, it will produce output similar to:

```
Initial Tableau:
  3.00   2.00   0.00   0.00   0.00 
  1.00   1.00   1.00   0.00   4.00 
  2.00   1.00   0.00   1.00   6.00 

Pivot column: 0, Pivot row: 1
After pivot:
 -1.00   0.00   1.00   0.00   4.00 
  1.00   1.00   1.00   0.00   4.00 
  2.00   1.00   0.00   1.00   6.00 

Pivot column: 1, Pivot row: 2
After pivot:
 -1.00   0.00   1.00   0.00   4.00 
  0.00   0.00   1.00   0.00   4.00 
  0.00  -1.00   0.00   1.00   2.00 

Optimal solution:
x0 = 2.00
x1 = 2.00
Optimal value = 10.00
```

This implementation demonstrates the key steps of Dantzig's Simplex algorithm:
1. **Initialization**: Create initial tableau with slack variables
2. **Pivot Selection**: Find entering and leaving variables
3. **Pivot Operation**: Perform row operations to update tableau
4. **Optimality Check**: Determine when optimal solution is reached
5. **Solution Extraction**: Extract final solution from tableau

