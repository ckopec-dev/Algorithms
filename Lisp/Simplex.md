# Simplex Algorithm in Lisp

Here's an implementation of the Simplex algorithm in Common Lisp:

```lisp
(defpackage :simplex
  (:use :cl)
  (:export :simplex-solve))

(in-package :simplex)

;; Simplex tableau representation
;; Each row represents a constraint or objective function
;; Last row is the objective function (with negative coefficients)
;; Last column contains the right-hand side values

(defun simplex-solve (tableau)
  "Solve linear programming problem using Simplex method.
   Tableau format: [[a11 a12 ... a1n b1]
                    [a21 a22 ... a2n b2]
                    [... ... ... ... ...]
                    [c1  c2  ...  cn  0]]"
  (let ((current-tableau (copy-tableau tableau)))
    (loop while (has-negative-entries (last-row current-tableau))
          do (let* ((pivot-column (find-pivot-column (last-row current-tableau)))
                    (pivot-row (find-pivot-row current-tableau pivot-column))
                    (pivot-element (get-element current-tableau pivot-row pivot-column)))
               (if (null pivot-row)
                   (error "Unbounded solution")
                   (progn
                     (normalize-row current-tableau pivot-row pivot-element)
                     (eliminate-column current-tableau pivot-row pivot-column)))))
    (format t "Optimal solution found:~%")
    (extract-solution current-tableau)))

(defun copy-tableau (tableau)
  "Create a deep copy of the tableau"
  (mapcar (lambda (row) (copy-list row)) tableau))

(defun last-row (tableau)
  "Get the last row of the tableau (objective function)"
  (car (last tableau)))

(defun has-negative-entries (row)
  "Check if any entry in the row is negative"
  (some (lambda (x) (< x 0)) row))

(defun find-pivot-column (objective-row)
  "Find the most negative entry in the objective row (entering variable)"
  (let ((min-index 0)
        (min-value (car objective-row)))
    (loop for i from 1 below (length objective-row)
          for value = (nth i objective-row)
          when (< value min-value)
            do (setf min-value value
                     min-index i))
    min-index))

(defun find-pivot-row (tableau pivot-column)
  "Find the pivot row using minimum ratio test"
  (let ((min-ratio nil)
        (pivot-row nil))
    (loop for i from 0 below (1- (length tableau))
          for row = (nth i tableau)
          for rhs = (nth (1- (length row)) row)
          for pivot-element = (nth pivot-column row)
          when (and (> pivot-element 0) (> rhs 0))
            do (let ((ratio (/ rhs pivot-element)))
                 (if (or (null min-ratio) (< ratio min-ratio))
                     (setf min-ratio ratio
                           pivot-row i)))))
    pivot-row))

(defun get-element (tableau row column)
  "Get element at specified position"
  (nth column (nth row tableau)))

(defun normalize-row (tableau row pivot-element)
  "Normalize the pivot row by dividing by pivot element"
  (let ((row-list (nth row tableau)))
    (setf (nth row tableau)
          (mapcar (lambda (x) (/ x pivot-element)) row-list))))

(defun eliminate-column (tableau pivot-row pivot-column)
  "Eliminate the pivot column using row operations"
  (loop for i from 0 below (length tableau)
        unless (= i pivot-row)
          do (let ((row (nth i tableau))
                   (pivot-element (nth pivot-column (nth pivot-row tableau))))
               (setf (nth i tableau)
                     (mapcar (lambda (current-value table-row-value)
                               (- current-value (* pivot-element table-row-value)))
                             row
                             (nth pivot-row tableau))))))

(defun extract-solution (tableau)
  "Extract the basic solution from the final tableau"
  (let ((solution '()))
    (loop for i from 0 below (1- (length (car tableau)))
          for basic-var = (find-basic-variable tableau i)
          do (if basic-var
                 (let ((value (get-basic-value tableau basic-var)))
                   (push (cons (1+ basic-var) value) solution))
                 (push (cons (1+ i) 0) solution)))
    (nreverse solution)))

(defun find-basic-variable (tableau col)
  "Find if column corresponds to a basic variable"
  (let ((count 0)
        (basic-row nil))
    (loop for i from 0 below (1- (length tableau))
          for row = (nth i tableau)
          for element = (nth col row)
          when (= element 1)
            do (incf count)
               (setf basic-row i)
          when (= element 0)
            do (incf count))
    (if (and (= count 1) basic-row)
        basic-row
        nil)))

(defun get-basic-value (tableau row)
  "Get the value of a basic variable from the tableau"
  (let ((rhs-col (1- (length (car tableau)))))
    (nth rhs-col (nth row tableau))))

;; Example usage:
;; Minimize: -3x1 - 2x2
;; Subject to: x1 + x2 <= 4
;;             2x1 + x2 <= 6
;;             x1, x2 >= 0

;; Standard form:
;; x1 + x2 + s1 = 4
;; 2x1 + x2 + s2 = 6
;; -3x1 - 2x2 + 0s1 + 0s2 = 0

(defun example-simplex ()
  "Example: Solve the linear programming problem"
  (let ((tableau '((1 1 1 0 4)   ; x1 + x2 + s1 = 4
                   (2 1 0 1 6)   ; 2x1 + x2 + s2 = 6
                   (-3 -2 0 0 0)))) ; -3x1 - 2x2 -> minimize
    (format t "Initial tableau:~%")
    (print-tableau tableau)
    (format t "~%Solving...~%")
    (simplex-solve tableau)))

(defun print-tableau (tableau)
  "Pretty print the tableau"
  (loop for row in tableau
        do (format t "~{~6,2F ~}~%" row)))

;; Run the example
(example-simplex)
```

## How to Use

1. **Load the code** into a Common Lisp environment
2. **Call the example function**: `(example-simplex)`
3. **The algorithm will solve** the linear programming problem and display the optimal solution

## Example Problem

This implementation solves:
```
Minimize: -3x₁ - 2x₂
Subject to:
  x₁ + x₂ ≤ 4
  2x₁ + x₂ ≤ 6
  x₁, x₂ ≥ 0
```

## Key Features

- **Tableau representation**: Standard simplex tableau format
- **Pivot selection**: Uses minimum ratio test for numerical stability
- **Basic solution extraction**: Properly identifies basic variables and their values
- **Error handling**: Detects unbounded solutions
- **Clean separation**: Clear function organization for each simplex step

The algorithm follows the standard simplex method steps: find entering variable, find leaving variable, pivot, and repeat until optimality is reached.

