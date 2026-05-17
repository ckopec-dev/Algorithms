# Vogel's Approximation Method in Lisp

Vogel's Approximation Method (VAM) is a heuristic algorithm for solving transportation problems. Here's an implementation in Lisp:

```lisp
(defun vogels-approximation-method (supply demand costs)
  "Solve transportation problem using Vogel's Approximation Method"
  (let ((rows (length supply))
        (cols (length demand))
        (solution (make-array (list rows cols) :initial-element 0))
        (supply-copy (copy-list supply))
        (demand-copy (copy-list demand))
        (total-cost 0))
    
    ;; Main VAM loop
    (loop while (and (some #'plusp supply-copy) (some #'plusp demand-copy))
          do
          (let ((penalties (calculate-penalties supply-copy demand-copy costs)))
            (let ((max-penalty (apply #'max penalties))
                  (max-penalty-indices (find-max-penalty-indices penalties)))
              (let ((row (first max-penalty-indices))
                    (col (second max-penalty-indices)))
                ;; Allocate maximum possible units
                (let ((allocation (min (nth row supply-copy) (nth col demand-copy))))
                  (setf (aref solution row col) allocation)
                  (incf total-cost (* allocation (aref costs row col)))
                  (decf (nth row supply-copy) allocation)
                  (decf (nth col demand-copy) allocation)
                  ;; Remove exhausted row/column if needed
                  (when (zerop (nth row supply-copy))
                    (setf (nth row supply-copy) nil))
                  (when (zerop (nth col demand-copy))
                    (setf (nth col demand-copy) nil)))))))
    
    (list :solution solution :total-cost total-cost)))

(defun calculate-penalties (supply demand costs)
  "Calculate penalties for each row and column"
  (let ((rows (length supply))
        (cols (length demand))
        (row-penalties (make-list rows))
        (col-penalties (make-list cols)))
    
    ;; Calculate row penalties
    (loop for i from 0 below rows
          do
          (let ((row-costs (remove nil (mapcar (lambda (j) (aref costs i j)) 
                                               (loop for j from 0 below cols collect j)))))
            (when (length> row-costs 1)
              (let ((sorted-row (sort (copy-list row-costs) #'<)))
                (setf (nth i row-penalties) (- (second sorted-row) (first sorted-row)))))))
    
    ;; Calculate column penalties
    (loop for j from 0 below cols
          do
          (let ((col-costs (remove nil (mapcar (lambda (i) (aref costs i j)) 
                                               (loop for i from 0 below rows collect i)))))
            (when (length> col-costs 1)
              (let ((sorted-col (sort (copy-list col-costs) #'<)))
                (setf (nth j col-penalties) (- (second sorted-col) (first sorted-col)))))))
    
    (append row-penalties col-penalties)))

(defun find-max-penalty-indices (penalties)
  "Find indices of maximum penalty"
  (let ((max-penalty (apply #'max penalties))
        (max-index 0))
    (loop for i from 0 below (length penalties)
          when (= (nth i penalties) max-penalty)
          do (setf max-index i)
          finally (return (list (floor max-index 2) (mod max-index 2))))))

;; Example usage:
(defun example-transportation-problem ()
  "Example transportation problem"
  (let* ((supply '(250 300 400))
         (demand '(200 350 250 200))
         (costs #2A((3 1 7 4)
                    (2 6 5 9)
                    (8 3 3 2))))
    
    (format t "Supply: ~A~%" supply)
    (format t "Demand: ~A~%" demand)
    (format t "Cost Matrix:~%")
    (print-costs costs)
    
    (let ((result (vogels-approximation-method supply demand costs)))
      (format t "~%Solution Matrix:~%")
      (print-solution (getf result :solution))
      (format t "Total Cost: ~A~%" (getf result :total-cost)))))

(defun print-costs (costs)
  "Pretty print cost matrix"
  (loop for i from 0 below (array-dimension costs 0)
        do (loop for j from 0 below (array-dimension costs 1)
                 do (format t "~3A " (aref costs i j)))
           (format t "~%")))

(defun print-solution (solution)
  "Pretty print solution matrix"
  (loop for i from 0 below (array-dimension solution 0)
        do (loop for j from 0 below (array-dimension solution 1)
                 do (format t "~3A " (aref solution i j)))
           (format t "~%")))

;; Run the example
(example-transportation-problem)
```

## How it works:

1. **Initialization**: Create copies of supply and demand arrays, and initialize solution matrix
2. **Penalty Calculation**: For each row and column, calculate the penalty by finding the difference between the two smallest costs
3. **Maximum Penalty Selection**: Select the cell with the maximum penalty
4. **Allocation**: Allocate as many units as possible to the selected cell
5. **Update**: Adjust supply/demand and remove exhausted rows/columns
6. **Repeat**: Continue until all supplies and demands are satisfied

## Key Features:

- **Heuristic Approach**: Provides near-optimal solutions quickly
- **Penalty Method**: Uses the difference between smallest costs to prioritize allocations
- **Matrix Operations**: Handles 2D cost matrices efficiently
- **Memory Management**: Uses array and list operations appropriately

## Time Complexity:
O(m×n) where m is number of sources and n is number of destinations

This implementation demonstrates the core logic of Vogel's Approximation Method in a functional Lisp style.

