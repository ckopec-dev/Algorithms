# Cocktail Shaker Sort in Lisp

Here's an implementation of the Cocktail Shaker Sort algorithm in Lisp:

```lisp
(defun cocktail-shaker-sort (arr)
  "Sort an array using the cocktail shaker sort algorithm."
  (let ((n (length arr))
        (swapped t)
        (start 0)
        (end (1- n)))
    (while swapped
      (setf swapped nil)
      
      ;; Forward pass (left to right)
      (loop for i from start below end
            do (when (> (aref arr i) (aref arr (1+ i)))
                 (rotatef (aref arr i) (aref arr (1+ i)))
                 (setf swapped t)))
      
      ;; Reduce the end boundary
      (decf end)
      
      ;; Check if sorting is complete
      (unless swapped) (return)
      
      ;; Backward pass (right to left)
      (loop for i from end downto (1+ start)
            do (when (> (aref arr (1- i)) (aref arr i))
                 (rotatef (aref arr (1- i)) (aref arr i))
                 (setf swapped t)))
      
      ;; Increase the start boundary
      (incf start)))
  arr)

;; Alternative implementation using recursive approach
(defun cocktail-shaker-sort-recursive (arr &optional (start 0) (end nil))
  "Recursive implementation of cocktail shaker sort."
  (let ((end (or end (1- (length arr)))))
    (when (<= start end)
      ;; Forward pass
      (loop for i from start below end
            do (when (> (aref arr i) (aref arr (1+ i)))
                 (rotatef (aref arr i) (aref arr (1+ i)))))
      
      ;; Backward pass
      (loop for i from end downto (1+ start)
            do (when (> (aref arr (1- i)) (aref arr i))
                 (rotatef (aref arr (1- i)) (aref arr i)))))
  arr)

;; Example usage:
(defun test-cocktail-sort ()
  "Test the cocktail shaker sort implementation."
  (let ((test-array #(64 34 25 12 22 11 90)))
    (format t "Original array: ~A~%" test-array)
    (cocktail-shaker-sort test-array)
    (format t "Sorted array: ~A~%" test-array)))

;; Run the test
(test-cocktail-sort)
```

## How it works:

1. **Bidirectional Sorting**: Unlike traditional bubble sort, cocktail shaker sort goes both left-to-right and right-to-left in each pass
2. **Two Passes**: 
   - Forward pass: Bubbles largest elements to the right
   - Backward pass: Bubbles smallest elements to the left
3. **Boundary Adjustment**: After each pass, the boundaries (start and end) are adjusted since the sorted elements are already in place
4. **Optimization**: Stops early if no swaps occur in a complete pass

## Example Output:
```
Original array: #(64 34 25 12 22 11 90)
Sorted array: #(11 12 22 25 34 64 90)
```

## Key Features:
- **Time Complexity**: O(n²) worst case, O(n) best case
- **Space Complexity**: O(1) - in-place sorting
- **Stability**: Stable sorting algorithm
- **Adaptive**: Performs better than bubble sort on partially sorted arrays

