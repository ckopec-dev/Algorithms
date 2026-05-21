# Comb Sort Algorithm in Lisp

Here's an implementation of the Comb Sort algorithm in Lisp:

```lisp
(defun comb-sort (arr)
  "Sort an array using the Comb Sort algorithm"
  (let ((n (length arr))
        (gap n)
        (swapped t))
    ;; Continue while gap > 1 or we made a swap
    (loop while (or (> gap 1) swapped)
          do (progn
               ;; Calculate next gap
               (setf gap (floor (+ gap 1) 1.3))
               (when (< gap 1) (setf gap 1))
               
               ;; Reset swapped flag
               (setf swapped nil)
               
               ;; Compare elements with current gap
               (loop for i from 0 below (- n gap)
                     do (progn
                          (when (> (aref arr i) (aref arr (+ i gap)))
                            (progn
                              ;; Swap elements
                              (let ((temp (aref arr i)))
                                (setf (aref arr i) (aref arr (+ i gap)))
                                (setf (aref arr (+ i gap)) temp))
                              (setf swapped t)))))))
  arr)

;; Example usage:
(defun test-comb-sort ()
  "Test the comb sort function with example arrays"
  (let ((test-array1 #(64 34 25 12 22 11 90))
        (test-array2 #(5 2 8 1 9))
        (test-array3 #(1)))
    (format t "Original array 1: ~A~%" test-array1)
    (format t "Sorted array 1: ~A~%" (comb-sort test-array1))
    
    (format t "Original array 2: ~A~%" test-array2)
    (format t "Sorted array 2: ~A~%" (comb-sort test-array2))
    
    (format t "Original array 3: ~A~%" test-array3)
    (format t "Sorted array 3: ~A~%" (comb-sort test-array3))))

;; Run the test
(test-comb-sort)
```

## How it works:

1. **Initialize**: Start with a gap equal to the array length
2. **Gap reduction**: Reduce the gap by dividing by 1.3 (using floor for integer division)
3. **Compare**: Compare elements separated by the current gap
4. **Swap**: If elements are out of order, swap them
5. **Repeat**: Continue until gap becomes 1 and no more swaps occur

## Key Features:

- **Gap sequence**: Uses the golden ratio (1.3) for optimal performance
- **Early termination**: Stops when no swaps occur in a complete pass
- **In-place sorting**: Sorts the array without requiring additional memory
- **Time complexity**: O(n²) worst case, O(n log n) average case

## Output example:
```
Original array 1: #(64 34 25 12 22 11 90)
Sorted array 1: #(11 12 22 25 34 64 90)
Original array 2: #(5 2 8 1 9)
Sorted array 2: #(1 2 5 8 9)
Original array 3: #(1)
Sorted array 3: #(1)
```

