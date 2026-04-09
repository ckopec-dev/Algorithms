# Ternary Search Algorithm in Lisp

Here's an implementation of the ternary search algorithm in Lisp:

```lisp
(defun ternary-search (arr target left right)
  "Perform ternary search on a sorted array for target value.
   Returns the index of target if found, otherwise returns nil."
  (if (> left right)
      nil
      (let* ((mid1 (+ left (floor (- right left) 3)))
             (mid2 (+ left (floor (* 2 (- right left)) 3))))
        (cond
          ;; If target is at mid1
          ((= (aref arr mid1) target) mid1)
          ;; If target is at mid2
          ((= (aref arr mid2) target) mid2)
          ;; If target is in the left third
          ((< target (aref arr mid1))
           (ternary-search arr target left (1- mid1)))
          ;; If target is in the right third
          ((> target (aref arr mid2))
           (ternary-search arr target (1+ mid2) right))
          ;; If target is in the middle third
          (t
           (ternary-search arr target (1+ mid1) (1- mid2)))))))

;; Example usage:
(defun example-ternary-search ()
  "Example demonstrating ternary search functionality"
  (let ((sorted-array (make-array 10 
                                  :element-type 'integer 
                                  :initial-contents '(1 3 5 7 9 11 13 15 17 19))))
    (format t "Array: ~A~%" sorted-array)
    (format t "Searching for 7: ~A~%" (ternary-search sorted-array 7 0 9))
    (format t "Searching for 15: ~A~%" (ternary-search sorted-array 15 0 9))
    (format t "Searching for 4: ~A~%" (ternary-search sorted-array 4 0 9))))

;; Run the example
(example-ternary-search)
```

## How it works:

1. **Divide**: The array is divided into three parts using two midpoints (`mid1` and `mid2`)
2. **Compare**: The target is compared with elements at both midpoints
3. **Conquer**: Based on the comparison, the search space is reduced to one of three segments:
   - Left third (if target < mid1)
   - Middle third (if mid1 < target < mid2)
   - Right third (if target > mid2)

## Time Complexity:
- **Best Case**: O(1)
- **Average Case**: O(log₃ n)
- **Worst Case**: O(log₃ n)

## Space Complexity:
- O(log₃ n) for the recursive implementation
- O(1) for the iterative version

The ternary search is particularly useful when the array is already sorted and you want to minimize the number of comparisons compared to binary search.

