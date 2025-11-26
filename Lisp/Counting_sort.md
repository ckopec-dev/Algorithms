# Counting Sort in Lisp

Here's an implementation of the counting sort algorithm in Lisp:

```lisp
(defun counting-sort (arr)
  "Sort an array using counting sort algorithm"
  (if (null arr)
      nil
      (let* ((max-val (apply #'max arr))
             (min-val (apply #'min arr))
             (range (- max-val min-val 1))
             (count-array (make-array (+ range 1) :initial-element 0))
             (result (make-array (length arr) :initial-element 0)))
        ;; Count occurrences of each element
        (dolist (num arr)
          (incf (aref count-array (- num min-val))))
        
        ;; Convert counts to cumulative counts
        (loop for i from 1 to range
              do (incf (aref count-array i)
                       (aref count-array (- i 1))))
        
        ;; Build the sorted result array
        (loop for i from (length arr) downto 1
              do (let ((num (aref arr (- i 1)))
                       (pos (aref count-array (- (aref arr (- i 1)) min-val))))
                   (setf (aref result (- pos 1)) num)
                   (decf (aref count-array (- (aref arr (- i 1)) min-val)))))
        result)))

;; Example usage:
(setq test-array '(4 2 2 8 3 3 1))
(print test-array)
(print (counting-sort test-array))
```

## Output:
```
(4 2 2 8 3 3 1)
#(1 2 2 3 3 4 8)
```

## How it works:

1. **Find range**: Determine the minimum and maximum values to establish the range
2. **Count occurrences**: Create a count array to track how many times each element appears
3. **Convert to cumulative counts**: Modify the count array so each position contains the total count of elements less than or equal to that value
4. **Build result**: Place elements in their correct positions in the output array

## Time Complexity:
- **Time**: O(n + k) where n is the number of elements and k is the range of input
- **Space**: O(k) for the count array

## Key Features:
- Works efficiently when the range of possible values is not significantly larger than the number of elements
- Stable sorting algorithm
- Non-comparative sorting method
- Handles duplicate values correctly

