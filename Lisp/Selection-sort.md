# Selection Sort in Lisp

Here's an implementation of the Selection Sort algorithm in Lisp:

```lisp
(defun selection-sort (arr)
  "Sort an array using the selection sort algorithm"
  (let ((n (length arr)))
    (loop for i from 0 below n
          do (let ((min-index i))
               (loop for j from (1+ i) below n
                     when (< (aref arr j) (aref arr min-index))
                     do (setf min-index j))
               (when (/= min-index i)
                 (let ((temp (aref arr i)))
                   (setf (aref arr i) (aref arr min-index))
                   (setf (aref arr min-index) temp)))))
    arr))

;; Example usage:
(setq my-array (vector 64 34 25 12 22 11 90))
(print my-array)
(selection-sort my-array)
(print my-array)
```

## How it works:

1. **Find the minimum**: For each position `i`, find the smallest element in the remaining unsorted portion
2. **Swap**: Swap the minimum element with the element at position `i`
3. **Repeat**: Continue until the entire array is sorted

## Output:
```
#(64 34 25 12 22 11 90)
#(11 12 22 25 34 64 90)
```

## Key Lisp Features Used:

- `defun` - Function definition
- `let` - Local variable binding
- `loop` - Loop construct for iteration
- `aref` - Array element access
- `setf` - Assignment
- `vector` - Array creation
- `length` - Array length function

The algorithm has O(n²) time complexity and O(1) space complexity, making it simple but inefficient for large datasets.

