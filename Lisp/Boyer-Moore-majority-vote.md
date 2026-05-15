# Boyer-Moore Majority Vote Algorithm in Lisp

Here's an implementation of the Boyer-Moore Majority Vote algorithm in Common Lisp:

```lisp
(defun boyer-moore-majority-vote (array)
  "Find the majority element in an array using Boyer-Moore Majority Vote algorithm.
   Returns the majority element if it exists, otherwise returns NIL."
  (if (null array)
      nil
      (let ((candidate nil)
            (count 0))
        ;; Phase 1: Find candidate
        (dolist (element array)
          (cond
            ((zerop count)
             (setf candidate element
                   count 1))
            ((equal element candidate)
             (incf count))
            (t
             (decf count))))
        ;; Phase 2: Verify candidate is actually majority
        (let ((candidate-count 0))
          (dolist (element array)
            (when (equal element candidate)
              (incf candidate-count)))
          (if (> candidate-count (floor (length array) 2))
              candidate
              nil)))))

;; Example usage:
;; Test case 1: Array with majority element
(setq test-array1 '(3 3 4 2 4 4 2 4 4))
(boyer-moore-majority-vote test-array1)
;; Returns: 4

;; Test case 2: Array without majority element
(setq test-array2 '(3 3 4 2 4 4 2 4))
(boyer-moore-majority-vote test-array2)
;; Returns: NIL

;; Test case 3: Array with single element
(setq test-array3 '(5))
(boyer-moore-majority-vote test-array3)
;; Returns: 5

;; Test case 4: Empty array
(setq test-array4 '())
(boyer-moore-majority-vote test-array4)
;; Returns: NIL
```

## Algorithm Explanation

The Boyer-Moore Majority Vote algorithm works in two phases:

1. **Phase 1 - Candidate Selection**: 
   - Maintain a candidate element and a counter
   - If counter is 0, set current element as candidate
   - If current element equals candidate, increment counter
   - Otherwise, decrement counter

2. **Phase 2 - Verification**:
   - Count occurrences of the candidate
   - Return candidate only if it appears more than n/2 times

## Time and Space Complexity

- **Time Complexity**: O(n) - two passes through the array
- **Space Complexity**: O(1) - only using a constant amount of extra space

## Key Features

- Handles edge cases (empty arrays, single elements)
- Returns NIL when no majority element exists
- Works with any comparable elements (numbers, symbols, etc.)
- Efficient and optimal for the majority vote problem

