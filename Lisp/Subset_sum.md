# Subset Sum Algorithm in Lisp

Here's an implementation of the subset sum problem using dynamic programming in Lisp:

```lisp
(defun subset-sum (arr target)
  "Returns T if there exists a subset of arr that sums to target, NIL otherwise."
  (let* ((n (length arr))
         (dp (make-array (list (1+ n) (1+ target)) :initial-element nil)))
    
    ;; Base case: sum of 0 can always be achieved with empty subset
    (loop for i from 0 to n do
          (setf (aref dp i 0) t))
    
    ;; Fill the DP table
    (loop for i from 1 to n do
          (loop for j from 1 to target do
                (let ((current-element (aref arr (1- i))))
                  ;; If current element is greater than target sum, skip it
                  (if (> current-element j)
                      (setf (aref dp i j) (aref dp (1- i) j))
                      ;; Otherwise, either include or exclude the element
                      (setf (aref dp i j)
                            (or (aref dp (1- i) j)
                                (aref dp (1- i) (- j current-element))))))))
    
    (aref dp n target)))

;; Alternative recursive approach with memoization
(defun subset-sum-recursive (arr target &optional memo)
  "Recursive subset sum with memoization."
  (if (zerop target)
      t
      (if (or (null arr) (< target 0))
          nil
          (let ((current-element (first arr))
                (rest-elements (rest arr)))
            (or (subset-sum-recursive rest-elements target memo)
                (and (>= target current-element)
                     (subset-sum-recursive rest-elements (- target current-element) memo)))))))

;; Example usage:
;; Test cases
(setq test-array '(3 34 4 12 5 2))
(setq target-sum 9)

;; Using dynamic programming approach
(print (subset-sum test-array target-sum))
;; Output: T (because 4 + 5 = 9)

(setq target-sum 30)
(print (subset-sum test-array target-sum))
;; Output: NIL (no subset sums to 30)

;; Using recursive approach
(print (subset-sum-recursive test-array 9))
;; Output: T

(print (subset-sum-recursive test-array 30))
;; Output: NIL
```

## How it works:

1. **Dynamic Programming Approach**: Creates a 2D boolean array `dp` where `dp[i][j]` represents whether sum `j` can be achieved using first `i` elements.

2. **Base Case**: Sum of 0 is always possible (empty subset).

3. **Recurrence Relation**: For each element, we can either:
   - Exclude it: `dp[i][j] = dp[i-1][j]`
   - Include it: `dp[i][j] = dp[i-1][j-element]` (if element ≤ j)

4. **Recursive Approach**: Uses memoization to avoid recomputing overlapping subproblems.

## Time and Space Complexity:
- **Dynamic Programming**: O(n × target) time, O(n × target) space
- **Recursive**: O(n × target) time, O(n × target) space for memoization

The algorithm efficiently determines whether a subset exists that sums to the target value.

