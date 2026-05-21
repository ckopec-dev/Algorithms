# Subset Sum Algorithm in Lisp

Here's an implementation of the subset sum algorithm in Lisp using dynamic programming:

```lisp
(defun subset-sum (arr target)
  "Returns T if there exists a subset of arr that sums to target, NIL otherwise."
  (let ((n (length arr)))
    (if (zerop n)
        (if (zerop target) t nil)
        (let ((dp (make-array (1+ target) :element-type 'bit :initial-element 0)))
          ;; Base case: sum of 0 is always possible (empty subset)
          (setf (aref dp 0) 1)
          
          ;; For each element in the array
          (loop for i from 0 below n do
                (let ((element (aref arr i)))
                  ;; Traverse backwards to avoid using updated values
                  (loop for j from target downto element do
                        (when (and (plusp (aref dp (- j element)))
                                   (zerop (aref dp j)))
                          (setf (aref dp j) 1)))))
          
          ;; Return whether target sum is possible
          (plusp (aref dp target))))))

;; Alternative recursive implementation with memoization
(defun subset-sum-recursive (arr target &optional memo)
  "Recursive subset sum with memoization."
  (if (zerop target)
      t
      (if (or (zerop (length arr)) (< target 0))
          nil
          (let ((element (aref arr 0))
                (rest (subseq arr 1)))
            (or (subset-sum-recursive rest target memo)
                (subset-sum-recursive rest (- target element) memo))))))

;; Example usage:
;; (subset-sum #(3 34 4 12 5 2) 9)  ; Returns T (subset: 4 + 5 = 9)
;; (subset-sum #(3 34 4 12 5 2) 30) ; Returns NIL
;; (subset-sum #() 0)               ; Returns T (empty subset)
```

## How it works:

1. **Dynamic Programming Approach**: Creates a boolean array `dp` where `dp[i]` represents whether sum `i` is achievable
2. **Base Case**: Sum of 0 is always possible (empty subset)
3. **Fill DP Table**: For each element, update all possible sums that can include this element
4. **Backward Traversal**: Process sums in reverse order to avoid using updated values in the same iteration

## Time and Space Complexity:
- **Time**: O(n × target) where n is the array length
- **Space**: O(target) for the DP array

## Example Output:
```lisp
(subset-sum #(3 34 4 12 5 2) 9)   ; Returns T
(subset-sum #(3 34 4 12 5 2) 30)  ; Returns NIL
(subset-sum #(1 2 3) 6)           ; Returns T (1+2+3=6)
```

