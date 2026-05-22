# Dynamic Programming Example in Lisp: Fibonacci Sequence

Here's an implementation of the Fibonacci sequence using dynamic programming with memoization in Common Lisp:

```lisp
(defun fibonacci-dp (n)
  "Calculate nth Fibonacci number using dynamic programming with memoization."
  (let ((memo (make-hash-table :test 'equal)))
    (labels ((fib (n)
               (cond
                 ;; Base cases
                 ((= n 0) 0)
                 ((= n 1) 1)
                 ;; Check if already computed
                 ((gethash n memo) (gethash n memo))
                 ;; Compute and store result
                 (t (let ((result (+ (fib (- n 1)) (fib (- n 2)))))
                      (setf (gethash n memo) result)
                      result)))))
      (fib n))))

;; Alternative implementation using array for better performance
(defun fibonacci-array (n)
  "Calculate nth Fibonacci number using dynamic programming with array."
  (if (<= n 1)
      n
      (let ((dp (make-array (1+ n) :initial-element 0)))
        (setf (aref dp 0) 0
              (aref dp 1) 1)
        (loop for i from 2 to n do
              (setf (aref dp i) (+ (aref dp (- i 1)) (aref dp (- i 2)))))
        (aref dp n))))

;; Example usage:
;; (fibonacci-dp 10)   ; Returns 55
;; (fibonacci-array 10) ; Returns 55

;; Test the functions
(format t "Fibonacci(10) using hash table: ~A~%" (fibonacci-dp 10))
(format t "Fibonacci(10) using array: ~A~%" (fibonacci-array 10))
```

## Key Features of this Dynamic Programming Implementation:

1. **Memoization**: Uses a hash table to store previously computed results
2. **Overlapping Subproblems**: Avoids recomputing the same Fibonacci numbers
3. **Optimal Substructure**: The solution builds on smaller subproblems
4. **Time Complexity**: Reduced from O(2^n) to O(n)
5. **Space Complexity**: O(n) for storing computed values

## How it works:

1. **Base cases**: F(0) = 0, F(1) = 1
2. **Memoization check**: Before computing, check if result exists
3. **Recursive computation**: F(n) = F(n-1) + F(n-2)
4. **Storage**: Store results to avoid future recomputation

This approach demonstrates the core principle of dynamic programming: breaking down complex problems into simpler subproblems and storing their solutions for reuse.

