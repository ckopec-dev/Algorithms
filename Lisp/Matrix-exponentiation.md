# Matrix Exponentiation in Lisp

Here's an example implementation of matrix exponentiation in Lisp:

```lisp
;; Matrix multiplication function
(defun matrix-multiply (a b)
  "Multiply two matrices A and B"
  (let ((rows-a (length a))
        (cols-a (length (first a)))
        (cols-b (length (first b))))
    (if (not (= (length (first a)) (length b)))
        (error "Matrix dimensions don't match for multiplication")
        (let ((result (make-list rows-a :initial-element nil)))
          (loop for i from 0 below rows-a do
                (setf (nth i result)
                      (loop for j from 0 below cols-b collect
                            (loop for k from 0 below cols-a
                                  sum (* (nth k (nth i a))
                                         (nth j (nth k b)))))))
          result))))

;; Matrix exponentiation using fast exponentiation
(defun matrix-power (matrix n)
  "Compute matrix^n using fast exponentiation"
  (if (= n 0)
      ;; Return identity matrix
      (let ((size (length matrix)))
        (loop for i from 0 below size collect
              (loop for j from 0 below size
                    collect (if (= i j) 1 0))))
      (if (= n 1)
          matrix
          (if (evenp n)
              ;; If n is even: (A^(n/2))^2
              (let ((half-power (matrix-power matrix (/ n 2))))
                (matrix-multiply half-power half-power))
              ;; If n is odd: A * A^(n-1)
              (matrix-multiply matrix (matrix-power matrix (- n 1)))))))

;; Example usage:
;; Define a 2x2 matrix
(setq my-matrix '((1 1) (1 0)))

;; Compute matrix^5
(matrix-power my-matrix 5)

;; Expected output for Fibonacci matrix:
;; '((8 5) (5 3))
```

## How it works:

1. **Matrix Multiply**: Multiplies two matrices using standard matrix multiplication algorithm
2. **Matrix Power**: Uses fast exponentiation (binary exponentiation) to compute matrix^n efficiently
3. **Base Cases**: 
   - Matrix^0 = Identity matrix
   - Matrix^1 = Matrix itself
4. **Recursive Cases**:
   - If n is even: (A^(n/2))^2
   - If n is odd: A × A^(n-1)

## Time Complexity:
- **Time**: O(log n × m³) where m is the matrix dimension
- **Space**: O(log n) for the recursion stack

This implementation is particularly useful for computing Fibonacci numbers, where the nth Fibonacci number can be obtained by computing the matrix [[1 1] [1 0]]^n and taking the top-left element of the result.

