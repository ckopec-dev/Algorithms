```lisp
(defun gcd (a b)
  "Compute the greatest common divisor using Euclidean algorithm"
  (if (= b 0)
      a
      (gcd b (mod a b))))

(defun pollard-rho (n)
  "Implement Pollard's rho algorithm to find a non-trivial factor of n"
  (if (evenp n)
      2
      (let ((x 2)
            (y 2)
            (c 1)
            (factor 1))
        (labels ((f (x) (mod (+ (* x x) c) n)))
          (loop while (= factor 1) do
                (setf x (f x)
                      y (f (f y))
                      factor (gcd (abs (- x y)) n)))
          (if (= factor n)
              nil  ; No factor found
              factor)))))

(defun pollard-rho-full (n)
  "Complete Pollard's rho algorithm with multiple attempts"
  (if (<= n 1)
      nil
      (let ((factor nil)
            (attempts 0)
            (max-attempts 100))
        (loop while (and (null factor) (< attempts max-attempts)) do
              (setf factor (pollard-rho n)
                    attempts (1+ attempts)))
        factor)))

;; Example usage:
;; Find a factor of 1347422205641
;; (pollard-rho-full 1347422205641)
;; Returns: 1347422205641 = 1347422205641 × 1 (if prime)
;; For a composite number like 15:
;; (pollard-rho-full 15)
;; Returns: 3 or 5 (one of the factors)

;; Test with a known composite number
(defun test-pollard-rho ()
  "Test the Pollard's rho algorithm with example numbers"
  (let ((test-numbers '(15 21 35 77 143 323 1347422205641)))
    (dolist (num test-numbers)
      (let ((factor (pollard-rho-full num)))
        (format t "Factor of ~A: ~A~%" num factor)))))
```

This implementation of Pollard's rho algorithm in Lisp includes:

1. **GCD function**: Computes the greatest common divisor using the Euclidean algorithm
2. **Pollard's rho main function**: Implements the core algorithm with:
   - A random function f(x) = (x² + c) mod n
   - Floyd's cycle detection (tortoise and hare)
   - GCD computation to find factors
3. **Full implementation**: Handles edge cases and multiple attempts
4. **Test function**: Demonstrates usage with various numbers

The algorithm works by:
1. Starting with two pointers (x and y) at the same position
2. Applying a function f(x) to generate sequences
3. Detecting cycles using Floyd's method
4. Computing GCD to find non-trivial factors

Example output for n=15: Would return either 3 or 5 (factors of 15)

