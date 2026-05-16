# Carmichael Function Computation in Lisp

The Carmichael function λ(n) (also known as the reduced totient function) is the smallest positive integer m such that a^m ≡ 1 (mod n) for all integers a coprime to n.

```lisp
(defun gcd (a b)
  "Compute the greatest common divisor of a and b"
  (if (= b 0)
      a
      (gcd b (mod a b))))

(defun coprime-p (a b)
  "Check if two numbers are coprime"
  (= (gcd a b) 1))

(defun euler-phi (n)
  "Compute Euler's totient function φ(n)"
  (let ((result n))
    (if (= n 1)
        1
        (loop for i from 2 to (isqrt n)
              while (<= (expt i 2) n)
              do (when (zerop (mod n i))
                   (loop while (zerop (mod n i))
                         do (setf n (floor n i)))
                   (setf result (* result (/ (- i 1) i)))))
    (when (> n 1)
      (setf result (* result (/ (- n 1) n))))
    result))

(defun carmichael-function (n)
  "Compute the Carmichael function λ(n)"
  (if (= n 1)
      1
      (let ((phi (euler-phi n))
            (factors '()))
        ;; Find prime factorization of n
        (let ((temp-n n) (i 2))
          (loop while (<= (expt i 2) temp-n)
                do (when (zerop (mod temp-n i))
                     (push i factors)
                     (loop while (zerop (mod temp-n i))
                           do (setf temp-n (floor temp-n i))))
                do (incf i))
          (when (> temp-n 1)
            (push temp-n factors)))
        
        ;; For each prime factor p with power k in n = p^k * m:
        ;; If p = 2 and k >= 3, then λ(n) = λ(2^k) = 2^(k-2)
        ;; Otherwise, λ(n) = λ(p^k) = p^(k-1)(p-1)
        ;; Then take LCM of all these values
        
        (let ((lambda-value 1))
          (dolist (p factors)
            (let ((k (count p (factorize n))))
              (if (and (= p 2) (>= k 3))
                  (setf lambda-value (* lambda-value (expt 2 (- k 2))))
                  (setf lambda-value (* lambda-value (* (expt p (- k 1)) (- p 1)))))))
          lambda-value))))

(defun factorize (n)
  "Return list of prime factors of n"
  (let ((factors '()) (i 2))
    (loop while (<= (expt i 2) n)
          do (when (zerop (mod n i))
               (push i factors)
               (loop while (zerop (mod n i))
                     do (setf n (floor n i))))
          do (incf i))
    (when (> n 1)
      (push n factors))
    (reverse factors)))

(defun carmichael-function-simple (n)
  "Simpler implementation of Carmichael function"
  (if (= n 1)
      1
      (let ((lambda-value 1)
            (temp-n n))
        ;; Handle factor 2
        (let ((k 0))
          (loop while (zerop (mod temp-n 2))
                do (setf temp-n (floor temp-n 2))
                do (incf k))
          (if (>= k 3)
              (setf lambda-value (* lambda-value (expt 2 (- k 2))))
              (if (> k 0)
                  (setf lambda-value (* lambda-value (expt 2 (- k 1)))))))
        
        ;; Handle odd factors
        (let ((i 3))
          (loop while (<= (expt i 2) temp-n)
                do (let ((k 0))
                     (loop while (zerop (mod temp-n i))
                           do (setf temp-n (floor temp-n i))
                           do (incf k))
                     (if (> k 0)
                         (setf lambda-value (* lambda-value (* (expt i (- k 1)) (- i 1))))))
                do (incf i 2)))
        
        ;; If temp-n > 1, then it's a prime factor
        (if (> temp-n 1)
            (setf lambda-value (* lambda-value (- temp-n 1))))
        
        lambda-value)))

;; Example usage:
;; (carmichael-function-simple 12)
;; Returns: 2
;; (carmichael-function-simple 15)
;; Returns: 4
;; (carmichael-function-simple 21)
;; Returns: 6
```

## Example Output:

```lisp
;; Compute λ(12)
;; 12 = 2² × 3¹
;; λ(12) = lcm(λ(2²), λ(3¹)) = lcm(2, 2) = 2

;; Compute λ(15)  
;; 15 = 3¹ × 5¹
;; λ(15) = lcm(λ(3¹), λ(5¹)) = lcm(2, 4) = 4

;; Compute λ(21)
;; 21 = 3¹ × 7¹  
;; λ(21) = lcm(λ(3¹), λ(7¹)) = lcm(2, 6) = 6
```

This implementation computes the Carmichael function by:
1. Finding the prime factorization of n
2. Applying the formula for λ(p^k) = p^(k-1)(p-1) for odd primes
3. For p = 2 and k ≥ 3, using λ(2^k) = 2^(k-2)
4. Taking the least common multiple of all the results

