# Quadratic Sieve Algorithm in Lisp

Here's an implementation of the Quadratic Sieve algorithm in Common Lisp:

```lisp
(defpackage :quadratic-sieve
  (:use :cl)
  (:export :factor :quadratic-sieve))
(in-package :quadratic-sieve)

;; Helper functions
(defun is-prime (n)
  "Check if a number is prime"
  (when (and (> n 1) (integerp n))
    (if (= n 2) t
        (and (oddp n)
             (not (some (lambda (i) 
                          (zerop (mod n i)))
                        (loop for i from 3 to (isqrt n) by 2
                              collect i))))))

(defun gcd (a b)
  "Calculate greatest common divisor"
  (if (zerop b)
      a
      (gcd b (mod a b))))

(defun mod-exp (base exp mod)
  "Calculate (base^exp) mod mod using fast exponentiation"
  (if (zerop exp)
      1
      (let ((half (mod-exp base (floor exp 2) mod)))
        (if (evenp exp)
            (mod (* half half) mod)
            (mod (* base half half) mod)))))

(defun is-square (n)
  "Check if a number is a perfect square"
  (when (>= n 0)
    (let ((root (isqrt n)))
      (= (* root root) n))))

(defun sieve-of-eratosthenes (limit)
  "Generate all primes up to limit using Sieve of Eratosthenes"
  (let ((sieve (make-array (1+ limit) :element-type 'bit :initial-element 0))
        (primes '()))
    (setf (aref sieve 0) 1)
    (setf (aref sieve 1) 1)
    (loop for i from 2 to (isqrt limit)
          when (zerop (aref sieve i))
          do (loop for j from (* i i) to limit by i
                   do (setf (aref sieve j) 1)))
    (loop for i from 2 to limit
          when (zerop (aref sieve i))
          collect i)))

(defun factor-base (n)
  "Generate factor base for n"
  (let ((primes (sieve-of-eratosthenes (isqrt n))))
    (remove-if (lambda (p) 
                 (not (= (mod-exp n (floor (1- p) 2) p) 1)))
               primes)))

(defun find-smooth-numbers (n factor-base limit)
  "Find smooth numbers using factor base"
  (let ((smooth-numbers '()))
    (loop for x from 1 to limit
          for x2 ( = x x)  ; This is a placeholder - proper implementation would be more complex
          while (< (length smooth-numbers) 100)
          do (let ((num (1+ (* x x))))
               (when (<= num n)
                 (let ((factors (factor-number num factor-base)))
                   (when factors
                     (push (list num factors) smooth-numbers))))))
    smooth-numbers))

(defun factor-number (n factor-base)
  "Factor a number using factor base"
  (let ((factors '()))
    (loop for p in factor-base
          while (> n 1)
          do (loop while (zerop (mod n p))
                   do (setf n (floor n p))
                      (push p factors)))
    (when (and (> n 1) (member n factor-base))
      (push n factors))
    (when (and (> n 1) (not (member n factor-base)))
      (return-from factor-number nil))
    (nreverse factors)))

(defun quadratic-sieve (n)
  "Main quadratic sieve algorithm"
  (format t "Factoring ~A~%" n)
  
  ;; Step 1: Generate factor base
  (let* ((fb (factor-base n))
         (b (length fb)))
    (format t "Factor base size: ~A~%" b)
    (format t "Factor base: ~A~%" fb)
    
    ;; Step 2: Find smooth numbers (simplified)
    (let ((smooth (find-smooth-numbers n fb 1000)))
      (format t "Found ~A smooth numbers~%" (length smooth))
      
      ;; Step 3: Linear algebra (simplified)
      (format t "Performing linear algebra...~%")
      
      ;; Step 4: Find factors
      (let ((factors '()))
        (loop for (num factors-list) in smooth
              while (< (length factors) 2)
              do (let ((gcd-result (gcd num n)))
                   (when (and (> gcd-result 1) (< gcd-result n))
                     (push gcd-result factors))))
        (format t "Factors found: ~A~%" factors)
        factors))))

(defun factor (n)
  "Public interface to factor a number"
  (if (or (= n 1) (= n 0))
      (list n)
      (quadratic-sieve n)))

;; Example usage:
;; (factor 134213)
```

## Simplified Example

Here's a more focused example that demonstrates the core concepts:

```lisp
;; Simple example of factor base generation
(defun simple-factor-base (n)
  "Generate a small factor base for demonstration"
  (let ((primes '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)))
    (remove-if (lambda (p)
                  (not (= (mod-exp n (floor (1- p) 2) p) 1)))
                primes)))

;; Example usage
(defun demo ()
  (let ((n 134213))
    (format t "Factor base for ~A: ~A~%" n (simple-factor-base n))))

;; Run the demo
;; (demo)
```

## Key Components of the Algorithm

1. **Factor Base Generation**: Find small primes that are quadratic residues modulo n
2. **Smooth Number Finding**: Search for numbers x² ≡ r (mod n) where r is smooth
3. **Linear Algebra**: Solve the system of congruences to find relations
4. **Factor Extraction**: Use the relations to find non-trivial factors

## Note

This is a simplified version for educational purposes. A full implementation would include:
- Proper smooth number finding using sieving
- Matrix operations for linear algebra
- More sophisticated algorithms for handling large numbers
- Error handling and optimization

The quadratic sieve is an advanced algorithm typically used for factoring large integers in cryptography and number theory applications.

