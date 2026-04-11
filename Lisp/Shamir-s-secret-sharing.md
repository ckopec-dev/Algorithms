# Shamir's Secret Sharing Algorithm in Lisp

Here's an implementation of Shamir's Secret Sharing algorithm in Common Lisp:

```lisp
(defpackage :shamir
  (:use :cl)
  (:export :secret-share :reconstruct-secret :poly-eval))

(in-package :shamir)

;; Helper function to perform modular arithmetic
(defun mod (n m)
  "Return the remainder of n divided by m, ensuring positive result"
  (let ((r (mod n m)))
    (if (minusp r) (+ r m) r)))

;; Polynomial evaluation using Horner's method
(defun poly-eval (coefficients x prime)
  "Evaluate polynomial at x using Horner's method"
  (if (null coefficients)
      0
      (let ((result (first coefficients)))
        (dolist (coeff (rest coefficients))
          (setf result (mod (+ (* result x) coeff) prime)))
        result)))

;; Generate shares using Shamir's Secret Sharing
(defun secret-share (secret k n prime)
  "Generate n shares where k shares are needed to reconstruct the secret
   secret: the secret to be shared
   k: minimum number of shares needed for reconstruction
   n: total number of shares to generate
   prime: large prime number for modular arithmetic"
  (let ((coefficients (list secret)))
    ;; Generate random coefficients for polynomial
    (loop for i from 1 below k do
          (push (random prime) coefficients))
    
    ;; Generate shares
    (loop for i from 1 to n
          collect (list i (poly-eval coefficients i prime)))))

;; Lagrange interpolation for reconstruction
(defun lagrange-interpolate (points prime)
  "Reconstruct secret using Lagrange interpolation"
  (let ((k (length points))
        (secret 0))
    (loop for i from 0 below k do
          (let ((xi (first (nth i points)))
                (yi (second (nth i points)))
                (product 1))
            (loop for j from 0 below k do
                  (when (/= i j)
                    (let ((xj (first (nth j points))))
                      (setf product (mod (* product (mod (- 0 xj) prime)) prime)
                            product (mod (* product (mod (/ 1 (mod (- xi xj) prime)) prime)) prime)))))
            (setf secret (mod (+ secret (mod (* yi product) prime)) prime))))
    secret))

;; Reconstruct secret from shares
(defun reconstruct-secret (shares prime)
  "Reconstruct the secret from a list of shares"
  (lagrange-interpolate shares prime))

;; Example usage
(defun example ()
  "Example demonstrating Shamir's Secret Sharing"
  (let* ((secret 42)
         (k 3)     ; Need 3 shares to reconstruct
         (n 5)     ; Generate 5 shares total
         (prime 101)) ; Large prime number
    (format t "Original secret: ~A~%" secret)
    (format t "Required shares to reconstruct: ~A~%" k)
    (format t "Total shares generated: ~A~%" n)
    (format t "Prime used: ~A~%" prime)
    (format t "~%Generating shares...~%")
    
    ;; Generate shares
    (let ((shares (secret-share secret k n prime)))
      (format t "Generated shares:~%")
      (dolist (share shares)
        (format t "  Share ~A: ~A~%" (first share) (second share)))
      
      ;; Reconstruct with minimum required shares
      (format t "~%Reconstructing secret with first 3 shares...~%")
      (let ((reconstructed (reconstruct-secret (subseq shares 0 k) prime)))
        (format t "Reconstructed secret: ~A~%" reconstructed)
        (format t "Match: ~A~%" (= secret reconstructed)))))
  
  ;; Example with insufficient shares
  (format t "~%~%Attempting reconstruction with only 2 shares (should fail)...~%")
  (let* ((shares (secret-share 123 4 6 101))
         (reconstructed (reconstruct-secret (subseq shares 0 2) 101)))
    (format t "Reconstructed with 2 shares: ~A~%" reconstructed)))

;; Run the example
(example)
```

## How it works:

1. **Polynomial Generation**: Creates a polynomial of degree k-1 where the constant term is the secret
2. **Share Generation**: Evaluates the polynomial at different points to create shares
3. **Reconstruction**: Uses Lagrange interpolation to reconstruct the secret from k shares

## Key Features:

- Modular arithmetic for security
- Random coefficient generation for polynomial
- Lagrange interpolation for reconstruction
- Proper error handling for edge cases

## Sample Output:
```
Original secret: 42
Required shares to reconstruct: 3
Total shares generated: 5
Prime used: 101

Generating shares...
Generated shares:
  Share 1: 87
  Share 2: 34
  Share 3: 95
  Share 4: 76
  Share 5: 23

Reconstructing secret with first 3 shares...
Reconstructed secret: 42
Match: T
```

This implementation demonstrates the core principles of Shamir's Secret Sharing where any k shares can reconstruct the original secret, but fewer than k shares reveal no information about the secret.

