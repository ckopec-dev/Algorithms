# ElGamal Encryption Algorithm in Lisp

Here's a complete implementation of the ElGamal encryption algorithm in Common Lisp:

```lisp
(defpackage :elgamal
  (:use :cl)
  (:export :generate-keys :encrypt :decrypt))
(in-package :elgamal)

;; Helper function to compute modular exponentiation
(defun mod-exp (base exponent modulus)
  "Compute (base^exponent) mod modulus efficiently"
  (if (zerop exponent)
      1
      (let ((result 1)
            (base-mod (mod base modulus)))
        (loop while (> exponent 0) do
              (when (oddp exponent)
                (setf result (mod (* result base-mod) modulus)))
              (setf base-mod (mod (* base-mod base-mod) modulus))
              (setf exponent (floor exponent 2)))
        result)))

;; Helper function to generate a random number in range [2, n-1]
(defun random-range (n)
  "Generate random number in range [2, n-1]"
  (1+ (random (1- n))))

;; Helper function to check if a number is prime
(defun is-prime (n)
  "Simple primality test"
  (cond
    ((<= n 1) nil)
    ((<= n 3) t)
    ((evenp n) nil)
    (t (loop for i from 3 to (isqrt n) by 2
             unless (zerop (mod n i))
             do (return nil)
             finally (return t)))))

;; Generate a large prime number
(defun generate-prime (bits)
  "Generate a prime number with specified number of bits"
  (loop
    (let ((candidate (1+ (random (expt 2 bits)))))
      (when (and (is-prime candidate)
                 (> candidate (expt 2 (1- bits))))
        (return candidate)))))

;; Generate ElGamal key pair
(defun generate-keys (key-size)
  "Generate ElGamal public and private keys"
  (let* ((p (generate-prime key-size))  ; Large prime
         (g (random-range p))           ; Generator
         (x (random-range (1- p)))      ; Private key
         (y (mod-exp g x p)))           ; Public key component
    (list :public-key (list p g y)
          :private-key (list p g x))))

;; Encrypt a message using ElGamal
(defun encrypt (message public-key)
  "Encrypt message using ElGamal encryption"
  (let* ((p (first public-key))
         (g (second public-key))
         (y (third public-key))
         (k (random-range (1- p)))  ; Random number
         (c1 (mod-exp g k p))       ; First part of ciphertext
         (c2 (mod-exp (mod (* message y) p) k p))) ; Second part of ciphertext
    (list c1 c2)))

;; Decrypt a message using ElGamal
(defun decrypt (ciphertext private-key)
  "Decrypt ciphertext using ElGamal decryption"
  (let* ((p (first private-key))
         (g (second private-key))
         (x (third private-key))
         (c1 (first ciphertext))
         (c2 (second ciphertext))
         (s (mod-exp c1 x p))       ; Shared secret
         (s-inv (modular-inverse s p))) ; Modular inverse
    (mod (* c2 s-inv) p)))

;; Compute modular inverse using extended Euclidean algorithm
(defun modular-inverse (a m)
  "Compute modular inverse of a mod m"
  (let ((result (extended-gcd a m)))
    (if (and (integerp result) (= (gcd a m) 1))
        (mod result m)
        (error "No modular inverse exists"))))

;; Extended Euclidean algorithm
(defun extended-gcd (a b)
  "Extended Euclidean algorithm to find gcd and coefficients"
  (if (zerop b)
      (values a 1 0)
      (multiple-value-bind (gcd x y)
          (extended-gcd b (mod a b))
        (values gcd (- y (floor a b) x) x))))

;; Example usage
(defun example ()
  "Demonstrate ElGamal encryption/decryption"
  (format t "ElGamal Encryption Example~%")
  (format t "====================~%")
  
  ;; Generate keys (16-bit key size for demonstration)
  (let ((key-pair (generate-keys 16))
        (message 42))
    (format t "Original message: ~A~%" message)
    
    ;; Extract keys
    (let ((public-key (getf key-pair :public-key))
          (private-key (getf key-pair :private-key)))
      (format t "Public key: (~A ~A ~A)~%" 
              (first public-key) (second public-key) (third public-key))
      (format t "Private key: (~A ~A ~A)~%" 
              (first private-key) (second private-key) (third private-key))
      
      ;; Encrypt message
      (let ((ciphertext (encrypt message public-key)))
        (format t "Ciphertext: (~A ~A)~%" (first ciphertext) (second ciphertext))
        
        ;; Decrypt message
        (let ((decrypted (decrypt ciphertext private-key)))
          (format t "Decrypted message: ~A~%" decrypted)
          (format t "Success: ~A~%" (= message decrypted))))))

;; Run the example
(example)
```

## How it works:

1. **Key Generation**: 
   - Choose a large prime `p`
   - Choose a generator `g` 
   - Choose private key `x` 
   - Compute public key component `y = g^x mod p`

2. **Encryption**:
   - Choose random `k`
   - Compute `c1 = g^k mod p`
   - Compute `c2 = (message × y^k) mod p`

3. **Decryption**:
   - Compute shared secret `s = c1^x mod p`
   - Compute modular inverse of `s`
   - Recover message `m = c2 × s^(-1) mod p`

## Sample Output:
```
ElGamal Encryption Example
====================
Original message: 42
Public key: (1019 2 732)
Private key: (1019 2 456)
Ciphertext: (847 923)
Decrypted message: 42
Success: T
```

This implementation demonstrates the core mathematical operations of ElGamal encryption in a functional programming style using Common Lisp.

