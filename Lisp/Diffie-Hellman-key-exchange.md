# Diffie-Hellman Key Exchange in Lisp

Here's an implementation of the Diffie-Hellman key exchange algorithm in Common Lisp:

```lisp
(defpackage :dh-key-exchange
  (:use :cl)
  (:export :diffie-hellman-key-exchange
          :generate-prime
          :modular-exponentiation))

(in-package :dh-key-exchange)

;; Generate a random prime number (simplified version)
(defun generate-prime (bits)
  "Generate a prime number with specified number of bits"
  (let ((candidate (1+ (random (expt 2 bits)))))
    (loop while (not (primep candidate))
          do (incf candidate))
    candidate))

;; Simple primality test (not suitable for very large numbers)
(defun primep (n)
  "Simple primality test"
  (cond
    ((<= n 1) nil)
    ((= n 2) t)
    ((evenp n) nil)
    (t (loop for i from 3 to (isqrt n) by 2
             never (zerop (mod n i))))))

;; Modular exponentiation (a^b mod m)
(defun modular-exponentiation (base exponent modulus)
  "Calculate (base^exponent) mod modulus efficiently"
  (if (zerop exponent)
      1
      (let ((result 1)
            (base-mod (mod base modulus)))
        (loop while (> exponent 0)
              do (when (oddp exponent)
                   (setf result (mod (* result base-mod) modulus)))
                  (setf base-mod (mod (* base-mod base-mod) modulus))
                  (setf exponent (floor exponent 2)))
        result)))

;; Diffie-Hellman key exchange
(defun diffie-hellman-key-exchange (prime generator private-key)
  "Perform Diffie-Hellman key exchange
   Returns: public key (g^private-key mod prime)"
  (modular-exponentiation generator private-key prime))

;; Complete key exchange simulation
(defun simulate-dh-exchange ()
  "Simulate a complete Diffie-Hellman key exchange"
  (format t "=== Diffie-Hellman Key Exchange ===~%")
  
  ;; Step 1: Agree on public parameters (these are shared)
  (let* ((prime 23)           ; Small prime for demonstration
         (generator 5)        ; Primitive root modulo prime
         (alice-private 6)    ; Alice's private key
         (bob-private 15))    ; Bob's private key
    (format t "Public parameters:~%")
    (format t "  Prime (p): ~A~%" prime)
    (format t "  Generator (g): ~A~%" generator)
    (format t "~%")
    
    ;; Step 2: Each party generates their public key
    (let ((alice-public (diffie-hellman-key-exchange prime generator alice-private))
          (bob-public (diffie-hellman-key-exchange prime generator bob-private)))
      (format t "Alice's private key: ~A~%" alice-private)
      (format t "Alice's public key: ~A~%" alice-public)
      (format t "Bob's private key: ~A~%" bob-private)
      (format t "Bob's public key: ~A~%" bob-public)
      (format t "~%")
      
      ;; Step 3: Each party computes the shared secret
      (let ((alice-secret (diffie-hellman-key-exchange prime bob-public alice-private))
            (bob-secret (diffie-hellman-key-exchange prime alice-public bob-private)))
        (format t "Shared secret computed by Alice: ~A~%" alice-secret)
        (format t "Shared secret computed by Bob: ~A~%" bob-secret)
        (format t "~%")
        
        ;; Verify they're the same
        (if (= alice-secret bob-secret)
            (format t "✓ Key exchange successful! Shared secret: ~A~%" alice-secret)
            (format t "✗ Key exchange failed!~%"))))))

;; Run the simulation
(simulate-dh-exchange)
```

## Example Output:
```
=== Diffie-Hellman Key Exchange ===
Public parameters:
  Prime (p): 23
  Generator (g): 5

Alice's private key: 6
Alice's public key: 8
Bob's private key: 15
Bob's public key: 19

Shared secret computed by Alice: 2
Shared secret computed by Bob: 2

✓ Key exchange successful! Shared secret: 2
```

## How it works:

1. **Public Parameters**: Both parties agree on a large prime number `p` and a generator `g`
2. **Private Keys**: Each party selects a private key (Alice: 6, Bob: 15)
3. **Public Keys**: Each party computes their public key using: `public_key = g^private_key mod p`
4. **Shared Secret**: Both parties compute the same shared secret using: `secret = other_public_key^private_key mod p`

The beauty of this algorithm is that even though the public keys and parameters are visible to anyone, the shared secret can only be computed by someone who knows one of the private keys.

