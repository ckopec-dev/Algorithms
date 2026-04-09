# Elliptic Curve Cryptography in Lisp

Here's an example implementation of Elliptic Curve Cryptography using the `cl-crypto` library in Common Lisp:

```lisp
(defpackage :ecc-example
  (:use :cl :cl-crypto))
(in-package :ecc-example)

;; Define an elliptic curve (secp256k1 - commonly used in Bitcoin)
(defparameter *curve* 
  (make-ec-curve
    :name "secp256k1"
    :p #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F
    :a 0
    :b 7
    :gx #x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
    :gy #x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8
    :n #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
    :h 1))

;; Generate a private key (random 256-bit number)
(defun generate-private-key ()
  (random-number (ec-curve-n *curve*)))

;; Calculate public key from private key
(defun public-key (private-key)
  (ec-point-multiply *curve* private-key (ec-curve-g *curve*)))

;; Simple signature generation (simplified version)
(defun sign-message (message private-key)
  (let* ((hash (sha256 message))
         (k (random-number (ec-curve-n *curve*)))
         (r (mod (ec-point-x (ec-point-multiply *curve* k (ec-curve-g *curve*))) 
                 (ec-curve-n *curve*)))
         (s (mod (* (mod (inv-mod k (ec-curve-n *curve*)) (ec-curve-n *curve*))
                    (+ (ec-point-x (ec-point-multiply *curve* private-key (ec-curve-g *curve*))) 
                       (* r (mod (sha256 message) (ec-curve-n *curve*)))))
               (ec-curve-n *curve*))))
    (list :r r :s s)))

;; Example usage
(defun run-ecc-example ()
  (format t "Elliptic Curve Cryptography Example~%")
  (format t "================================~%")
  
  ;; Generate private key
  (let ((private-key (generate-private-key)))
    (format t "Private Key: ~A~%" private-key)
    
    ;; Calculate public key
    (let ((public-key (public-key private-key)))
      (format t "Public Key X: ~A~%" (ec-point-x public-key))
      (format t "Public Key Y: ~A~%" (ec-point-y public-key))
      
      ;; Sign a message
      (let ((message "Hello, World!"))
        (format t "Message: ~A~%" message)
        (let ((signature (sign-message message private-key)))
          (format t "Signature R: ~A~%" (first signature))
          (format t "Signature S: ~A~%" (second signature)))))))
```

## Alternative Implementation with External Library

```lisp
;; Using cl-ec for elliptic curve operations
(defpackage :ecc-advanced
  (:use :cl :cl-ec :cl-crypto))
(in-package :ecc-advanced)

;; Create elliptic curve point operations
(defun ecc-point-add (point1 point2 curve)
  "Add two points on an elliptic curve"
  (ec-point-add curve point1 point2))

(defun ecc-point-multiply (scalar point curve)
  "Multiply a point by a scalar (scalar multiplication)"
  (ec-point-multiply curve scalar point))

;; Key generation example
(defun generate-keys ()
  (let* ((curve *secp256k1*)
         (private-key (random-number (ec-curve-n curve)))
         (public-key (ec-point-multiply curve private-key (ec-curve-g curve))))
    (format t "Private Key: ~A~%" private-key)
    (format t "Public Key X: ~A~%" (ec-point-x public-key))
    (format t "Public Key Y: ~A~%" (ec-point-y public-key))
    (values private-key public-key)))

;; Complete example
(defun complete-example ()
  (format t "=== ECC Key Generation Example ===~%")
  (let ((result (generate-keys)))
    (format t "Keys generated successfully!~%")
    result))
```

## Installation Requirements

To run this code, you'll need to install the following libraries:

```bash
# Using Quicklisp
(ql:quickload "cl-crypto")
(ql:quickload "cl-ec")
```

## Key Features Demonstrated

1. **Curve Definition**: Secp256k1 curve parameters
2. **Key Generation**: Private and public key creation
3. **Point Operations**: Elliptic curve point addition and scalar multiplication
4. **Signature Generation**: Basic ECDSA signature creation
5. **Security**: Uses cryptographically secure random number generation

This example shows the fundamental operations of ECC, including key generation, point arithmetic, and signature creation, which are essential components of modern cryptographic systems.

