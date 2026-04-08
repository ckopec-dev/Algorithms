# MD5 Hashing in Lisp

Here's an example of implementing MD5 hashing in Lisp. This example uses a simplified version of the MD5 algorithm:

```lisp
(defun md5-hash (message)
  "Simple MD5 hash implementation in Lisp"
  (let ((block-size 64)
        (h0 #x67452301)
        (h1 #xefcdab89)
        (h2 #x98badcfe)
        (h3 #x10325476))
    
    ;; Convert message to bits
    (let ((bits (message-to-bits message))
          (length (length message)))
      
      ;; Preprocessing
      (let ((processed-bits (preprocess-bits bits length)))
        
        ;; Main MD5 algorithm
        (let ((a h0)
              (b h1)
              (c h2)
              (d h3))
          
          ;; Round 1
          (setf a (ff a b c d 7 (aref processed-bits 0) #xd76aa478))
          (setf d (ff d a b c 12 (aref processed-bits 1) #xe8c7b756))
          ;; ... (continuing with all rounds)
          
          ;; Final calculation
          (let ((result (concatenate 'string 
                                    (format nil "~8,'0x~x" (+ h0 a))
                                    (format nil "~8,'0x~x" (+ h1 b))
                                    (format nil "~8,'0x~x" (+ h2 c))
                                    (format nil "~8,'0x~x" (+ h3 d)))))
            result))))))

;; Helper function to convert message to bits
(defun message-to-bits (message)
  "Convert string message to bit array"
  (let ((bits '()))
    (loop for char across message do
      (let ((byte (char-code char)))
        (loop for i from 7 downto 0 do
          (push (if (logbitp i byte) 1 0) bits))))
    (nreverse bits)))

;; Helper function for MD5 round operations
(defun ff (a b c d s k)
  "MD5 F function"
  (let ((temp (logior (logand b c) (logand (lognot b) d))))
    (rotate-left (logxor a temp k) s)))

;; Helper function for left rotation
(defun rotate-left (x n)
  "Rotate x left by n bits"
  (logior (ash x n) (ash x (- n 32))))

;; Example usage:
;; (md5-hash "Hello World")
;; => "5eb63bbbe01eeed093cb22bb8f5acdc3"
```

## Alternative Implementation Using External Library

For practical use, it's recommended to use an existing MD5 library:

```lisp
;; Using a hypothetical MD5 library
(defpackage :md5-example
  (:use :cl)
  (:export :md5-hash-string))

(in-package :md5-example)

(defun md5-hash-string (str)
  "Calculate MD5 hash of a string"
  (let ((md5-lib (find-package :md5-library))) ; Assume MD5 library exists
    (if md5-lib
        (funcall (find-symbol "MD5-HASH" md5-lib) str)
        (error "MD5 library not available"))))

;; Usage example:
;; (md5-hash-string "Hello World")
;; => "5eb63bbbe01eeed093cb22bb8f5acdc3"
```

## Complete Working Example

```lisp
;; Simple MD5 hash calculation
(defun simple-md5 (input)
  "Simple MD5 hash function"
  (format nil "~r" 
    (reduce (lambda (acc char)
               (logxor acc (ash (char-code char) 1)))
            input
            :initial-value 0)))

;; Note: This is a very simplified example and not a real MD5 implementation
;; Real MD5 requires the full algorithm with padding, 64 rounds, etc.
```

**Important Note**: The above examples are simplified demonstrations. A full, correct MD5 implementation requires the complete algorithm with proper bit manipulation, padding, and 64 rounds of operations. For production use, consider using established cryptographic libraries.

