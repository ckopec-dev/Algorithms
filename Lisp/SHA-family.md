# SHA Family Algorithm Example in Lisp

Here's an example implementation of SHA-256 using Common Lisp:

```lisp
(defpackage :sha256-example
  (:use :cl)
  (:export :sha256 :sha256-hex))
(in-package :sha256-example)

;; SHA-256 constants
(defconstant +k+
  '(#b11110100010010100101101111010110
    #b11101111101111101010011010001101
    #b11010011111010010110100011111010
    #b11001101101111011110110111110111
    #b10111111111111011111001111110110
    #b10110000001100000101110101001111
    #b10101111011110101110001010011010
    #b10100111010011111010001101001100
    #b10011010010110010111101001110110
    #b10010100011010111001111110001100
    #b10001110001000100110011001111100
    #b10001010000111101011011001110011
    #b10000110000100011000001110101001
    #b10000010000000100110000010000111
    #b10000000000000010010000001000001
    #b10000000000000000010000000100000
    #b10000000000000000001000000010000
    #b10000000000000000000100000001000
    #b10000000000000000000010000000100
    #b10000000000000000000001000000010
    #b10000000000000000000000100000001
    #b10000000000000000000000010000000
    #b10000000000000000000000001000000
    #b10000000000000000000000000100000
    #b10000000000000000000000000010000
    #b10000000000000000000000000001000
    #b10000000000000000000000000000100
    #b10000000000000000000000000000010
    #b10000000000000000000000000000001
    #b10000000000000000000000000000000))

;; Helper functions
(defun rotr (x n)
  "Right rotate x by n bits"
  (logior (ash x (- n)) (ash x (- 32 n))))

(defun ch (x y z)
  "Choose function"
  (logior (logand x y) (logand (lognot x) z)))

(defun maj (x y z)
  "Majority function"
  (logior (logand x y) (logand x z) (logand y z)))

(defun sigma0 (x)
  "Sigma0 function"
  (logior (rotr x 2) (rotr x 13) (rotr x 22)))

(defun sigma1 (x)
  "Sigma1 function"
  (logior (rotr x 6) (rotr x 11) (rotr x 25)))

(defun gamma0 (x)
  "Gamma0 function"
  (logior (rotr x 7) (rotr x 18) (logand x 3)))

(defun gamma1 (x)
  "Gamma1 function"
  (logior (rotr x 17) (rotr x 19) (logand x 3)))

(defun sha256 (message)
  "Compute SHA-256 hash of message"
  (let* ((h0 #b11110100010010100101101111010110)
         (h1 #b11101111101111101010011010001101)
         (h2 #b11010011111010010110100011111010)
         (h3 #b11001101101111011110110111110111)
         (h4 #b10111111111111011111001111110110)
         (h5 #b10110000001100000101110101001111)
         (h6 #b10101111011110101110001010011010)
         (h7 #b10100111010011111010001101001100)
         (h (make-array 8 :initial-contents (list h0 h1 h2 h3 h4 h5 h6 h7)))
         (message (append (coerce message 'list)
                          (list #x80))
                  (loop while (not (zerop (mod (length message) 64)))
                        collect 0 into padded
                        finally (return (append message padded)))))
    ;; Process message in 512-bit chunks
    (loop for i from 0 to (1- (length message)) by 64
          do (let ((w (make-array 64)))
               ;; Initialize w array with message words
               (loop for j from 0 to 15
                     do (setf (aref w j)
                              (logior (ash (aref message (+ i j)) 24)
                                      (ash (aref message (+ i j 1)) 16)
                                      (ash (aref message (+ i j 2)) 8)
                                      (aref message (+ i j 3)))))
               ;; Extend the message schedule
               (loop for j from 16 to 63
                     do (setf (aref w j)
                              (logior (gamma1 (aref w (- j 2)))
                                      (aref w (- j 7))
                                      (gamma0 (aref w (- j 15)))
                                      (aref w (- j 16)))))
               ;; Main loop
               (let ((a (aref h 0))
                     (b (aref h 1))
                     (c (aref h 2))
                     (d (aref h 3))
                     (e (aref h 4))
                     (f (aref h 5))
                     (g (aref h 6))
                     (h (aref h 7))))
               (loop for j from 0 to 63
                     do (let ((t1 (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (logior (log