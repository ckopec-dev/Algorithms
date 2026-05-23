# Fast Fourier Transform (FFT) in Lisp

Here's an implementation of the Cooley-Tukey FFT algorithm in Common Lisp:

```lisp
(defun fft (x)
  "Compute the Fast Fourier Transform of a complex vector x"
  (let ((n (length x)))
    (if (<= n 1)
        x
        (let ((even (fft (remove-if-not (lambda (i) (evenp i)) 
                                   (loop for i from 0 below n 
                                         collect (aref x i)))))
              (odd (fft (remove-if-not (lambda (i) (oddp i)) 
                                    (loop for i from 0 below n 
                                          collect (aref x i)))))
              (result (make-array n :element-type 'complex))
              (loop for k from 0 below (/ n 2)
                    for w = (exp (* #C(0 1) (/ (* 2 pi k) n)))
                    do (let ((t (aref even k))
                             (u (aref odd k)))
                         (setf (aref result k) (+ t (* w u)))
                         (setf (aref result (+ k (/ n 2))) (- t (* w u)))))
              result))))

;; Alternative implementation using bit-reversal and in-place computation
(defun fft-in-place (x)
  "In-place FFT implementation using bit-reversal"
  (let* ((n (length x))
         (bits (integer-length (1- n))))
    (if (<= n 1)
        x
        (progn
          ;; Bit-reversal permutation
          (loop for i from 0 below n
                for j = (loop for k from 0 below bits
                              for bit = (logand (ash i (- k)) 1)
                              sum (ash bit (* 2 (- bits k 1)))
                              while (< k bits))
                when (< i j)
                do (rotatef (aref x i) (aref x j)))
          
          ;; Cooley-Tukey FFT
          (loop for step from 1 to bits
                for m = (ash 1 step)
                for k from 0 below n by m
                for w-step = (exp (* #C(0 1) (/ (* 2 pi) m)))
                do (loop for j from 0 below (/ m 2)
                         for w = (exp (* #C(0 1) (/ (* 2 pi j) m)))
                         for u = (aref x (+ k j))
                         for v = (* w (aref x (+ k j (/ m 2))))
                         do (setf (aref x (+ k j)) (+ u v))
                            (setf (aref x (+ k j (/ m 2))) (- u v))))
          x))))

;; Example usage
(defun example-fft ()
  "Example demonstrating FFT usage"
  (let ((signal (make-array 8 :element-type 'complex
                           :initial-contents 
                           (list #C(1 0) #C(1 0) #C(1 0) #C(1 0)
                                 #C(0 0) #C(0 0) #C(0 0) #C(0 0)))))
    (format t "Input signal: ~A~%" signal)
    (let ((fft-result (fft signal)))
      (format t "FFT result: ~A~%" fft-result)
      fft-result)))

;; Simple test with known values
(defun test-fft ()
  "Test FFT with a simple sine wave"
  (let ((n 8)
        (freq 1))
    (let ((signal (make-array n :element-type 'complex)))
      (loop for i from 0 below n
            do (setf (aref signal i) 
                     (complex (sin (* 2 pi freq (/ i n))) 0)))
      (format t "Sine wave signal: ~A~%" signal)
      (let ((result (fft signal)))
        (format t "FFT result: ~A~%" result)))))

;; Run the example
(example-fft)
```

## Key Features of this Implementation:

1. **Recursive Approach**: Uses the Cooley-Tukey algorithm recursively
2. **Complex Number Support**: Handles complex numbers using Common Lisp's built-in complex type
3. **Bit-Reversal**: Includes an in-place version with bit-reversal permutation
4. **Efficient**: O(n log n) time complexity
5. **Memory Efficient**: The in-place version modifies the input array

## Usage Notes:

- The input array should have a length that is a power of 2
- Uses `#C(0 1)` for the imaginary unit i
- Returns complex numbers as results
- The in-place version modifies the original array

This implementation demonstrates the core principles of the FFT algorithm while maintaining readability in the Lisp programming language.

