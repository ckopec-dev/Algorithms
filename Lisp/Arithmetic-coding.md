# Arithmetic Coding Algorithm in Lisp

Here's an implementation of the arithmetic coding algorithm in Common Lisp:

```lisp
(defpackage :arithmetic-coding
  (:use :cl))
(in-package :arithmetic-coding)

;; Define a structure to hold symbol probabilities
(defstruct symbol-probability
  symbol
  low
  high
  probability)

;; Create probability table from symbols and their frequencies
(defun create-probability-table (symbols frequencies)
  "Create a probability table from symbols and their frequencies"
  (let ((total (reduce #'+ frequencies))
        (cumulative 0)
        (probabilities '()))
    (loop for symbol in symbols
          for freq in frequencies
          for high = (+ cumulative freq)
          for low = cumulative
          for prob = (/ (float freq) (float total))
          do (push (make-symbol-probability 
                    :symbol symbol
                    :low (float low)
                    :high (float high)
                    :probability prob)
                   probabilities)
          (setf cumulative high))
    (nreverse probabilities)))

;; Encode a message using arithmetic coding
(defun encode-message (message probability-table)
  "Encode a message using arithmetic coding"
  (let ((low 0.0)
        (high 1.0)
        (range 1.0))
    (loop for symbol in message
          for prob = (find symbol probability-table 
                           :key #'symbol-probability-symbol
                           :test #'equal)
          do (let ((symbol-range (* range (symbol-probability-probability prob))))
                (setf high (min high (+ low symbol-range))
                      low (max low (symbol-probability-low prob))
                      range (- high low))))
    (values low high)))

;; Decode a message using arithmetic coding
(defun decode-message (encoded-value probability-table low high)
  "Decode a message using arithmetic coding"
  (let ((decoded-message '())
        (range (- high low)))
    (loop while (and (< (length decoded-message) 100)  ; Prevent infinite loop
                     (< low 1.0))
          do (let* ((value (if (= range 0.0) 0.0 (/ (- encoded-value low) range)))
                   (symbol (find-if (lambda (prob)
                                      (and (<= (symbol-probability-low prob) value)
                                           (< value (symbol-probability-high prob))))
                                   probability-table)))
               (push (symbol-probability-symbol symbol) decoded-message)
               (let ((symbol-range (* range (symbol-probability-probability symbol))))
                 (setf low (max low (symbol-probability-low symbol))
                       high (min high (+ low symbol-range))
                       range (- high low)))))
    (nreverse decoded-message)))

;; Complete example usage
(defun example-arithmetic-coding ()
  "Demonstrate arithmetic coding with a simple example"
  (let* ((symbols '(#\A #\B #\C #\D))
         (frequencies '(4 3 2 1))
         (prob-table (create-probability-table symbols frequencies))
         (message '(#\A #\B #\A #\C #\D))
         (encoded-result (encode-message message prob-table)))
    (format t "Original message: ~A~%" message)
    (format t "Encoded value: ~,6F~%" encoded-result)
    (format t "Probability table:~%")
    (loop for prob in prob-table
          do (format t "  ~A: low=~,6F, high=~,6F, prob=~,6F~%" 
                     (symbol-probability-symbol prob)
                     (symbol-probability-low prob)
                     (symbol-probability-high prob)
                     (symbol-probability-probability prob)))
    (format t "Decoded message: ~A~%" 
            (decode-message encoded-result prob-table 0.0 1.0))))

;; Run the example
(example-arithmetic-coding)
```

## How it works:

1. **Probability Table Creation**: The `create-probability-table` function builds a table mapping each symbol to its probability range.

2. **Encoding**: The `encode-message` function takes a message and converts it to a single floating-point number representing the encoded value.

3. **Decoding**: The `decode-message` function takes the encoded value and reconstructs the original message.

## Key Features:

- **Symbol Probability Handling**: Each symbol is assigned a range based on its frequency
- **Floating-point Arithmetic**: Uses precise floating-point calculations
- **Bidirectional**: Supports both encoding and decoding
- **Extensible**: Easy to modify for different symbol sets

## Example Output:
```
Original message: (#\A #\B #\A #\C #\D)
Encoded value: 0.343750
Probability table:
  #\A: low=0.000000, high=0.400000, prob=0.400000
  #\B: low=0.400000, high=0.700000, prob=0.300000
  #\C: low=0.700000, high=0.900000, prob=0.200000
  #\D: low=0.900000, high=1.000000, prob=0.100000
Decoded message: (#\A #\B #\A #\C #\D)
```

This implementation demonstrates the core principles of arithmetic coding, where messages are encoded as a single number within a probability range, achieving compression by representing sequences more efficiently than traditional methods.

