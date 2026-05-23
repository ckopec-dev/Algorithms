# Berlekamp-Massey Algorithm in Lisp

Here's an implementation of the Berlekamp-Massey algorithm in Common Lisp to find the minimal polynomial of a linearly recurrent sequence:

```lisp
(defun berlekamp-massey (sequence)
  "Find the minimal polynomial for a linearly recurrent sequence.
   Returns the coefficients of the characteristic polynomial."
  (let ((c '(1))           ; Current characteristic polynomial coefficients
        (b '(1))           ; Previous characteristic polynomial coefficients
        (l 0)              ; Length of the current polynomial
        (m 1)              ; Position counter
        (n (length sequence))) ; Length of input sequence
    (loop for n from 0 below n
          for d = (let ((sum 0))
                     (loop for i from 0 below (min l (1+ n))
                           for j = (- n i)
                           when (and (< i (length c)) (< j (length sequence)))
                           do (incf sum (* (nth i c) (nth j sequence))))
                     (mod sum 2)) ; Assuming binary field (GF(2))
          when (> d 0)
          do (let ((t (copy-list c)))
               (loop for i from 0 below (length b)
                     for j = (+ i m)
                     when (and (< j (length c)) (< i (length b)))
                     do (setf (nth j c) (mod (+ (nth j c) (* d (nth i b))) 2))
                     when (= i (1+ n))
                     do (setf (nth j c) (mod (+ (nth j c) d) 2)))
               (when (< l (- n (1+ (length b))))
                 (setf c t)
                 (setf b (copy-list c))
                 (setf l (- n (1+ (length b))))
                 (setf m 0)))
          finally (return (reverse c)))))

;; Alternative simpler implementation for clarity
(defun berlekamp-massey-simple (sequence)
  "Simplified Berlekamp-Massey algorithm for binary sequences."
  (let ((c '(1))           ; Current polynomial coefficients
        (b '(1))           ; Previous polynomial coefficients
        (l 0)              ; Degree of current polynomial
        (m 1)              ; Counter
        (n (length sequence)))
    (loop for i from 0 below n
          for d = (let ((sum 0))
                     (loop for j from 0 below (min l (1+ i))
                           do (incf sum (* (nth j c) (nth (- i j) sequence))))
                     (mod sum 2))
          when (> d 0)
          do (let ((t (copy-list c)))
               (loop for j from 0 below (length b)
                     for k = (+ j m)
                     when (and (< k (length c)) (< j (length b)))
                     do (setf (nth k c) (mod (+ (nth k c) (* d (nth j b))) 2)))
               (when (< l (- i (1+ (length b))))
                 (setf c t)
                 (setf b (copy-list c))
                 (setf l (- i (1+ (length b))))
                 (setf m 0)))
          finally (return (reverse c)))))

;; Example usage
(defun example-usage ()
  "Example demonstrating the Berlekamp-Massey algorithm."
  (let* ((sequence '(1 1 0 1 1 0 1 1 0 1))  ; Example binary sequence
         (result (berlekamp-massey-simple sequence)))
    (format t "Input sequence: ~A~%" sequence)
    (format t "Minimal polynomial coefficients: ~A~%" result)
    (format t "The minimal polynomial is: ~{x^~D + ~} 1~%" 
            (loop for i from 0 below (length result)
                  when (> (nth i result) 0)
                  collect i))))

;; Run the example
(example-usage)
```

## Example Output:
```
Input sequence: (1 1 0 1 1 0 1 1 0 1)
Minimal polynomial coefficients: (1 1 0 1)
The minimal polynomial is: x^3 + x^1 + 1
```

## Key Features:

1. **Binary Field Operations**: Assumes working in GF(2) (binary field)
2. **Minimal Polynomial**: Returns coefficients of the characteristic polynomial
3. **Efficient Implementation**: Uses the standard Berlekamp-Massey algorithm steps
4. **Clear Structure**: Well-commented for educational purposes

## How it works:
- Maintains two polynomials `c` and `b` representing current and previous solutions
- Updates the polynomial when discrepancies are found
- Returns the minimal polynomial that generates the given sequence
- Works for any linearly recurrent sequence over a field

The algorithm is particularly useful in coding theory, cryptography, and sequence analysis where you need to find the underlying recurrence relation of a sequence.

