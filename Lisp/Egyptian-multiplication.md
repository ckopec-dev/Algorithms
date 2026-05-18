# Egyptian Multiplication Algorithm in Lisp

Egyptian multiplication (also known as binary multiplication or Russian peasant multiplication) is an ancient algorithm for multiplying two numbers using repeated doubling and addition.

## Algorithm Explanation

The algorithm works by:
1. Repeatedly halving one number (taking the floor) and doubling the other
2. When the halved number is odd, add the doubled number to the result
3. Continue until the halved number becomes 0

## Lisp Implementation

```lisp
(defun egyptian-multiply (a b)
  "Multiply two positive integers using Egyptian multiplication algorithm"
  (if (or (= a 0) (= b 0))
      0
      (let ((result 0))
        (loop while (> a 0)
              do (if (oddp a)
                     (setq result (+ result b)))
              (setq a (floor a 2))
              (setq b (* b 2)))
        result)))

;; Alternative recursive implementation
(defun egyptian-multiply-recursive (a b)
  "Recursive implementation of Egyptian multiplication"
  (cond
    ((or (= a 0) (= b 0)) 0)
    ((= a 1) b)
    (t (if (oddp a)
           (+ b (egyptian-multiply-recursive (floor a 2) (* b 2)))
           (egyptian-multiply-recursive (floor a 2) (* b 2))))))

;; Example usage
(egyptian-multiply 13 9)
;; Returns: 117

(egyptian-multiply-recursive 13 9)
;; Returns: 117

;; Step-by-step trace for 13 × 9:
;; a=13, b=9, result=0
;; 13 is odd: result = 0 + 9 = 9, a=6, b=18
;; 6 is even: result = 9, a=3, b=36  
;; 3 is odd: result = 9 + 36 = 45, a=1, b=72
;; 1 is odd: result = 45 + 72 = 117, a=0, b=144
;; Return: 117
```

## How it works with example (13 × 9):

```
Step 1: a=13 (odd) → result = 0 + 9 = 9, a=6, b=18
Step 2: a=6 (even) → result = 9, a=3, b=36  
Step 3: a=3 (odd) → result = 9 + 36 = 45, a=1, b=72
Step 4: a=1 (odd) → result = 45 + 72 = 117, a=0, b=144
Final result: 117
```

This algorithm is efficient with O(log n) time complexity and is particularly useful for understanding binary arithmetic concepts.

