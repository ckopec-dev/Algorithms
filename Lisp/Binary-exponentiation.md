# Binary Exponentiation Algorithm in Lisp

Here's an implementation of the binary exponentiation algorithm in Lisp:

```lisp
(defun binary-exponentiation (base exponent)
  "Calculate base raised to the power of exponent using binary exponentiation"
  (cond
    ;; Base case: anything to the power of 0 is 1
    ((= exponent 0) 1)
    
    ;; Base case: anything to the power of 1 is itself
    ((= exponent 1) base)
    
    ;; If exponent is even: base^(2k) = (base^2)^k
    ((evenp exponent)
     (let ((half-result (binary-exponentiation base (/ exponent 2))))
       (* half-result half-result)))
    
    ;; If exponent is odd: base^(2k+1) = base * base^(2k)
    (t
     (* base (binary-exponentiation base (- exponent 1))))))

;; Alternative iterative version
(defun binary-exponentiation-iterative (base exponent)
  "Calculate base raised to the power of exponent using iterative binary exponentiation"
  (if (= exponent 0)
      1
      (let ((result 1)
            (current-base base)
            (current-exp exponent))
        (loop while (> current-exp 0) do
              (if (oddp current-exp)
                  (setf result (* result current-base)))
              (setf current-base (* current-base current-base))
              (setf current-exp (floor current-exp 2)))
        result)))

;; Example usage:
;; (binary-exponentiation 2 10)  ; Returns 1024
;; (binary-exponentiation 3 4)   ; Returns 81
;; (binary-exponentiation 5 0)   ; Returns 1
```

## How it works:

The binary exponentiation algorithm uses the binary representation of the exponent to reduce the number of multiplications needed. Instead of multiplying the base by itself `n` times for `base^n`, it uses the property that:

- If `n` is even: `base^n = (base^(n/2))^2`
- If `n` is odd: `base^n = base × base^(n-1)`

This reduces the time complexity from O(n) to O(log n).

## Example trace for `binary-exponentiation(2, 10)`:

```
2^10 = (2^5)^2
     = (2 × 2^4)^2
     = (2 × (2^2)^2)^2
     = (2 × 4^2)^2
     = (2 × 16)^2
     = 32^2
     = 1024
```

