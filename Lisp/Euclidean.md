# Euclidean Algorithm in Lisp

Here's an implementation of the Euclidean algorithm to find the Greatest Common Divisor (GCD) in Lisp:

## Recursive Implementation

```lisp
(defun gcd (a b)
  "Calculate the GCD of two numbers using Euclidean algorithm"
  (if (= b 0)
      a
      (gcd b (mod a b))))

;; Example usage:
(gcd 48 18)   ; Returns 6
(gcd 100 25)  ; Returns 25
(gcd 17 13)   ; Returns 1
```

## Iterative Implementation

```lisp
(defun gcd-iterative (a b)
  "Calculate GCD iteratively using Euclidean algorithm"
  (loop while (not (= b 0))
        do (let ((temp b))
             (setf b (mod a b))
             (setf a temp)))
  a)

;; Example usage:
(gcd-iterative 48 18)   ; Returns 6
(gcd-iterative 100 25)  ; Returns 25
```

## Step-by-step Example

For `gcd(48, 18)`:
1. `gcd(48, 18)` → `gcd(18, 48 mod 18)` → `gcd(18, 12)`
2. `gcd(18, 12)` → `gcd(12, 18 mod 12)` → `gcd(12, 6)`
3. `gcd(12, 6)` → `gcd(6, 12 mod 6)` → `gcd(6, 0)`
4. `gcd(6, 0)` → `6`

The algorithm works by repeatedly replacing the larger number with the remainder of dividing the larger by the smaller number until one of them becomes zero.

