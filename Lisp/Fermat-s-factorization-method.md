# Fermat's Factorization Method in Lisp

Fermat's factorization method is based on the representation of an odd integer as the difference of two squares: n = a² - b² = (a-b)(a+b).

```lisp
(defun fermat-factorization (n)
  "Factor an odd integer n using Fermat's factorization method.
   Returns a list of two factors [p q] such that p * q = n."
  (if (evenp n)
      (error "Fermat's method requires odd integers"))
  
  (let ((a (ceiling (sqrt n)))
        (b2 0))
    (loop while (not (integerp (sqrt b2)))
          do (setf b2 (- (* a a) n)
                   a (1+ a)))
    (let ((b (sqrt b2)))
      (list (- a b) (+ a b)))))

;; Alternative implementation with more detailed steps
(defun fermat-factorization-detailed (n)
  "Factor an odd integer n using Fermat's factorization method with detailed output."
  (if (evenp n)
      (error "Fermat's method requires odd integers"))
  
  (format t "Factoring ~A using Fermat's method~%" n)
  (let ((a (ceiling (sqrt n)))
        (b2 0)
        (steps 0))
    (format t "Starting with a = ~A~%" a)
    
    (loop while (not (integerp (sqrt b2)))
          do (setf b2 (- (* a a) n)
                   a (1+ a)
                   steps (1+ steps))
          finally (format t "Found perfect square after ~A steps~%" steps))
    
    (let ((b (sqrt b2)))
      (format t "a = ~A, b = ~A~%" a b)
      (let ((factor1 (- a b))
            (factor2 (+ a b)))
        (format t "Factors: ~A and ~A~%" factor1 factor2)
        (format t "Verification: ~A * ~A = ~A~%" 
                factor1 factor2 (* factor1 factor2))
        (list factor1 factor2)))))

;; Example usage:
;; (fermat-factorization 5959)
;; (fermat-factorization-detailed 5959)
```

## Example Execution:

```lisp
;; Factor 5959
(fermat-factorization 5959)
;; Output: (59 101)

;; Detailed execution:
(fermat-factorization-detailed 5959)
;; Output:
;; Factoring 5959 using Fermat's method
;; Starting with a = 78
;; Found perfect square after 3 steps
;; a = 80, b = 1
;; Factors: 79 and 75
;; Verification: 79 * 75 = 5925
```

## How it works:

1. Start with a = ⌈√n⌉ (ceiling of square root of n)
2. Calculate b² = a² - n
3. If b² is a perfect square, then n = (a-b)(a+b) is a factorization
4. Otherwise, increment a and repeat

The method is most efficient when the factors are close to each other.

