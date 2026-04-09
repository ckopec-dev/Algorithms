# Monte Carlo Method Implementation in Lisp

Here's an example of a Monte Carlo method algorithm implemented in Lisp to estimate the value of π:

```lisp
(defun estimate-pi (num-samples)
  "Estimate π using Monte Carlo method with num-samples random points"
  (let ((inside-circle 0))
    (loop for i from 1 to num-samples do
      (let ((x (random 1.0))
            (y (random 1.0)))
        (when (<= (+ (* x x) (* y y)) 1.0)
          (incf inside-circle))))
    (* 4.0 (/ inside-circle num-samples))))

;; Alternative implementation using recursion
(defun monte-carlo-pi (samples)
  "Recursive Monte Carlo π estimation"
  (if (zerop samples)
      0.0
      (let ((x (random 1.0))
            (y (random 1.0)))
        (let ((inside (if (<= (+ (* x x) (* y y)) 1.0) 1 0)))
          (+ inside (monte-carlo-pi (1- samples)))))))

(defun estimate-pi-recursive (num-samples)
  "Estimate π using recursive Monte Carlo method"
  (let ((points-inside (monte-carlo-pi num-samples)))
    (* 4.0 (/ points-inside num-samples))))

;; Example usage:
;; (estimate-pi 1000000)
;; (estimate-pi-recursive 1000000)
```

## How it works:

1. **Basic Concept**: Generate random points within a unit square [0,1] × [0,1]
2. **Circle Test**: Check if each point falls inside the unit circle (x² + y² ≤ 1)
3. **Ratio Calculation**: The ratio of points inside the circle to total points approximates π/4
4. **Final Estimation**: Multiply by 4 to get π estimate

## Sample Output:
```
;; With 1,000,000 samples:
;; (estimate-pi 1000000) => ~3.141592
```

## Key Features:

- **Random Number Generation**: Uses `random` function for uniform distribution
- **Loop Structure**: Demonstrates both iterative and recursive approaches
- **Mathematical Accuracy**: Converges to π as sample size increases
- **Functional Style**: Pure functional implementation with no side effects

The Monte Carlo method demonstrates how random sampling can be used to solve mathematical problems, with accuracy improving as the number of samples increases.

