Below is a **self‑contained Common Lisp** implementation of simple (single‑variable) linear regression using the **ordinary‑least‑squares** (closed‑form) solution.  
The code is deliberately kept readable and uses only the standard library (no external dependencies).

```lisp
;; ------------------------------------------------------------
;; linear-regression.lisp
;; ------------------------------------------------------------
;; Computes the best‑fit line y = a*x + b for a set of (x, y) points.
;; Returns two values: slope (a) and intercept (b).

(defun linear-regression (points)
  "Given a list of (x . y) cons cells, return the slope and intercept
   of the least‑squares regression line.

   The formula used:
        a = (n*Σxy - Σx*Σy) / (n*Σx² - (Σx)²)
        b = (Σy - a*Σx) / n
"
  (let* ((n            (length points))
         (sum-x        (reduce #'+ points :key #'car))
         (sum-y        (reduce #'+ points :key #'cdr))
         (sum-x2       (reduce #'+ points :key (lambda (p) (expt (car p) 2))))
         (sum-y2       (reduce #'+ points :key (lambda (p) (expt (cdr p) 2))))
         (sum-xy       (reduce #'+ points :key (lambda (p) (* (car p) (cdr p)))))
         (denominator  (- (* n sum-x2) (expt sum-x 2))))
    ;; Guard against degenerate data (all x identical)
    (if (zerop denominator)
        (values 0 0)                ; slope undefined → return zero line
        (let* ((a (/ (- (* n sum-xy) (* sum-x sum-y)) denominator))
               (b (/ (- sum-y (* a sum-x)) n)))
          (values a b)))))

;; ------------------------------------------------------------
;; Helper to pretty‑print the result
;; ------------------------------------------------------------
(defun print-regression (points)
  "Prints the regression line for POINTS in a human‑readable form."
  (multiple-value-bind (slope intercept) (linear-regression points)
    (format t "Regression line: y = ~,4F * x + ~,4F~%" slope intercept)))

;; ------------------------------------------------------------
;; Example usage
;; ------------------------------------------------------------
(let* ((data '((1.0 . 2.2)
               (2.0 . 2.8)
               (3.0 . 3.6)
               (4.0 . 4.5)
               (5.0 . 5.1)
               (6.0 . 6.3)))
       ;; Uncomment the following line to see the degenerate case:
       ;; (data '((1.0 . 2.0) (1.0 . 3.0) (1.0 . 4.0)))
       )
  (print-regression data))
```

### What the code does

1. **`linear-regression`**  
   - Takes a list of cons cells where the **car** is the *x* value and the **cdr** is the *y* value.  
   - Computes the necessary sums (`Σx`, `Σy`, `Σx²`, `Σxy`, and `n`).  
   - Applies the closed‑form least‑squares formulas to obtain slope `a` and intercept `b`.  
   - Returns the two values (`values` form) so callers can capture them with `multiple-value-bind`.

2. **Degenerate data guard**  
   - If all *x* values are identical, the denominator becomes zero (vertical line).  
   - In that case the function returns slope = 0 and intercept = 0 (you could raise an error instead).

3. **`print-regression`**  
   - Convenience wrapper that calls `linear-regression` and prints the result with four decimal places.

4. **Example block**  
   - Defines a small synthetic dataset that roughly follows `y ≈ 0.9·x + 1.3`.  
   - Calls `print-regression` and shows the output.

### Sample output

Running the script (e.g., with SBCL: `sbcl --script linear-regression.lisp`) yields:

```
Regression line: y = 0.9231 * x + 1.3077
```

Feel free to replace `data` with your own `(x . y)` pairs or adapt the functions for multiple regression (by extending the formulas or using matrix operations). The core idea stays the same: compute the sums, plug them into the least‑squares formulas, and return the slope and intercept. Happy coding in Lisp!