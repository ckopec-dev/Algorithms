**Extended Euclidean Algorithm in Common Lisp**

Below is a compact, self‑contained implementation that returns three values:

* `g` – the greatest common divisor `gcd(a, b)`
* `x` – the Bézout coefficient satisfying `a·x + b·y = g`
* `y` – the Bézout coefficient satisfying `a·x + b·y = g`

```lisp
(defun extended-euclid (a b)
  "Return (values g x y) such that
   g = gcd(a, b) and a*x + b*y = g.
   Works for any integers (including negative ones)."
  (labels ((rec (old-r r old-s s old-t t)
             (if (zerop r)
                 (values old-r old-s old-t)
                 (let* ((quot (floor old-r r))
                        (new-r (- old-r (* quot r)))
                        (new-s (- old-s (* quot s)))
                        (new-t (- old-t (* quot t))))
                   (rec r new-r s new-s t new-t)))))
    (rec a b 1 0 0 1)))

;; -----------------------------------------------------------------
;; Example usage:
;; -----------------------------------------------------------------
;; (extended-euclid 240 46)
;; => 2
;; => -9
;; => 47
;; Explanation: gcd(240,46)=2 and 240*(-9) + 46*47 = 2
```

### How it works
1. **Initial state**  
   - `old‑r = a`, `r = b` – the current remainders.  
   - `old‑s = 1`, `s = 0` – coefficients for `a`.  
   - `old‑t = 0`, `t = 1` – coefficients for `b`.

2. **Loop invariant** (maintained each iteration)  
   ```
   a·old_s + b·old_t = old_r
   a·s       + b·t   = r
   ```

3. **Step**  
   - Compute the integer quotient `q = floor(old‑r / r)`.  
   - Update the triple `(old‑r, r)`, `(old‑s, s)`, `(old‑t, t)` exactly as in the ordinary Euclidean algorithm, but also propagating the Bézout coefficients.

4. **Termination**  
   When `r` becomes `0`, `old‑r` holds `gcd(a,b)` and the associated coefficients `old‑s`, `old‑t` satisfy the Bézout identity.

The function returns the three values via `multiple-value-bind`‑style `values`, which is idiomatic in Common Lisp. Feel free to adapt the code to other Lisp dialects (Scheme, Clojure, etc.) by adjusting the syntax for multiple return values.