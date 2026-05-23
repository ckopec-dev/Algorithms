# Brent's Cycle Detection Algorithm in Lisp

Brent's cycle detection algorithm is an efficient method for detecting cycles in sequences. Here's an implementation in Common Lisp:

```lisp
(defun brent-cycle-detection (next-func start-value)
  "Detect cycle in a sequence using Brent's algorithm.
   next-func: function that computes next value from current value
   start-value: initial value to start the sequence"
  (let ((tortoise start-value)
        (hare start-value)
        (power 1)
        (length 0))
    (loop while (not (equal tortoise hare))
          do (progn
               (setf hare (funcall next-func hare))
               (incf length)
               (when (= length power)
                 (setf tortoise hare)
                 (setf power (* power 2))
                 (setf length 0))))
    (let ((tortoise start-value)
          (cycle-start 0))
      (loop while (not (equal tortoise hare))
            do (progn
                 (setf tortoise (funcall next-func tortoise))
                 (setf hare (funcall next-func hare))
                 (incf cycle-start)))
      (values cycle-start
              (funcall next-func tortoise)))))

;; Example usage:
;; Detect cycle in sequence: 1 -> 2 -> 3 -> 4 -> 2 -> 3 -> 4 -> 2 -> ...
(defun example-next (x)
  "Example function: 1->2, 2->3, 3->4, 4->2 (cycle)"
  (cond ((= x 1) 2)
        ((= x 2) 3)
        ((= x 3) 4)
        ((= x 4) 2)
        (t x)))

;; Test the algorithm
(let ((result (brent-cycle-detection #'example-next 1)))
  (format t "Cycle starts at position: ~A~%" (first result))
  (format t "Cycle length: ~A~%" (second result)))
```

## How it works:

1. **Two pointers**: Uses tortoise (slow) and hare (fast) pointers
2. **Power of 2**: The hare moves in powers of 2 (1, 2, 4, 8, ...)
3. **Cycle detection**: When tortoise and hare meet, a cycle is detected
4. **Cycle start**: The algorithm then finds the exact start of the cycle

## Key advantages:

- **Time complexity**: O(λ + μ) where λ is cycle length and μ is cycle start position
- **Space complexity**: O(1) - constant space
- **Efficient**: Fewer function calls than Floyd's algorithm
- **Deterministic**: Always finds the same result

The algorithm is particularly useful for detecting cycles in linked lists, pseudorandom number generators, and other sequential data structures.

