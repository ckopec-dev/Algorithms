```lisp
(defun bogo-sort (lst)
  "BogoSort algorithm implementation in Lisp"
  (if (null lst)
      nil
    (loop while (not (sorted-p lst))
          do (setf lst (shuffle lst)))
    lst))

(defun sorted-p (lst)
  "Check if list is sorted in ascending order"
  (cond
    ((null lst) t)
    ((null (cdr lst)) t)
    ((<= (car lst) (cadr lst))
     (sorted-p (cdr lst)))
    (t nil)))

(defun shuffle (lst)
  "Randomly shuffle elements in list"
  (if (null lst)
      nil
    (let ((shuffled '()))
      (loop for i from 1 to (length lst)
            do (let ((random-index (random (length lst))))
                 (push (nth random-index lst) shuffled)))
      (reverse shuffled))))

;; Example usage:
;; (bogo-sort '(3 1 4 1 5 9 2 6))
;; (bogo-sort '(5 2 8 1 9))
```

This implementation demonstrates the BogoSort algorithm in Lisp:

1. **`bogo-sort`**: Main function that repeatedly shuffles the list until it's sorted
2. **`sorted-p`**: Helper function to check if a list is in ascending order
3. **`shuffle`**: Helper function to randomly rearrange list elements

The algorithm works by:
- Checking if the list is sorted
- If not sorted, randomly shuffling the elements
- Repeating until the list becomes sorted
- Returning the sorted list

Note: BogoSort has an average time complexity of O(n!) and should only be used for educational purposes.

