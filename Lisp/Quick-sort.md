# Quick Sort in Lisp

Here's an implementation of the Quick Sort algorithm in Lisp:

```lisp
(defun quick-sort (lst)
  "Sort a list using the quick sort algorithm."
  (cond
    ;; Base case: empty list or single element
    ((null lst) nil)
    ((null (rest lst)) lst)
    ;; Recursive case
    (t
     (let ((pivot (first lst))
           (rest-list (rest lst)))
       ;; Partition the list
       (let ((smaller (remove-if (lambda (x) (>= x pivot)) rest-list))
             (larger (remove-if (lambda (x) (< x pivot)) rest-list)))
         ;; Recursively sort and combine
         (append (quick-sort smaller)
                 (list pivot)
                 (quick-sort larger)))))))

;; Alternative implementation using partitioning with two lists
(defun quick-sort-alt (lst)
  "Alternative quick sort implementation."
  (if (null lst)
      nil
      (let ((pivot (first lst))
            (rest-list (rest lst)))
        (let ((smaller (remove-if (lambda (x) (>= x pivot)) rest-list))
              (larger (remove-if (lambda (x) (< x pivot)) rest-list)))
          (append (quick-sort-alt smaller)
                  (list pivot)
                  (quick-sort-alt larger))))))

;; Example usage:
;; (quick-sort '(64 34 25 12 22 11 90))
;; => (11 12 22 25 34 64 90)

;; (quick-sort '(5 2 8 1 9))
;; => (1 2 5 8 9)
```

## How it works:

1. **Base Case**: If the list is empty or has only one element, it's already sorted
2. **Pivot Selection**: Choose the first element as the pivot
3. **Partitioning**: Separate the remaining elements into two groups:
   - Elements smaller than the pivot
   - Elements greater than or equal to the pivot
4. **Recursion**: Recursively sort both partitions
5. **Combination**: Combine the sorted smaller elements, pivot, and sorted larger elements

## Time Complexity:
- **Best/Average Case**: O(n log n)
- **Worst Case**: O(n²) - when pivot is always the smallest or largest element

## Space Complexity:
- O(log n) - due to recursive call stack

The algorithm is efficient and widely used due to its good average-case performance and in-place sorting capability.

