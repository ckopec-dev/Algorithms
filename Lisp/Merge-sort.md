# Merge Sort in Lisp

Here's an implementation of the merge sort algorithm in Common Lisp:

```lisp
(defun merge-sort (lst)
  "Sort a list using the merge sort algorithm."
  (cond
    ;; Base case: empty list or single element list
    ((null lst) nil)
    ((null (rest lst)) lst)
    ;; Recursive case: split and merge
    (t
     (let ((mid (floor (length lst) 2)))
       (merge-lists 
        (merge-sort (subseq lst 0 mid))
        (merge-sort (subseq lst mid)))))))

(defun merge-lists (left right)
  "Merge two sorted lists into one sorted list."
  (cond
    ;; If left list is empty, return right list
    ((null left) right)
    ;; If right list is empty, return left list
    ((null right) left)
    ;; Compare first elements and merge
    ((<= (first left) (first right))
     (cons (first left)
           (merge-lists (rest left) right)))
    (t
     (cons (first right)
           (merge-lists left (rest right))))))

;; Example usage:
;; (merge-sort '(3 1 4 1 5 9 2 6 5 3 5))
;; Returns: (1 1 2 3 3 4 5 5 5 6 9)
```

## How it works:

1. **Base cases**: If the list is empty or has only one element, it's already sorted
2. **Split**: Divide the list into two halves at the midpoint
3. **Recursive sort**: Recursively sort both halves
4. **Merge**: Combine the two sorted halves into one sorted list

## Example execution:
```lisp
(merge-sort '(3 1 4 1 5 9 2 6))
;; Step 1: Split into (3 1 4 1) and (5 9 2 6)
;; Step 2: Recursively sort both halves
;; Step 3: Merge results to get (1 1 2 3 4 5 6 9)
```

The algorithm has O(n log n) time complexity and is stable, meaning equal elements maintain their relative order.

