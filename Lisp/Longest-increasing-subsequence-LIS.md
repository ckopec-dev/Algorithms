# Longest Increasing Subsequence (LIS) in Lisp

Here's an implementation of the LIS algorithm using dynamic programming in Common Lisp:

```lisp
(defun longest-increasing-subsequence (arr)
  "Find the length of the longest increasing subsequence in array arr."
  (if (null arr)
      0
      (let ((n (length arr))
            (lis (make-array (length arr) :element-type 'fixnum :initial-element 1)))
        ;; Fill the lis array using dynamic programming
        (loop for i from 1 below n do
              (loop for j from 0 below i do
                    (when (and (< (aref arr j) (aref arr i))
                               (> (+ (aref lis j) 1) (aref lis i)))
                      (setf (aref lis i) (+ (aref lis j) 1)))))
        ;; Find maximum value in lis array
        (loop for i from 0 below n maximizing (aref lis i)))))

(defun longest-increasing-subsequence-with-sequence (arr)
  "Find the actual longest increasing subsequence (not just length)."
  (if (null arr)
      nil
      (let* ((n (length arr))
             (lis (make-array n :element-type 'fixnum :initial-element 1))
             (prev (make-array n :element-type 'fixnum :initial-element -1))
             (max-len 1)
             (max-index 0))
        ;; Fill lis and prev arrays
        (loop for i from 1 below n do
              (loop for j from 0 below i do
                    (when (and (< (aref arr j) (aref arr i))
                               (> (+ (aref lis j) 1) (aref lis i)))
                      (setf (aref lis i) (+ (aref lis j) 1))
                      (setf (aref prev i) j))))
        ;; Find the index with maximum length
        (loop for i from 0 below n do
              (when (> (aref lis i) max-len)
                (setf max-len (aref lis i))
                (setf max-index i)))
        ;; Reconstruct the actual subsequence
        (let ((result '()))
          (loop while (>= max-index 0) do
                (push (aref arr max-index) result)
                (setf max-index (aref prev max-index)))
          (nreverse result)))))

;; Example usage:
;; (longest-increasing-subsequence '(10 22 9 33 21 50 41 60 80))
;; Returns: 6 (subsequence: 10 22 33 50 60 80)

;; (longest-increasing-subsequence-with-sequence '(10 22 9 33 21 50 41 60 80))
;; Returns: (10 22 33 50 60 80)
```

## How it works:

1. **Basic LIS Algorithm**: 
   - Uses dynamic programming approach
   - Creates an array `lis` where `lis[i]` represents the length of LIS ending at index `i`
   - For each element, checks all previous elements smaller than current element
   - Updates the LIS length accordingly

2. **Enhanced Version**:
   - Also tracks the actual subsequence using a `prev` array
   - Reconstructs the actual longest increasing subsequence

## Time and Space Complexity:
- **Time Complexity**: O(n²) where n is the length of the array
- **Space Complexity**: O(n) for the auxiliary arrays

## Example Run:
```lisp
(longest-increasing-subsequence '(10 22 9 33 21 50 41 60 80))
;; Returns: 6

(longest-increasing-subsequence-with-sequence '(10 22 9 33 21 50 41 60 80))
;; Returns: (10 22 33 50 60 80)
```

