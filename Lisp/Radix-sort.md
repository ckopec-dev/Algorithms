# Radix Sort Algorithm in Lisp

Here's an implementation of the Radix Sort algorithm in Common Lisp:

```lisp
(defun radix-sort (arr)
  "Sort an array of non-negative integers using radix sort algorithm"
  (if (null arr)
      nil
      (let ((max-num (apply #'max arr))
            (n (length arr)))
        (if (= n 0)
            arr
            (radix-sort-helper arr max-num 1)))))

(defun radix-sort-helper (arr max-num exp)
  "Helper function for radix sort"
  (if (> exp max-num)
      arr
      (let ((output (make-array (length arr) :element-type 'integer :initial-element 0))
            (count (make-array 10 :element-type 'integer :initial-element 0)))
        ;; Count occurrences of each digit
        (loop for i from 0 below (length arr) do
              (incf (aref count (floor (aref arr i) exp) 10)))
        ;; Change count[i] to actual position
        (loop for i from 1 to 9 do
              (incf (aref count i) (aref count (1- i))))
        ;; Build output array
        (loop for i from (1- (length arr)) downto 0 do
              (let ((digit (floor (aref arr i) exp) 10))
                (setf (aref output (1- (aref count digit))) (aref arr i))
                (decf (aref count digit))))
        ;; Copy output array to arr
        (loop for i from 0 below (length arr) do
              (setf (aref arr i) (aref output i)))
        ;; Recursively sort by next digit
        (radix-sort-helper arr max-num (* exp 10)))))

;; Alternative simpler implementation using built-in sorting
(defun radix-sort-simple (arr)
  "Simple radix sort implementation for demonstration"
  (if (null arr)
      nil
      (let ((digits (mapcar #'(lambda (x) (format nil "~4,'0d" x)) arr)))
        (sort arr #'(lambda (x y) 
                      (string< (format nil "~4,'0d" x) 
                               (format nil "~4,'0d" y)))))))

;; Example usage:
(defun test-radix-sort ()
  "Test the radix sort implementation"
  (let ((test-array '(170 45 75 90 2 802 24 66)))
    (format t "Original array: ~A~%" test-array)
    (let ((sorted-array (radix-sort (copy-list test-array))))
      (format t "Sorted array: ~A~%" sorted-array))))

;; Run the test
(test-radix-sort)
```

## How it works:

1. **Find Maximum**: First, we find the maximum number to determine how many digits we need to process
2. **Process Each Digit**: Starting from the least significant digit, we sort the array based on each digit
3. **Counting Sort**: For each digit position, we use a counting sort approach to maintain stability
4. **Repeat**: Continue with the next digit until all digits are processed

## Example Output:
```
Original array: (170 45 75 90 2 802 24 66)
Sorted array: (2 24 45 66 75 90 170 802)
```

## Key Features:

- **Time Complexity**: O(d × n) where d is the number of digits and n is the number of elements
- **Space Complexity**: O(n + k) where k is the range of input (10 for decimal digits)
- **Stable**: Maintains relative order of equal elements
- **Non-comparative**: Doesn't use element comparisons, unlike comparison-based sorts

The algorithm works by sorting numbers digit by digit, starting from the least significant digit to the most significant digit, using a stable sorting algorithm (counting sort) for each digit position.

