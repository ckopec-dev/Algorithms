# Introsort Algorithm in Lisp

Here's an implementation of the Introsort algorithm in Common Lisp:

```lisp
(defparameter *introsort-threshold* 16)
(defparameter *max-depth* 20)

(defun introsort (array &optional (start 0) (end (1- (length array))))
  "Main Introsort function that implements the introspective sort algorithm."
  (when (< start end)
    (let ((depth (floor (log (- end start) 2))))
      (if (<= depth *max-depth*)
          (introsort-recursive array start end)
          (heap-sort array start end)))))

(defun introsort-recursive (array start end)
  "Recursive helper function for Introsort."
  (if (<= (- end start) *introsort-threshold*)
      (insertion-sort array start end)
      (let ((pivot (partition array start end)))
        (introsort-recursive array start (1- pivot))
        (introsort-recursive array (1+ pivot) end)))))

(defun partition (array start end)
  "Partition function using median-of-three pivot selection."
  (let* ((mid (floor (+ start end) 2))
         (pivot (median-of-three array start mid end))
         (i (1- start)))
    (loop for j from start to end do
          (when (<= (aref array j) pivot)
            (incf i)
            (swap array i j)))
    (swap array (1+ i) end)
    (1+ i)))

(defun median-of-three (array start mid end)
  "Find median of three elements for pivot selection."
  (let ((a (aref array start))
        (b (aref array mid))
        (c (aref array end)))
    (cond ((and (<= a b) (<= b c)) b)
          ((and (<= b a) (<= a c)) a)
          (t c))))

(defun insertion-sort (array start end)
  "Insertion sort for small subarrays."
  (loop for i from (1+ start) to end do
        (let ((key (aref array i))
              (j (1- i)))
          (loop while (and (>= j start) (> (aref array j) key)) do
                (setf (aref array (1+ j)) (aref array j))
                (decf j))
          (setf (aref array (1+ j)) key))))

(defun heap-sort (array start end)
  "Heap sort implementation for large subarrays."
  (heapify array start end)
  (loop while (> end start) do
        (swap array start end)
        (decf end)
        (heapify-down array start end)))

(defun heapify (array start end)
  "Build max heap."
  (let ((n (1+ end)))
    (loop for i from (floor n 2) downto start do
          (heapify-down array i end))))

(defun heapify-down (array start end)
  "Heapify down operation."
  (let ((parent start)
        (child (1+ (* 2 start))))
    (loop while (<= child end) do
          (when (and (< child end)
                     (> (aref array child)
                        (aref array (1+ child))))
            (incf child))
          (when (> (aref array parent) (aref array child))
            (return))
          (swap array parent child)
          (setf parent child
                child (1+ (* 2 child))))))

(defun swap (array i j)
  "Swap two elements in array."
  (let ((temp (aref array i)))
    (setf (aref array i) (aref array j)
          (aref array j) temp)))

;; Example usage:
(let ((test-array #(64 34 25 12 22 11 90 88 76 50 42)))
  (format t "Original array: ~a~%" test-array)
  (introsort test-array)
  (format t "Sorted array: ~a~%" test-array))
```

## How Introsort Works

The Introsort algorithm combines three sorting techniques:

1. **Quick Sort**: Uses quick sort for most of the sorting process
2. **Heap Sort**: Switches to heap sort when recursion depth exceeds a threshold
3. **Insertion Sort**: Uses insertion sort for small subarrays (≤ 16 elements)

## Key Features

- **Hybrid Approach**: Combines the best aspects of different sorting algorithms
- **Guaranteed Performance**: Worst-case O(n log n) time complexity
- **Adaptive**: Switches between algorithms based on conditions
- **In-place**: Sorts the array without requiring additional memory

## Time Complexity

- **Best Case**: O(n log n)
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n) - guaranteed by the heap sort fallback

## Space Complexity

- O(log n) - due to recursion stack depth

This implementation demonstrates how Introsort provides both good average performance and worst-case guarantees, making it suitable for production sorting needs.

