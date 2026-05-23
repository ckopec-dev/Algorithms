# Bucket Sort Algorithm in Lisp

Here's an implementation of the Bucket Sort algorithm in Common Lisp:

```lisp
(defun bucket-sort (array &optional (bucket-count 10))
  "Sort an array using bucket sort algorithm.
   Assumes array contains numbers between 0 and 1."
  (if (null array)
      nil
    (let ((buckets (make-array bucket-count :element-type 'list :initial-element nil))
          (result '()))
      ;; Distribute elements into buckets
      (dolist (element array)
        (let ((bucket-index (floor (* element bucket-count))))
          ;; Handle edge case where element is 1.0
          (when (= bucket-index bucket-count)
            (setf bucket-index (1- bucket-count)))
          (push element (aref buckets bucket-index)))))
      
      ;; Sort each bucket and collect results
      (loop for i from 0 below bucket-count do
            (let ((bucket (aref buckets i)))
              (when bucket
                (setf bucket (sort bucket '<))
                (setf result (append result bucket)))))
      
      result)))

;; Alternative implementation with more explicit bucket handling
(defun bucket-sort-verbose (array &optional (bucket-count 10))
  "Verbose version of bucket sort with detailed steps."
  (if (null array)
      nil
    (let ((buckets (make-array bucket-count :element-type 'list :initial-element nil)))
      ;; Step 1: Distribute elements into buckets
      (format t "Distributing elements into ~A buckets~%" bucket-count)
      (dolist (element array)
        (let ((bucket-index (floor (* element bucket-count))))
          ;; Handle the case where element equals 1.0
          (when (= bucket-index bucket-count)
            (setf bucket-index (1- bucket-count)))
          (push element (aref buckets bucket-index))
          (format t "  Element ~A goes to bucket ~A~%" element bucket-index))))
      
      ;; Step 2: Sort each bucket
      (format t "Sorting individual buckets~%")
      (loop for i from 0 below bucket-count do
            (let ((bucket (aref buckets i)))
              (when bucket
                (setf (aref buckets i) (sort bucket '<))
                (format t "  Bucket ~A: ~A~%" i bucket)))))
      
      ;; Step 3: Collect sorted elements
      (let ((result '()))
        (loop for i from 0 below bucket-count do
              (let ((bucket (aref buckets i)))
                (when bucket
                  (setf result (append result bucket)))))
        (format t "Final sorted result: ~A~%" result)
        result))))

;; Example usage
(defun test-bucket-sort ()
  "Test the bucket sort implementation."
  (let ((test-array '(0.42 0.32 0.33 0.52 0.37 0.47 0.51)))
    (format t "Original array: ~A~%" test-array)
    (let ((sorted-array (bucket-sort test-array)))
      (format t "Sorted array: ~A~%" sorted-array))
    (format t "~%"))
  
  ;; Test with verbose version
  (let ((test-array2 '(0.89 0.12 0.74 0.23 0.65 0.34 0.98 0.01)))
    (format t "Testing with array: ~A~%" test-array2)
    (bucket-sort-verbose test-array2)))

;; Run the test
(test-bucket-sort)
```

## How it works:

1. **Bucket Creation**: Creates `bucket-count` empty buckets (arrays)
2. **Distribution**: Places each element into the appropriate bucket based on its value
3. **Sorting**: Sorts each individual bucket
4. **Collection**: Concatenates all sorted buckets to form the final sorted array

## Example Output:
```
Original array: (0.42 0.32 0.33 0.52 0.37 0.47 0.51)
Sorted array: (0.32 0.33 0.37 0.42 0.47 0.51 0.52)

Testing with array: (0.89 0.12 0.74 0.23 0.65 0.34 0.98 0.01)
Distributing elements into 10 buckets
  Element 0.89 goes to bucket 8
  Element 0.12 goes to bucket 1
  Element 0.74 goes to bucket 7
  Element 0.23 goes to bucket 2
  Element 0.65 goes to bucket 6
  Element 0.34 goes to bucket 3
  Element 0.98 goes to bucket 9
  Element 0.01 goes to bucket 0
Sorting individual buckets
Final sorted result: (0.01 0.12 0.23 0.34 0.65 0.74 0.89 0.98)
```

## Time Complexity:
- **Best Case**: O(n + k) where n is the number of elements and k is the number of buckets
- **Average Case**: O(n + k)
- **Worst Case**: O(n²) when all elements fall into the same bucket

## Space Complexity:
- O(n + k) for the buckets and result array

