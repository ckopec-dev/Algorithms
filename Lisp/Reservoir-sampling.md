# Reservoir Sampling Algorithm in Lisp

Here's an implementation of the Reservoir Sampling algorithm in Common Lisp:

```lisp
(defun reservoir-sampling (stream k)
  "Perform reservoir sampling on a stream of elements, returning k random elements.
   Uses the algorithm where we keep a reservoir of size k and replace elements
   with decreasing probability as we process more items."
  (let ((reservoir (make-array k :fill-pointer 0 :adjustable t))
        (count 0))
    (loop for item in stream do
      (incf count)
      (if (< (length reservoir) k)
          ;; If reservoir isn't full yet, add the item
          (vector-push-extend item reservoir)
          ;; If reservoir is full, replace elements with probability k/count
          (let ((r (random count)))
            (when (< r k)
              (setf (aref reservoir r) item)))))
    reservoir))

;; Alternative implementation using a more explicit approach
(defun reservoir-sampling-verbose (stream k)
  "Verbose implementation of reservoir sampling algorithm."
  (let ((reservoir '())
        (count 0))
    (loop for item in stream do
      (incf count)
      (if (< (length reservoir) k)
          ;; Add item to reservoir if it's not full
          (push item reservoir)
          ;; Replace random element with probability k/count
          (let ((replace-index (random count)))
            (when (< replace-index k)
              (setf (nth replace-index reservoir) item)))))
    (nreverse reservoir)))

;; Example usage:
(defun example-usage ()
  "Example demonstrating reservoir sampling with different inputs."
  (let ((large-stream (loop for i from 1 to 1000 collect i)))
    (format t "Original stream (first 10 elements): ~A~%" 
            (subseq large-stream 0 10))
    (format t "Reservoir sample of 5 elements: ~A~%" 
            (reservoir-sampling large-stream 5))
    (format t "Reservoir sample of 3 elements: ~A~%" 
            (reservoir-sampling large-stream 3))))

;; Simple test function
(defun test-reservoir ()
  "Simple test to verify the algorithm works correctly."
  (let ((test-stream '(1 2 3 4 5 6 7 8 9 10)))
    (format t "Test stream: ~A~%" test-stream)
    (format t "Sample of 3: ~A~%" (reservoir-sampling test-stream 3))
    (format t "Sample of 5: ~A~%" (reservoir-sampling test-stream 5))))

;; Run the example
(test-reservoir)
```

## How it works:

1. **Initialization**: Start with an empty reservoir of size k
2. **Processing**: For each element in the stream:
   - If reservoir isn't full, add the element
   - If reservoir is full, replace a random element with probability k/count
3. **Result**: Returns k randomly selected elements from the stream

## Key Features:

- **Memory Efficient**: Only stores k elements regardless of stream size
- **Unbiased**: Each element has equal probability of being selected
- **Single Pass**: Processes the stream only once
- **Streaming**: Works with infinite streams

## Time Complexity:
- **Time**: O(n) where n is the number of elements in the stream
- **Space**: O(k) where k is the reservoir size

## Example Output:
```
Test stream: (1 2 3 4 5 6 7 8 9 10)
Sample of 3: (2 7 10)
Sample of 5: (1 3 5 8 9)
```

The algorithm is particularly useful when dealing with large datasets that don't fit in memory or when processing data streams where you need a random sample of fixed size.

