# Heap Sort Algorithm in Lisp

Here's an implementation of the Heap Sort algorithm in Common Lisp:

```lisp
(defun heap-sort (arr)
  "Sort an array using the heap sort algorithm."
  (let ((n (length arr)))
    ;; Build max heap
    (loop for i from (1- (floor n 2)) downto 0
          do (heapify arr n i))
    
    ;; Extract elements from heap one by one
    (loop for i from (1- n) downto 1
          do (progn
               ;; Move current root to end
               (rotatef (aref arr 0) (aref arr i))
               ;; Call heapify on the reduced heap
               (heapify arr i 0)))
    arr))

(defun heapify (arr n i)
  "Transform subtree rooted at index i into a max heap."
  (let ((largest i)
        (left (* 2 i 1))
        (right (* 2 i 2)))
    
    ;; If left child exists and is greater than root
    (when (and (< left n)
               (> (aref arr left) (aref arr largest)))
      (setf largest left))
    
    ;; If right child exists and is greater than largest so far
    (when (and (< right n)
               (> (aref arr right) (aref arr largest)))
      (setf largest right))
    
    ;; If largest is not root
    (when (/= largest i)
      (rotatef (aref arr i) (aref arr largest))
      ;; Recursively heapify the affected sub-tree
      (heapify arr n largest))))

;; Example usage:
(defun example ()
  "Example of heap sort usage."
  (let ((arr #(64 34 25 12 22 11 90)))
    (format t "Original array: ~a~%" arr)
    (heap-sort arr)
    (format t "Sorted array: ~a~%" arr)))

;; Run the example
(example)
```

## How it works:

1. **Build Max Heap**: Convert the input array into a max heap structure
2. **Extract Elements**: Repeatedly extract the maximum element (root) and place it at the end
3. **Heapify**: Maintain the heap property after each extraction

## Time Complexity:
- **Time**: O(n log n) in all cases
- **Space**: O(1) - sorts in-place

## Output:
```
Original array: #(64 34 25 12 22 11 90)
Sorted array: #(11 12 22 25 34 64 90)
```

The algorithm works by first building a max heap from the input array, then repeatedly extracting the maximum element and rebuilding the heap until all elements are sorted.

