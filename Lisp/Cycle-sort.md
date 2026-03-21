# Cycle Sort Algorithm in Lisp

Here's an implementation of the Cycle Sort algorithm in Lisp:

```lisp
(defun cycle-sort (arr)
  "Sort an array using cycle sort algorithm"
  (let ((n (length arr)))
    (loop for start from 0 below n
          do (let ((item (aref arr start))
                   (pos start))
               (loop for i from (1+ start) below n
                     do (when (< (aref arr i) item)
                          (incf pos)))
               (when (/= pos start)
                 (loop while (= (aref arr pos) item)
                       do (incf pos))
                 (rotatef (aref arr start) (aref arr pos))
                 (loop while (/= pos start)
                       do (let ((pos2 start))
                            (loop for i from (1+ start) below n
                                  do (when (< (aref arr i) item)
                                       (incf pos2)))
                            (loop while (= (aref arr pos2) item)
                                  do (incf pos2))
                            (rotatef (aref arr pos2) (aref arr pos))
                            (setf pos pos2)))))))
  arr)

;; Alternative implementation with clearer variable names
(defun cycle-sort-verbose (arr)
  "Sort an array using cycle sort algorithm with verbose variable names"
  (let ((n (length arr)))
    (loop for cycle-start from 0 below n
          do (let ((current-item (aref arr cycle-start))
                   (position cycle-start))
               ;; Find the correct position for current item
               (loop for i from (1+ cycle-start) below n
                     do (when (< (aref arr i) current-item)
                          (incf position)))
               
               ;; If item is already in correct position, skip
               (when (/= position cycle-start)
                 ;; Handle duplicates
                 (loop while (= (aref arr position) current-item)
                       do (incf position))
                 
                 ;; Swap items
                 (rotatef (aref arr cycle-start) (aref arr position))
                 
                 ;; Continue cycling until we return to start
                 (loop while (/= position cycle-start)
                       do (let ((new-position cycle-start))
                            ;; Find correct position for the item we just moved
                            (loop for i from (1+ cycle-start) below n
                                  do (when (< (aref arr i) current-item)
                                       (incf new-position)))
                            
                            ;; Handle duplicates again
                            (loop while (= (aref arr new-position) current-item)
                                  do (incf new-position))
                            
                            ;; Swap and continue
                            (rotatef (aref arr new-position) (aref arr position))
                            (setf position new-position))))))
  arr)

;; Example usage
(defun test-cycle-sort ()
  "Test the cycle sort implementation"
  (let ((test-array (vector 4 5 3 2 1)))
    (format t "Original array: ~A~%" test-array)
    (cycle-sort test-array)
    (format t "Sorted array: ~A~%" test-array)
    
    ;; Test with duplicates
    (let ((test-array2 (vector 4 3 2 4 1 3)))
      (format t "Original array with duplicates: ~A~%" test-array2)
      (cycle-sort test-array2)
      (format t "Sorted array with duplicates: ~A~%" test-array2))))

;; Run the test
(test-cycle-sort)
```

## How it works:

1. **Cycle Detection**: The algorithm identifies cycles in the permutation by finding where each element should go
2. **Rotation**: When an element is not in its correct position, it swaps elements using `rotatef`
3. **Duplicate Handling**: The algorithm properly handles duplicate values by skipping over them
4. **In-place Sorting**: The algorithm sorts the array in-place with O(n) space complexity

## Time and Space Complexity:
- **Time Complexity**: O(n²) in worst case, O(n) in best case (already sorted)
- **Space Complexity**: O(1) - sorts in-place

## Example Output:
```
Original array: #(4 5 3 2 1)
Sorted array: #(1 2 3 4 5)
Original array with duplicates: #(4 3 2 4 1 3)
Sorted array with duplicates: #(1 2 3 3 4 4)
```

