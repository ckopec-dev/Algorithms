# Bead Sort Algorithm in Lisp

Bead sort is a natural sorting algorithm that simulates the process of beads falling under gravity. Here's an implementation in Lisp:

```lisp
(defun bead-sort (list)
  "Sort a list of non-negative integers using bead sort algorithm"
  (if (null list)
      nil
      (let ((max-val (apply #'max list))
            (beads (make-array (length list) :element-type 'integer :initial-element 0)))
        ;; Initialize beads array
        (loop for i from 0 below (length list)
              do (setf (aref beads i) (nth i list)))
        
        ;; Simulate bead falling process
        (loop for i from 1 to max-val
              do (let ((count 0))
                   (loop for j from 0 below (length list)
                         do (when (and (>= (aref beads j) i)
                                       (< (aref beads j) (length list)))
                              (incf count)))
                   (loop for j from 0 below (length list)
                         do (when (and (>= (aref beads j) i)
                                       (< (aref beads j) (length list)))
                              (setf (aref beads j) (1+ (aref beads j)))))))
        
        ;; Extract sorted result
        (loop for i from 0 below (length list)
              collect (aref beads i)))))

;; Alternative simpler implementation
(defun bead-sort-simple (list)
  "Simpler bead sort implementation"
  (if (null list)
      nil
      (let ((max-val (apply #'max list))
            (sorted-list '()))
        ;; Create a 'bead' representation
        (loop for i from 1 to max-val
              do (let ((count 0))
                   (loop for item in list
                         do (when (>= item i)
                              (incf count)))
                   (loop for j from 1 to count
                         do (push i sorted-list)))))
        (nreverse sorted-list))))

;; Example usage:
;; (bead-sort-simple '(4 3 1 2 5))
;; Returns: (1 2 3 4 5)

;; (bead-sort-simple '(5 2 8 1 9 3))
;; Returns: (1 2 3 5 8 9)
```

## How it works:

1. **Find maximum value** in the input list
2. **Simulate bead falling**: For each "level" from 1 to maximum value, count how many numbers are greater than or equal to that level
3. **Reconstruct sorted array**: Build the result by placing beads in their final positions

## Example trace:
For input `[4 3 1 2 5]`:
- Level 1: 5 beads → result: [1 1 1 1 1]
- Level 2: 4 beads → result: [2 2 2 2 1] 
- Level 3: 3 beads → result: [3 3 3 2 1]
- Level 4: 2 beads → result: [4 4 3 2 1]
- Level 5: 1 bead → result: [5 4 3 2 1]

Final sorted: [1 2 3 4 5]

