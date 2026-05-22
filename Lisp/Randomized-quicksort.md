```lisp
(defun randomized-quicksort (arr low high)
  "Randomized quicksort implementation in Lisp"
  (when (< low high)
    ;; Partition the array and get pivot index
    (let ((pivot-index (randomized-partition arr low high)))
      ;; Recursively sort elements before and after partition
      (randomized-quicksort arr low (1- pivot-index))
      (randomized-quicksort arr (1+ pivot-index) high))))

(defun randomized-partition (arr low high)
  "Randomized partition function using random pivot selection"
  (let ((random-index (+ low (random (1+ (- high low)))))
        (pivot (aref arr high)))
    ;; Swap random element with last element
    (swap arr random-index high)
    ;; Partition using standard quicksort logic
    (partition arr low high)))

(defun partition (arr low high)
  "Standard partition function"
  (let ((pivot (aref arr high))
        (i (1- low)))
    (loop for j from low to (1- high) do
          (when (<= (aref arr j) pivot)
            (incf i)
            (swap arr i j)))
    ;; Place pivot in correct position
    (swap arr (1+ i) high)
    (1+ i)))

(defun swap (arr i j)
  "Swap two elements in array"
  (let ((temp (aref arr i)))
    (setf (aref arr i) (aref arr j))
    (setf (aref arr j) temp)))

;; Example usage:
;; (let ((test-array (make-array 6 :initial-contents '(3 6 8 10 1 2))))
;;   (randomized-quicksort test-array 0 5)
;;   test-array)
```

