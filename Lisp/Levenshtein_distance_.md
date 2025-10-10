# Levenshtein Distance Algorithm in Lisp

Here's an implementation of the Levenshtein distance algorithm in Common Lisp:

```lisp
(defun levenshtein-distance (str1 str2)
  "Calculate the Levenshtein distance between two strings."
  (let ((len1 (length str1))
        (len2 (length str2)))
    (if (zerop len1)
        len2
        (if (zerop len2)
            len1
            (let ((matrix (make-array (list (1+ len1) (1+ len2)) 
                                     :element-type 'fixnum 
                                     :initial-element 0)))
              ;; Initialize first row and column
              (loop for i from 0 to len1 do
                    (setf (aref matrix i 0) i))
              (loop for j from 0 to len2 do
                    (setf (aref matrix 0 j) j))
              
              ;; Fill the matrix
              (loop for i from 1 to len1 do
                    (loop for j from 1 to len2 do
                          (let ((cost (if (char= (char str1 (1- i)) 
                                           (char str2 (1- j)))
                                          0
                                          1)))
                            (setf (aref matrix i j)
                                  (min (1+ (aref matrix (1- i) j))     ; deletion
                                       (1+ (aref matrix i (1- j)))     ; insertion
                                       (1+ (aref matrix (1- i) (1- j)) cost)))))) ; substitution
              
              (aref matrix len1 len2))))))

;; Example usage:
(levenshtein-distance "kitten" "sitting")
;; Returns: 3

(levenshtein-distance "hello" "world")
;; Returns: 4

(levenshtein-distance "same" "same")
;; Returns: 0

(levenshtein-distance "" "test")
;; Returns: 4

(levenshtein-distance "algorithm" "logarithm")
;; Returns: 3
```

## How it works:

1. **Initialization**: Create a matrix of size (len1+1) × (len2+1)
2. **Base cases**: Fill the first row and column with incremental values
3. **Dynamic programming**: For each cell, calculate the minimum cost of:
   - Deletion: `matrix[i-1][j] + 1`
   - Insertion: `matrix[i][j-1] + 1` 
   - Substitution: `matrix[i-1][j-1] + cost` (0 if characters match, 1 otherwise)
4. **Result**: The value at the bottom-right corner of the matrix

## Alternative recursive implementation with memoization:

```lisp
(defun levenshtein-recursive (str1 str2 &optional (memo (make-hash-table)))
  "Recursive implementation with memoization."
  (let ((key (list str1 str2)))
    (if (gethash key memo)
        (gethash key memo)
        (let ((result 
                (cond
                  ((string= str1 str2) 0)
                  ((string= str1 "") (length str2))
                  ((string= str2 "") (length str1))
                  ((char= (char str1 0) (char str2 0))
                   (levenshtein-recursive (subseq str1 1) (subseq str2 1) memo))
                  (t (1+ (min 
                           (levenshtein-recursive (subseq str1 1) str2 memo)
                           (levenshtein-recursive str1 (subseq str2 1) memo)
                           (levenshtein-recursive (subseq str1 1) (subseq str2 1) memo)))))))
          (setf (gethash key memo) result)
          result))))

;; Example usage:
(levenshtein-recursive "kitten" "sitting")
;; Returns: 3
```

The first implementation is more efficient with O(m×n) time complexity, while the recursive version has exponential time complexity without memoization but is more intuitive to understand.

