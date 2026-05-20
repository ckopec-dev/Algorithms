# Levenshtein Distance Algorithm in Lisp

Here's an implementation of the Levenshtein distance algorithm in Common Lisp:

```lisp
(defun levenshtein-distance (str1 str2)
  "Calculate the Levenshtein distance between two strings."
  (let ((len1 (length str1))
        (len2 (length str2)))
    (if (or (= len1 0) (= len2 0))
        (max len1 len2)
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
                                    0 1)))
                        (setf (aref matrix i j)
                              (min (1+ (aref matrix (1- i) j))     ; deletion
                                   (1+ (aref matrix i (1- j)))     ; insertion
                                   (+ (aref matrix (1- i) (1- j)) cost)))))) ; substitution
          
          (aref matrix len1 len2)))))

;; Example usage:
(levenshtein-distance "kitten" "sitting")
;; Returns: 3

(levenshtein-distance "hello" "world")
;; Returns: 4

(levenshtein-distance "same" "same")
;; Returns: 0

(levenshtein-distance "" "test")
;; Returns: 4
```

## How it works:

1. **Base case**: If either string is empty, return the length of the other string
2. **Matrix initialization**: Create a 2D array where `matrix[i][j]` represents the distance between the first `i` characters of `str1` and the first `j` characters of `str2`
3. **Initialization**: Fill the first row and column with sequential numbers (0, 1, 2, ...)
4. **Dynamic programming**: For each cell, calculate the minimum cost of:
   - Deletion: `matrix[i-1][j] + 1`
   - Insertion: `matrix[i][j-1] + 1`  
   - Substitution: `matrix[i-1][j-1] + cost` (where cost is 0 if characters match, 1 otherwise)
5. **Result**: The bottom-right cell contains the Levenshtein distance

## Time and Space Complexity:
- **Time**: O(m × n) where m and n are the lengths of the input strings
- **Space**: O(m × n) for the matrix storage

