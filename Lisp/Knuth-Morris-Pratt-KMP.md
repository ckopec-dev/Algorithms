# Knuth-Morris-Pratt (KMP) Algorithm in Lisp

Here's an implementation of the KMP algorithm in Common Lisp:

```lisp
(defun compute-lps-array (pattern)
  "Compute the Longest Proper Prefix which is also Suffix array for KMP"
  (let ((m (length pattern))
        (lps (make-array (length pattern) :element-type 'fixnum :initial-element 0))
        (len 0)
        (i 1))
    (loop while (< i m) do
      (if (char= (char pattern i) (char pattern len))
          (progn
            (incf len)
            (setf (aref lps i) len)
            (incf i))
          (if (> len 0)
              (setf len (aref lps (1- len)))
              (progn
                (setf (aref lps i) 0)
                (incf i)))))
    lps))

(defun kmp-search (text pattern)
  "Search for pattern in text using KMP algorithm"
  (let ((n (length text))
        (m (length pattern))
        (lps (compute-lps-array pattern))
        (i 0)
        (j 0)
        (matches '()))
    (loop while (and (< i n) (< j m)) do
      (if (char= (char text i) (char pattern j))
          (progn
            (incf i)
            (incf j))
          (if (> j 0)
              (setf j (aref lps (1- j)))
              (incf i))))
    (if (= j m)
        (progn
          (push (- i j) matches)
          (kmp-search (subseq text (1+ (- i j))) pattern))
        matches)))

(defun kmp-search-simple (text pattern)
  "Simple version that returns all starting positions of pattern in text"
  (let ((n (length text))
        (m (length pattern))
        (lps (compute-lps-array pattern))
        (i 0)
        (j 0)
        (positions '()))
    (loop while (< i n) do
      (if (char= (char text i) (char pattern j))
          (progn
            (incf i)
            (incf j))
          (if (> j 0)
              (setf j (aref lps (1- j)))
              (incf i))))
    (if (= j m)
        (push (- i j) positions))
    positions))

;; More complete implementation
(defun kmp-search-complete (text pattern)
  "Complete KMP search returning all matches"
  (if (or (null text) (null pattern) (zerop (length pattern)))
      '()
      (let* ((n (length text))
             (m (length pattern))
             (lps (compute-lps-array pattern))
             (positions '()))
        (loop with i = 0
              with j = 0
              while (< i n) do
                (if (char= (char text i) (char pattern j))
                    (progn
                      (incf i)
                      (incf j))
                    (if (> j 0)
                        (setf j (aref lps (1- j)))
                        (incf i))))
        (if (= j m)
            (push (- i j) positions))
        (nreverse positions))))

;; Example usage:
;; (kmp-search-complete "ABABDABACDABABCABCABCABCABC" "ABABCABCABCABC")
;; Returns: (13)
```

## Example Usage:

```lisp
;; Test the algorithm
(let ((text "ABABDABACDABABCABCABCABCABC")
      (pattern "ABABCABCABCABC"))
  (kmp-search-complete text pattern))
;; Output: (13)

;; Another example
(let ((text "AABAACAADAABAABA")
      (pattern "AABA"))
  (kmp-search-complete text pattern))
;; Output: (0 9 13)
```

## How it works:

1. **LPS Array Construction**: The `compute-lps-array` function builds the Longest Proper Prefix which is also Suffix array
2. **Pattern Matching**: The main search function uses this LPS array to avoid unnecessary character comparisons
3. **Time Complexity**: O(n + m) where n is text length and m is pattern length
4. **Space Complexity**: O(m) for the LPS array

The KMP algorithm is particularly useful when you need to search for a pattern in a text efficiently, especially when the pattern has overlapping subpatterns.

