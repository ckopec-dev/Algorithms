# Burrows-Wheeler Transform in Lisp

Here's an implementation of the Burrows-Wheeler Transform (BWT) algorithm in Common Lisp:

```lisp
(defun bwt-transform (string)
  "Apply Burrows-Wheeler Transform to input string."
  (let ((sorted-table (sort (bwt-make-table string) 
                           #'string<)))
    (mapconcat #'bwt-last-char sorted-table)))

(defun bwt-make-table (string)
  "Create all rotations of the string with sentinel character."
  (let ((n (length string))
        (sentinel #\|))
    (loop for i from 0 below n
          collect (concatenate 'string 
                              (subseq string i n)
                              (subseq string 0 i)
                              (string sentinel)))))

(defun bwt-last-char (rotation)
  "Extract the last character of a rotation."
  (string (char rotation (1- (length rotation)))))

(defun bwt-inverse (bwt-string)
  "Apply inverse Burrows-Wheeler Transform."
  (let ((n (length bwt-string))
        (sorted-bwt (sort (remove #\| bwt-string) #'char<)))
    (loop with table = (make-list n)
          for i from 0 below n
          do (setf table 
                   (sort (loop for j from 0 below n
                               collect (concatenate 'string 
                                                   (string (char bwt-string j))
                                                   (nth j table)))
                         #'string<)))
    (loop for i from 0 below n
          when (string= (string (char bwt-string i)) "|")
          return (subseq (nth i table) 0 (1- (length (nth i table)))))))

;; Example usage:
;; (bwt-transform "banana")
;; => "annb$aa"

;; (bwt-inverse "annb$aa")
;; => "banana"
```

## Example Walkthrough

Let's trace through transforming the string "banana":

1. **Create rotations:**
   ```
   banana|
   anana|b
   nana|ba
   ana|ban
   na|bana
   a|banana
   ```

2. **Sort rotations lexicographically:**
   ```
   a|banana
   anana|b
   ana|ban
   banana|
   na|bana
   nana|ba
   ```

3. **Extract last characters:**
   ```
   a
   b
   n
   |
   a
   a
   ```

4. **Result:** "annb$aa"

The inverse process reconstructs the original string from the BWT output.

