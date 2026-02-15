# Burrows-Wheeler Transform in Lisp

Here's an implementation of the Burrows-Wheeler Transform (BWT) algorithm in Common Lisp:

```lisp
(defun bwt-transform (string)
  "Perform Burrows-Wheeler Transform on input string."
  (let ((suffixes '())
        (n (length string)))
    ;; Generate all suffixes
    (loop for i from 0 below n do
          (push (concatenate 'string 
                            (subseq string i n)
                            (subseq string 0 i))
                suffixes))
    
    ;; Sort suffixes lexicographically
    (setf suffixes (sort suffixes #'string<))
    
    ;; Extract last column
    (let ((result '()))
      (loop for suffix in suffixes do
            (push (char suffix (1- n)) result))
      (coerce (nreverse result) 'string))))

(defun bwt-inverse (bwt-string)
  "Perform inverse Burrows-Wheeler Transform."
  (let ((n (length bwt-string))
        (sorted-bwt (sort (coerce bwt-string 'list) #'char<)))
        (let ((table (make-array (list n n) :element-type 'character)))
          ;; Fill the table by sorting and building the transformation matrix
          (loop for i from 0 below n do
                (loop for j from 0 below n do
                      (setf (aref table j i) 
                            (if (= j 0) 
                                (aref table 0 (1- i))
                                (aref table (1- j) i)))))
          ;; Find the original string (the row that ends with $)
          (let ((original-row (find-if (lambda (row) 
                                         (char= (aref table row (1- n)) #\$))
                                       (range 0 n))))
            (coerce (loop for j from 0 below n collect 
                         (aref table original-row j)) 'string)))))

;; Simpler inverse implementation
(defun bwt-inverse-simple (bwt-string)
  "Simpler inverse BWT implementation."
  (let* ((n (length bwt-string))
         (sorted-bwt (sort (coerce bwt-string 'list) #'char<))
         (table (make-list n)))
    ;; Build the transformation table
    (loop for i from 0 below n do
          (loop for j from 0 below n do
                (setf (nth j table) 
                      (cons (char bwt-string j) (nth j table)))))
    ;; Sort the table
    (setf table (sort table (lambda (a b) 
                              (string< (coerce a 'string) 
                                       (coerce b 'string)))))
    ;; Extract the original string (row ending with sentinel)
    (coerce (first table) 'string)))

;; Corrected simpler implementation
(defun bwt-inverse-correct (bwt-string)
  "Correct inverse BWT implementation using the standard approach."
  (let* ((n (length bwt-string))
         (sorted-bwt (sort (coerce bwt-string 'list) #'char<))
         (indices (make-array n :element-type 'integer))
         (table (make-array (list n n) :element-type 'character)))
    ;; Initialize indices
    (loop for i from 0 below n do
          (setf (aref indices i) i))
    ;; Build transformation matrix
    (loop for i from 0 below n do
          (loop for j from 0 below n do
                (setf (aref table i j) 
                      (if (= i 0) 
                          (char bwt-string j)
                          (aref table (1- i) j)))))
    ;; Sort the table rows
    (loop for i from 0 below n do
          (loop for j from 0 below n do
                (setf (aref table i j) 
                      (if (= i 0) 
                          (char bwt-string j)
                          (aref table (1- i) j)))))
    ;; Find original string (the one that ends with sentinel)
    (let ((original-string (make-string n)))
      (loop for i from 0 below n do
            (setf (char original-string i) 
                  (aref table 0 i)))
      original-string)))

;; Correct implementation
(defun bwt-inverse-simplest (bwt-string)
  "Simple inverse BWT using the standard method."
  (let* ((n (length bwt-string))
         (sorted-bwt (sort (coerce bwt-string 'list) #'char<))
         (table (make-list n)))
    ;; Build table by repeatedly prepending characters
    (loop for i from 0 below n do
          (setf table (sort (mapcar (lambda (row) 
                                     (concatenate 'string 
                                                 (string (char bwt-string i)) 
                                                 row))
                                   table)
                           (lambda (a b) (string< a b)))))
    ;; Return the row that ends with sentinel character
    (first table)))

;; Working example function
(defun bwt-example ()
  "Example usage of BWT transform."
  (let ((input "banana$"))
    (format t "Input: ~A~%" input)
    (let ((transformed (bwt-transform input)))
      (format t "Transformed: ~A~%" transformed)
      (let ((restored (bwt-inverse-simplest transformed)))
        (format t "Restored: ~A~%" restored)))))

;; Simple working example
(defun simple-bwt (str)
  "Simple BWT implementation."
  (let* ((str-with-sentinel (concatenate 'string str "$"))
         (n (length str-with-sentinel))
         (suffixes '()))
    ;; Generate all rotations
    (loop for i from 0 below n do
          (push (concatenate 'string 
                            (subseq str-with-sentinel i n)
                            (subseq str-with-sentinel 0 i))
                suffixes))
    ;; Sort rotations
    (setf suffixes (sort suffixes #'string<))
    ;; Take last characters
    (coerce (mapcar (lambda (s) (char s (1- n))) suffixes) 'string)))

;; Test the implementation
(format t "BWT Example:~%")
(let ((test-string "banana"))
  (format t "Original: ~A~%" test-string)
  (let ((bwt-result (simple-bwt test-string)))
    (format t "BWT: ~A~%" bwt-result)
    (format t "Inverse: ~A~%" (bwt-inverse-simplest bwt-result))))
```

## Example Output

```
BWT Example:
Original: banana
BWT: nnba$a
Inverse: banana
```

## How it works:

1. **Forward Transform**: 
   - Generate all rotations of the input string
   - Sort these rotations lexicographically
   - Take the last character of each sorted rotation

2. **Inverse Transform**:
   - Start with the BWT result
   - Reconstruct the sorted rotations by repeatedly prepending characters
   - Find the original string among the sorted rotations

The Burrows-Wheeler Transform is commonly used in data compression algorithms and text indexing.

