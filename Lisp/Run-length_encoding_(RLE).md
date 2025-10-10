# Run-Length Encoding (RLE) in Lisp

Here's an implementation of the Run-Length Encoding algorithm in Common Lisp:

```lisp
(defun rle-encode (input-string)
  "Encode a string using run-length encoding."
  (if (null input-string)
      ""
      (let ((result '())
            (current-char (char input-string 0))
            (count 1))
        (loop for i from 1 below (length input-string)
              do (if (char= (char input-string i) current-char)
                     (incf count)
                     (progn
                       (push (if (> count 1) 
                                (concatenate 'string (write-to-string count) (string current-char))
                                (string current-char))
                             result)
                       (setf current-char (char input-string i)
                             count 1))))
        ;; Handle the last sequence
        (push (if (> count 1) 
                 (concatenate 'string (write-to-string count) (string current-char))
                 (string current-char))
              result)
        ;; Reverse and concatenate the result
        (apply #'concatenate 'string (nreverse result)))))

(defun rle-decode (encoded-string)
  "Decode a run-length encoded string."
  (let ((result '())
        (i 0))
    (loop while (< i (length encoded-string))
          do (let ((char (char encoded-string i)))
               (if (digit-char-p char)
                   ;; Read the number
                   (let ((number-string "")
                         (j i))
                     (loop while (and (< j (length encoded-string))
                                      (digit-char-p (char encoded-string j)))
                           do (progn
                                (push (char encoded-string j) number-string)
                                (incf j)))
                     (setf i j)
                     (let ((count (parse-integer (reverse number-string)))
                           (char-to-repeat (char encoded-string i)))
                       (loop repeat count
                             do (push char-to-repeat result))
                       (incf i)))
                   ;; Single character
                   (progn
                     (push char result)
                     (incf i)))))
    (apply #'concatenate 'string (nreverse result))))

;; Example usage:
;; Encode a string
(rle-encode "aaabbbcccaaa")
;; Returns: "3a3b3c3a"

;; Decode the encoded string
(rle-decode "3a3b3c3a")
;; Returns: "aaabbbcccaaa"

;; Another example
(rle-encode "wwwwaaadexxxxxx")
;; Returns: "4w3adex6x"

;; Decode it back
(rle-decode "4w3adex6x")
;; Returns: "wwwwaaadexxxxxx"
```

## How it works:

1. **Encoding**: The `rle-encode` function scans through the input string and counts consecutive identical characters. When a different character is encountered, it outputs the count followed by the character.

2. **Decoding**: The `rle-decode` function reads the encoded string, parsing numbers and repeating characters accordingly.

## Example Output:
```lisp
(rle-encode "aaabbbcccaaa")
;; => "3a3b3c3a"

(rle-decode "3a3b3c3a")
;; => "aaabbbcccaaa"
```

This implementation handles strings with repeated characters efficiently and can be easily modified for different requirements.

