# LZW Algorithm Implementation in Lisp

Here's a complete implementation of the LZW compression algorithm in Common Lisp:

```lisp
(defpackage :lzw
  (:use :cl)
  (:export :lzw-compress :lzw-decompress))

(in-package :lzw)

;; LZW Compression Algorithm
(defun lzw-compress (input-string)
  "Compress a string using LZW algorithm"
  (let ((dictionary (make-hash-table :test 'equal))
        (code 256)  ; Start with ASCII values
        (result '())
        (current-string "")
        (input-chars (coerce input-string 'list)))
    
    ;; Initialize dictionary with ASCII characters
    (loop for i from 0 to 255 do
          (setf (gethash (string (code-char i)) dictionary) i))
    
    ;; Process input characters
    (loop for char in input-chars do
          (let ((extended-string (concatenate 'string current-string (string char))))
            (if (gethash extended-string dictionary)
                (setf current-string extended-string)
                (progn
                  ;; Output the code for current string
                  (push (gethash current-string dictionary) result)
                  ;; Add new string to dictionary
                  (setf (gethash extended-string dictionary) code)
                  (incf code)
                  (setf current-string (string char))))))
    
    ;; Output the last code
    (when current-string
      (push (gethash current-string dictionary) result))
    
    ;; Return result in correct order
    (nreverse result)))

;; LZW Decompression Algorithm
(defun lzw-decompress (codes)
  "Decompress LZW compressed codes back to string"
  (let ((dictionary (make-hash-table :test 'equal))
        (code 256)
        (result '())
        (old-code nil))
    
    ;; Initialize dictionary with ASCII characters
    (loop for i from 0 to 255 do
          (setf (gethash i dictionary) (string (code-char i))))
    
    ;; Process codes
    (loop for code in codes do
          (if (gethash code dictionary)
              (let ((string (gethash code dictionary)))
                (if old-code
                    (let ((old-string (gethash old-code dictionary))
                          (new-string (concatenate 'string old-string (string (char string 0)))))
                      (setf (gethash code dictionary) string)
                      (setf (gethash (length dictionary) dictionary) new-string)
                      (incf code))
                    (setf (gethash code dictionary) string))
                (push string result))
              (let ((old-string (gethash old-code dictionary))
                    (new-string (concatenate 'string old-string (string (char old-string 0)))))
                (setf (gethash code dictionary) new-string)
                (push new-string result)
                (incf code))))
    
    ;; Return result as string
    (apply #'concatenate 'string (nreverse result))))

;; Alternative simpler decompression function
(defun lzw-decompress-simple (codes)
  "Simpler LZW decompression implementation"
  (let ((dictionary (make-hash-table :test 'equal))
        (result '())
        (code 256))
    
    ;; Initialize dictionary with ASCII characters
    (loop for i from 0 to 255 do
          (setf (gethash i dictionary) (string (code-char i))))
    
    (let ((previous-code nil))
      (loop for code in codes do
            (let ((string (gethash code dictionary)))
              (if previous-code
                  (let ((old-string (gethash previous-code dictionary)))
                    (setf (gethash (length dictionary) dictionary) 
                          (concatenate 'string old-string (string (char string 0))))
                    (incf code))
                  (setf (gethash (length dictionary) dictionary) 
                        (concatenate 'string (string (char string 0)) (string (char string 0)))))
              (push string result)
              (setf previous-code code))))
    
    (apply #'concatenate 'string (nreverse result))))

;; Example usage
(defun test-lzw ()
  "Test the LZW compression and decompression"
  (let ((original "ABABABABABABABABABABABABABABABABABABABABABABABABABABABABABABAB

