# Lempel-Ziv Algorithm Implementation in Lisp

Here's a simple implementation of the Lempel-Ziv compression algorithm in Common Lisp:

```lisp
(defpackage :lz-compression
  (:use :cl))
(in-package :lz-compression)

;; Lempel-Ziv Compression Algorithm
(defun lz-compress (input-string)
  "Compress a string using Lempel-Ziv algorithm."
  (let ((dictionary (make-hash-table :test 'equal))
        (result '())
        (index 1)
        (i 0))
    ;; Initialize dictionary with single characters
    (loop for char across input-string
          for j from 0
          do (unless (gethash char dictionary)
               (setf (gethash char dictionary) index)
               (incf index)))
    
    ;; Process the string
    (loop while (< i (length input-string))
          do (let ((current-char (char input-string i))
                   (match-length 0)
                   (match-string ""))
               ;; Find longest match in dictionary
               (loop for j from i below (length input-string)
                     for substr = (subseq input-string i (1+ j))
                     while (gethash substr dictionary)
                     do (setf match-string substr
                              match-length (1+ match-length)))
               
               ;; Add to result
               (if (> match-length 0)
                   (let ((code (gethash match-string dictionary)))
                     (push (list code match-length) result))
                   (let ((code (gethash current-char dictionary)))
                     (push (list code 0) result)))
               
               ;; Add new entries to dictionary
               (loop for j from (1+ i) to (length input-string)
                     for new-entry = (subseq input-string i j)
                     unless (gethash new-entry dictionary)
                     do (setf (gethash new-entry dictionary) index
                              (incf index)
                              j (length input-string))))
          (incf i))
    
    ;; Return compressed result (reverse to maintain order)
    (nreverse result)))

;; Lempel-Ziv Decompression Algorithm
(defun lz-decompress (compressed-data)
  "Decompress data using Lempel-Ziv algorithm."
  (let ((dictionary (make-hash-table :test 'equal))
        (result '())
        (index 1))
    ;; Initialize dictionary with single characters
    (loop for i from 0 to 255
          do (setf (gethash i dictionary) (string (code-char i))))
    
    ;; Process compressed data
    (loop for (code length) in compressed-data
          do (let ((entry (gethash code dictionary)))
               (if entry
                   (progn
                     (push entry result)
                     ;; Add new entry to dictionary
                     (when (and (> length 0)
                                (< index 1000)) ; Prevent infinite loop
                       (let ((new-entry (concatenate 'string entry
                                                    (subseq entry 0 length))))
                         (setf (gethash index dictionary) new-entry)
                         (incf index))))
                   (progn
                     (push (string (code-char code)) result)
                     (setf (gethash index dictionary) (string (code-char code)))
                     (incf index)))))
    
    ;; Return decompressed string
    (apply #'concatenate 'string (nreverse result))))

;; Example usage
(defun example ()
  "Demonstrate Lempel-Ziv compression/decompression."
  (let ((original "ABABABABABABABABABABABABABABABABABABABABABABABABABABABABABABAB

