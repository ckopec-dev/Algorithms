# Run-Length Encoding (RLE) in Lisp

Here's an implementation of the Run-Length Encoding algorithm in Lisp:

```lisp
(defun run-length-encode (input-list)
  "Encode a list using run-length encoding.
   Returns a list of (count element) pairs."
  (if (null input-list)
      nil
      (let ((result nil)
            (current-element (first input-list))
            (count 1))
        (dolist (element (rest input-list))
          (if (equal element current-element)
              (incf count)
              (progn
                (push (list count current-element) result)
                (setf current-element element
                      count 1))))
        (push (list count current-element) result)
        (nreverse result))))

(defun run-length-decode (encoded-list)
  "Decode a run-length encoded list.
   Takes a list of (count element) pairs and returns the original list."
  (if (null encoded-list)
      nil
      (let ((result nil))
        (dolist (pair encoded-list)
          (let ((count (first pair))
                (element (second pair)))
            (dotimes (i count)
              (push element result)))))
        (nreverse result))))

;; Example usage:
;; Encode a list
(setq test-list '(a a a b b c c c c d))
;; => (A A A B B C C C C D)

(run-length-encode test-list)
;; => ((3 A) (2 B) (4 C) (1 D))

;; Decode the encoded list
(run-length-decode '((3 A) (2 B) (4 C) (1 D)))
;; => (A A A B B C C C C D)
```

## How it works:

1. **Encoding**: The `run-length-encode` function takes a list and groups consecutive identical elements together, counting how many times each element appears consecutively.

2. **Decoding**: The `run-length-decode` function takes the encoded pairs and reconstructs the original list by repeating each element according to its count.

## Example Output:

```lisp
;; Input: '(a a a b b c c c c d)
;; After encoding: ((3 A) (2 B) (4 C) (1 D))
;; After decoding: '(a a a b b c c c c d)
```

This implementation handles any type of elements in the list and correctly processes empty lists and single elements.

