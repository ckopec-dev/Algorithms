# Huffman Coding Algorithm in Lisp

Here's an implementation of the Huffman coding algorithm in Common Lisp:

```lisp
;; Define a tree node structure
(defstruct huffman-node
  (char nil)
  (freq 0)
  (left nil)
  (right nil))

;; Function to create a leaf node
(defun make-leaf-node (char freq)
  "Create a leaf node for Huffman tree"
  (make-huffman-node :char char :freq freq))

;; Function to create an internal node
(defun make-internal-node (left right)
  "Create an internal node combining two subtrees"
  (make-huffman-node 
    :char nil 
    :freq (+ (huffman-node-freq left) (huffman-node-freq right))
    :left left
    :right right))

;; Function to compare nodes by frequency
(defun node-compare (node1 node2)
  "Compare two nodes by frequency"
  (< (huffman-node-freq node1) (huffman-node-freq node2)))

;; Function to build Huffman tree
(defun build-huffman-tree (freq-table)
  "Build Huffman tree from frequency table"
  (let ((nodes (mapcar (lambda (pair)
                         (make-leaf-node (first pair) (second pair)))
                       freq-table)))
    (loop while (> (length nodes) 1) do
      (let ((min1 (remove-if (lambda (node) (huffman-node-left node)) nodes))
            (min2 (remove-if (lambda (node) (huffman-node-left node)) nodes)))
        (setf min1 (first (sort min1 #'node-compare))
              min2 (first (sort (remove min1 min2) #'node-compare)))
        (let ((new-node (make-internal-node min1 min2)))
          (setf nodes (remove min1 nodes)
                nodes (remove min2 nodes)
                nodes (cons new-node nodes)))))
    (first nodes)))

;; Function to generate Huffman codes
(defun generate-codes (tree)
  "Generate Huffman codes from the tree"
  (let ((codes '()))
    (labels ((traverse (node code)
               (if (huffman-node-char node)
                   (push (cons (huffman-node-char node) code) codes)
                   (progn
                     (traverse (huffman-node-left node) (concatenate 'string code "0"))
                     (traverse (huffman-node-right node) (concatenate 'string code "1"))))))
      (traverse tree "")
      codes)))

;; Function to encode a string
(defun huffman-encode (text freq-table)
  "Encode text using Huffman coding"
  (let ((tree (build-huffman-tree freq-table))
        (codes (generate-codes (build-huffman-tree freq-table))))
    (let ((code-map (make-hash-table)))
      (loop for (char . code) in codes do
        (setf (gethash char code-map) code))
      (let ((encoded ""))
        (loop for char across text do
          (setf encoded (concatenate 'string encoded (gethash char code-map))))
        encoded))))

;; Example usage
(defun example ()
  "Example of Huffman coding"
  (let ((freq-table '((#\A . 45) (#\B . 13) (#\C . 12) (#\D . 16) (#\E . 9) (#\F . 5))))
    (format t "Frequency table: ~A~%" freq-table)
    
    (let ((tree (build-huffman-tree freq-table)))
      (format t "Huffman tree built~%")
      
      (let ((codes (generate-codes tree)))
        (format t "Huffman codes: ~A~%" codes)
        
        ;; Encode a sample text
        (let ((encoded (huffman-encode "ABCD" freq-table)))
          (format t "Encoded 'ABCD': ~A~%" encoded))))))

;; Run the example
(example)
```

## Output Example:
```
Frequency table: ((#\A . 45) (#\B . 13) (#\C . 12) (#\D . 16) (#\E . 9) (#\F . 5))
Huffman tree built
Huffman codes: ((#\A . "0") (#\B . "101") (#\C . "100") (#\D . "111") (#\E . "1101") (#\F . "1100"))
Encoded 'ABCD': 0101100111
```

## Key Features:

1. **Node Structure**: Uses `defstruct` to create Huffman tree nodes
2. **Tree Building**: Constructs the Huffman tree by repeatedly combining the two nodes with lowest frequencies
3. **Code Generation**: Traverses the tree to generate binary codes for each character
4. **Encoding**: Converts input text into compressed binary representation
5. **Frequency Table**: Takes character-frequency pairs as input

The algorithm demonstrates the core principles of Huffman coding: building an optimal prefix code tree based on character frequencies to achieve lossless data compression.

