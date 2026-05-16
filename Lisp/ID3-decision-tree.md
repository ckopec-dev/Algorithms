# ID3 Decision Tree Algorithm in Lisp

Here's an implementation of the ID3 algorithm for building decision trees in Lisp:

```lisp
;; Define a tree node structure
(defstruct node
  (label nil)
  (is-leaf nil)
  (decision nil)
  (children nil)
  (attribute nil))

;; Calculate entropy of a dataset
(defun entropy (dataset)
  (if (null dataset)
      0.0
      (let ((total (length dataset))
            (pos-count (length (remove-if-not #'(lambda (x) (eq (car (last x)) 'yes)) dataset)))
            (neg-count (length (remove-if-not #'(lambda (x) (eq (car (last x)) 'no)) dataset))))
        (let ((p-pos (if (zerop total) 0.0 (/ pos-count total)))
              (p-neg (if (zerop total) 0.0 (/ neg-count total))))
          (if (or (zerop p-pos) (zerop p-neg))
              0.0
              (- (+ (* p-pos (log p-pos 2)) (* p-neg (log p-neg 2)))))))))

;; Calculate information gain for an attribute
(defun information-gain (dataset attribute-index)
  (let ((total (length dataset))
        (attribute-values (remove-duplicates (mapcar (lambda (x) (nth attribute-index x)) dataset)))
        (entropy-dataset (entropy dataset)))
    (let ((weighted-entropy 0.0))
      (dolist (value attribute-values)
        (let ((subset (remove-if-not (lambda (x) (eq (nth attribute-index x) value)) dataset))
              (subset-size (length (remove-if-not (lambda (x) (eq (nth attribute-index x) value)) dataset))))
          (when (> subset-size 0)
            (setf weighted-entropy (+ weighted-entropy 
                                     (* (/ subset-size total) (entropy subset)))))))
      (- entropy-dataset weighted-entropy))))

;; Find the best attribute to split on
(defun best-attribute (dataset attributes)
  (let ((max-gain 0.0)
        (best-attr nil))
    (dolist (attr attributes)
      (let ((gain (information-gain dataset attr)))
        (when (> gain max-gain)
          (setf max-gain gain)
          (setf best-attr attr))))
    best-attr))

;; Get unique values for an attribute
(defun unique-values (dataset attribute-index)
  (remove-duplicates (mapcar (lambda (x) (nth attribute-index x)) dataset)))

;; Get subset of dataset for a specific attribute value
(defun subset-dataset (dataset attribute-index value)
  (remove-if-not (lambda (x) (eq (nth attribute-index x) value)) dataset))

;; ID3 algorithm implementation
(defun id3 (dataset attributes target-attribute)
  (let ((labels (mapcar (lambda (x) (car (last x))) dataset)))
    (cond
      ;; If all examples have the same label, return a leaf node
      ((and (not (null labels)) 
            (every (lambda (x) (eq x (car labels))) labels))
       (make-node :is-leaf t :decision (car labels)))
      
      ;; If no attributes left, return leaf with most common label
      ((null attributes)
       (make-node :is-leaf t :decision (most-common-label labels)))
      
      ;; Otherwise, create internal node
      (t
       (let ((best-attr (best-attribute dataset attributes))
             (node (make-node :label best-attr :attribute best-attr)))
         (let ((attr-values (unique-values dataset best-attr)))
           (setf (node-children node) 
                 (mapcar (lambda (value)
                           (let ((subset (subset-dataset dataset best-attr value)))
                             (if (null subset)
                                 ;; If subset is empty, create leaf with most common label
                                 (make-node :is-leaf t :decision (most-common-label labels))
                                 ;; Recursively build subtree
                                 (id3 subset 
                                      (remove best-attr attributes) 
                                      target-attribute))))
                         attr-values)))
         node)))))

;; Helper function to find most common label
(defun most-common-label (labels)
  (let ((label-counts (remove-duplicates labels 
                                         :key (lambda (x) x) 
                                         :count t)))
    (car (sort label-counts 
               (lambda (a b) (> (cdr a) (cdr b)))))))

;; Example usage:
;; Define sample dataset
;; Format: (outlook temperature humidity windy play)
(setq sample-data '((sunny hot high false no)
                    (sunny hot high true no)
                    (overcast hot high false yes)
                    (rainy mild high false yes)
                    (rainy cool normal false yes)
                    (rainy cool normal true no)
                    (overcast cool normal true yes)
                    (sunny mild high false no)
                    (sunny cool normal false yes)
                    (rainy mild normal false yes)
                    (sunny mild normal true yes)
                    (overcast mild high true yes)
                    (overcast hot normal false yes)
                    (rainy mild high true no)))

;; Run ID3 algorithm
(setq decision-tree (id3 sample-data '(0 1 2 3) 4))

;; Print the decision tree structure
(defun print-tree (node depth)
  (if (node-is-leaf node)
      (format t "~A~A: ~A~%" 
              (make-string (* depth 2) #\Space) 
              "Leaf" 
              (node-decision node))
      (let ((indent (make-string (* depth 2) #\Space)))
        (format t "~A~A: ~A~%" 
                indent 
                "Node" 
                (node-label node))
        (dolist (child (node-children node))
          (print-tree child (+ depth 1))))))

;; Print the resulting tree
(print-tree decision-tree 0)
```

## Key Features of this Implementation:

1. **Node Structure**: Defines a tree node with label, leaf status, decision, and children
2. **Entropy Calculation**: Computes entropy of datasets for information gain
3. **Information Gain**: Calculates how much information each attribute provides
4. **Best Attribute Selection**: Finds the attribute with maximum information gain
5. **Recursive Tree Building**: Builds the tree recursively by splitting on best attributes
6. **Handling Edge Cases**: Deals with empty subsets and identical labels

## Sample Output:
The algorithm will build a decision tree that classifies whether to play based on weather conditions like outlook, temperature, humidity, and wind. The tree structure will show the decision path from root to leaves, where each leaf represents a final classification (yes/no).

This implementation demonstrates the core ID3 algorithm principles in functional Lisp style, with proper handling of the recursive tree-building process and information theory concepts.

