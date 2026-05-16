# Decision Tree Learning Algorithm in Lisp

Here's an implementation of a decision tree learning algorithm in Lisp:

```lisp
;; Decision Tree Learning Algorithm in Lisp

;; Define the tree node structure
(defstruct decision-node
  attribute    ; attribute to test
  value        ; value of the attribute
  children     ; list of child nodes
  class        ; class label (if leaf node)
  is-leaf)     ; boolean indicating if leaf node

;; Helper function to check if all examples have same class
(defun all-same-class (examples)
  "Check if all examples in the list have the same class"
  (if (null examples)
      nil
      (let ((first-class (car (car examples))))
        (every (lambda (example)
                  (equal (car (car example)) first-class))
                examples))))

;; Helper function to get unique values of an attribute
(defun get-unique-values (examples attribute-index)
  "Get unique values for a given attribute across all examples"
  (remove-duplicates 
    (mapcar (lambda (example) 
               (nth attribute-index (cdr example)))
             examples)))

;; Helper function to count class occurrences
(defun count-classes (examples)
  "Count occurrences of each class in examples"
  (let ((class-counts (make-hash-table :test 'equal)))
    (dolist (example examples)
      (let ((class (car example)))
        (if (gethash class class-counts)
            (incf (gethash class class-counts))
            (setf (gethash class class-counts) 1))))
    class-counts))

;; Helper function to get majority class
(defun majority-class (examples)
  "Get the class that appears most frequently in examples"
  (let ((class-counts (count-classes examples))
        (max-count 0)
        (majority-class nil))
    (maphash (lambda (class count)
                 (when (> count max-count)
                   (setf max-count count
                         majority-class class)))
               class-counts)
    majority-class))

;; Helper function to calculate entropy
(defun entropy (examples)
  "Calculate entropy of examples"
  (if (null examples)
      0
      (let ((class-counts (count-classes examples))
            (total (length examples))
            (entropy 0.0))
        (maphash (lambda (class count)
                     (let ((probability (/ count total)))
                       (when (> probability 0)
                         (incf entropy (* (- probability) 
                                          (log probability 2)))))
                   class-counts)
        entropy)))

;; Helper function to calculate information gain
(defun information-gain (examples attribute-index)
  "Calculate information gain for a given attribute"
  (let ((total (length examples))
        (entropy-before (entropy examples))
        (entropy-after 0.0))
    ;; Calculate weighted entropy after splitting
    (let ((unique-values (get-unique-values examples attribute-index)))
      (dolist (value unique-values)
        (let ((subset (remove-if-not 
                        (lambda (example) 
                          (equal (nth attribute-index (cdr example)) value))
                        examples))
              (subset-size (length (remove-if-not 
                                     (lambda (example) 
                                       (equal (nth attribute-index (cdr example)) value))
                                     examples))))
          (when (> subset-size 0)
            (incf entropy-after 
                   (* (/ subset-size total) 
                      (entropy subset))))))
      (- entropy-before entropy-after))))

;; Main decision tree learning algorithm
(defun decision-tree-learning (examples attributes)
  "Main decision tree learning function"
  (cond
    ;; Base case 1: no examples
    ((null examples)
     (make-decision-node :class 'unknown :is-leaf t))
    
    ;; Base case 2: all examples have same class
    ((all-same-class examples)
     (let ((class (car (car examples))))
       (make-decision-node :class class :is-leaf t)))
    
    ;; Base case 3: no attributes left
    ((null attributes)
     (let ((majority (majority-class examples)))
       (make-decision-node :class majority :is-leaf t)))
    
    ;; Recursive case: build tree
    (t
     (let* ((best-attribute (argmax (lambda (attr-index)
                                       (information-gain examples attr-index))
                                    (range 0 (length attributes))))
            (node (make-decision-node 
                    :attribute (nth best-attribute attributes)
                    :children '()
                    :is-leaf nil)))
       ;; Create child nodes for each value of the best attribute
       (let ((unique-values (get-unique-values examples best-attribute)))
         (dolist (value unique-values)
           (let ((subset (remove-if-not 
                          (lambda (example) 
                            (equal (nth best-attribute (cdr example)) value))
                          examples)))
             (let ((child (decision-tree-learning subset 
                                                 (remove-if (lambda (attr)
                                                               (equal attr (nth best-attribute attributes)))
                                                           attributes))))
               (push (make-decision-node 
                       :value value
                       :children (list child)
                       :is-leaf (decision-node-is-leaf child))
                      (decision-node-children node))))))
       node))))

;; Helper function to create range
(defun range (start end)
  "Create a list of integers from start to end-1"
  (loop for i from start below end
        collect i))

;; Helper function to find maximum value using a key function
(defun argmax (keyfn sequence)
  "Find the element that maximizes keyfn"
  (let ((max-value nil)
        (max-key nil))
    (dolist (item sequence)
      (let ((key (funcall keyfn item)))
        (when (or (null max-key) (> key max-key))
          (setf max-key key
                max-value item))))
    max-value))

;; Example usage:
;; Define sample data (class, feature1, feature2, feature3)
;; Each example is a list: (class feature1 feature2 feature3)
(setq sample-examples 
      '((yes red small round)
        (no blue large square)
        (yes green medium triangle)
        (no red large square)
        (yes blue small round)
        (no green large triangle)))

;; Define attributes
(setq sample-attributes '("color" "size" "shape"))

;; Build decision tree
(setq decision-tree (decision-tree-learning sample-examples sample-attributes))

;; Print tree structure
(defun print-tree (node level)
  "Print decision tree structure"
  (if (decision-node-is-leaf node)
      (format t "~ALeaf: ~A~%" (make-string (* 2 level) #\Space) (decision-node-class node))
      (progn
        (format t "~A~A: ~A~%" (make-string (* 2 level) #\Space) 
                (decision-node-attribute node) (decision-node-value node))
        (dolist (child (decision-node-children node))
          (print-tree child (+ level 1))))))

;; Print the tree
(print-tree decision-tree 0)
```

## Key Components of the Implementation:

1. **Data Structures**: 
   - `decision-node` struct to represent tree nodes
   - Leaf nodes contain class labels
   - Internal nodes contain attributes and children

2. **Core Functions**:
   - `decision-tree-learning`: Main recursive algorithm
   - `information-gain`: Calculates which attribute to split on
   - `entropy`: Measures impurity of examples
   - `majority-class`: Handles leaf node creation

3. **Algorithm Steps**:
   - Base cases: Empty examples, all same class, no attributes
   - Recursive case: Find best attribute, create children for each value
   - Build tree by recursively calling on subsets

This implementation demonstrates the core decision tree learning algorithm using the ID3 approach with information gain for attribute selection.

