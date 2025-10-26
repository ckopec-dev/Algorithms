# Decision Tree Learning in Lisp

Here's an implementation of a decision tree learning algorithm in Lisp:

```lisp
;; Define a decision tree node structure
(defstruct decision-node
  attribute     ; attribute to test
  value         ; value for this branch
  children      ; list of child nodes
  class         ; class label (if leaf node)
  is-leaf)      ; boolean flag

;; Helper function to check if all examples have same class
(defun all-same-class (examples)
  "Check if all examples in the list have the same class"
  (if (null examples)
      nil
    (let ((first-class (car (car examples))))
      (every (lambda (example) 
                (equal (car example) first-class))
              examples))))

;; Helper function to get unique values of an attribute
(defun get-unique-values (examples attribute-index)
  "Get unique values for a given attribute across all examples"
  (remove-duplicates 
    (mapcar (lambda (example) 
               (nth attribute-index example))
             examples)))

;; Helper function to count class frequencies
(defun count-classes (examples)
  "Count frequency of each class in examples"
  (let ((class-counts (make-hash-table :test 'equal)))
    (dolist (example examples)
      (let ((class (car example)))
        (if (gethash class class-counts)
            (incf (gethash class class-counts))
            (setf (gethash class class-counts) 1))))
    class-counts))

;; Helper function to get most common class
(defun most-common-class (examples)
  "Return the class that appears most frequently"
  (let ((class-counts (count-classes examples))
        (max-count 0)
        (most-common nil))
    (maphash (lambda (class count)
                 (when (> count max-count)
                   (setf max-count count)
                   (setf most-common class)))
               class-counts)
    most-common))

;; Helper function to calculate entropy
(defun entropy (examples)
  "Calculate entropy of a set of examples"
  (if (null examples)
      0
    (let ((total (length examples))
          (class-counts (count-classes examples))
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
    (let ((unique-values (get-unique-values examples attribute-index)))
      (dolist (value unique-values)
        (let ((subset (remove-if-not 
                        (lambda (example) 
                          (equal (nth attribute-index example) value))
                        examples))
              (subset-size (length (remove-if-not 
                                     (lambda (example) 
                                       (equal (nth attribute-index example) value))
                                     examples))))
          (when (> subset-size 0)
            (incf entropy-after 
                  (* (/ subset-size total) 
                     (entropy subset))))))
      (- entropy-before entropy-after))))

;; Main decision tree learning algorithm
(defun decision-tree-learn (examples attributes)
  "Learn a decision tree from examples and attributes"
  (if (null examples)
      (make-decision-node :class nil :is-leaf t)
    (if (all-same-class examples)
        (make-decision-node :class (car (car examples)) :is-leaf t)
      (if (null attributes)
          (make-decision-node :class (most-common-class examples) :is-leaf t)
        (let ((best-attribute (argmax (lambda (attr-index)
                                       (information-gain examples attr-index))
                                     (range (length attributes)))))
          (let ((node (make-decision-node 
                        :attribute (nth best-attribute attributes)
                        :children nil
                        :is-leaf nil)))
            (let ((unique-values (get-unique-values examples best-attribute)))
              (dolist (value unique-values)
                (let ((subset (remove-if-not 
                                (lambda (example) 
                                  (equal (nth best-attribute example) value))
                                examples)))
                  (let ((child (decision-tree-learn 
                                 subset 
                                 (remove (nth best-attribute attributes) attributes))))
                    (push (make-decision-node 
                            :value value
                            :children (list child)
                            :is-leaf (decision-node-is-leaf child))
                          (decision-node-children node)))))))
            node))))))

;; Helper function to get range of numbers
(defun range (n)
  "Return list of numbers from 0 to n-1"
  (loop for i from 0 below n collect i))

;; Helper function to find maximum value in a list
(defun argmax (fn lst)
  "Return index of maximum value in list"
  (let ((max-value (apply #'max (mapcar fn lst)))
        (index 0))
    (dolist (item lst)
      (when (equal (funcall fn item) max-value)
        (return index))
      (incf index))))

;; Example usage
(defun example-usage ()
  "Example of how to use the decision tree learning algorithm"
  (let* ((examples '(("Yes" "Sunny" "Hot" "High" "Weak")
                     ("Yes" "Sunny" "Hot" "High" "Strong")
                     ("No" "Overcast" "Hot" "High" "Weak")
                     ("Yes" "Rain" "Mild" "High" "Weak")
                     ("Yes" "Rain" "Cool" "Normal" "Weak")
                     ("No" "Rain" "Cool" "Normal" "Strong")
                     ("Yes" "Mild" "Cool" "Normal" "Strong")
                     ("No" "Sunny" "Mild" "High" "Weak")
                     ("No" "Sunny" "Cool" "Normal" "Weak")
                     ("Yes" "Mild" "Cool" "Normal" "Strong")
                     ("No" "Overcast" "Mild" "High" "Strong")
                     ("Yes" "Overcast" "Hot" "Normal" "Weak")
                     ("Yes" "Rain" "Mild" "High" "Strong")))
         (attributes '("Outlook" "Temperature" "Humidity" "Wind")))
    (let ((tree (decision-tree-learn examples attributes)))
      (format t "Decision Tree learned successfully!~%")
      (format t "Root attribute: ~A~%" (decision-node-attribute tree))
      (format t "Number of children: ~A~%" (length (decision-node-children tree))))))

;; Run the example
(example-usage)
```

## Key Features of this Implementation:

1. **Data Structures**: Uses `defstruct` to define decision tree nodes with attributes like `attribute`, `children`, `class`, and `is-leaf`

2. **Core Functions**:
   - `all-same-class`: Checks if all examples have the same class
   - `get-unique-values`: Gets unique values for an attribute
   - `count-classes`: Counts class frequencies
   - `entropy`: Calculates entropy of examples
   - `information-gain`: Computes information gain for attribute selection
   - `decision-tree-learn`: Main recursive learning algorithm

3. **Algorithm Steps**:
   - If examples are empty, return default class
   - If all examples have same class, return leaf node with that class
   - If no attributes remain, return most common class
   - Otherwise, select best attribute using information gain
   - Recursively build children for each attribute value

4. **Example Dataset**: Uses the classic "Play Tennis" dataset with attributes like Outlook, Temperature, Humidity, and Wind

This implementation demonstrates the core principles of ID3 algorithm for decision tree learning in Lisp.

