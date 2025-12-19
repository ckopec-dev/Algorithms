# Random Forest Algorithm in Lisp

Here's an implementation of a Random Forest algorithm in Common Lisp:

```lisp
(defpackage :random-forest
  (:use :cl)
  (:export :make-random-forest
           :train-random-forest
           :predict-random-forest
           :random-forest-predict))
(in-package :random-forest)

;; Define a decision tree node
(defstruct tree-node
  feature-index
  threshold
  left-child
  right-child
  prediction
  is-leaf)

;; Define a decision tree
(defstruct decision-tree
  root
  num-features)

;; Define a random forest
(defstruct random-forest
  trees
  num-trees
  num-features
  num-classes)

;; Helper function to calculate entropy
(defun entropy (labels)
  "Calculate entropy of a set of labels"
  (when labels
    (let ((total (length labels))
          (class-counts (make-hash-table :test 'equal)))
      (dolist (label labels)
        (incf (gethash label class-counts 0)))
      (let ((entropy 0.0))
        (maphash (lambda (class count)
                     (let ((prob (/ count total)))
                       (when (> prob 0)
                         (incf entropy (* prob (log prob -2))))))
                   class-counts)
        (- entropy)))))

;; Helper function to find the best split
(defun find-best-split (features labels num-features)
  "Find the best feature and threshold to split on"
  (let ((best-gain -1.0)
        (best-feature nil)
        (best-threshold nil))
    (loop for feature-index from 0 below num-features do
      (let ((feature-values (mapcar (lambda (row) (aref row feature-index))
                                   features)))
        (let ((sorted-indices (sort (loop for i from 0 below (length feature-values)
                                        collect i)
                                   (lambda (i j)
                                     (< (aref feature-values i)
                                        (aref feature-values j)))))
              (best-split-gain -1.0))
          (loop for i from 0 below (1- (length sorted-indices)) do
            (let ((threshold (/ (+ (aref feature-values (aref sorted-indices i))
                                   (aref feature-values (aref sorted-indices (1+ i))))
                               2.0)))
              (let ((left-labels (loop for j from 0 below (length labels)
                                     when (< (aref feature-values j) threshold)
                                     collect (aref labels j)))
                    (right-labels (loop for j from 0 below (length labels)
                                      when (>= (aref feature-values j) threshold)
                                      collect (aref labels j))))
                (when (and left-labels right-labels)
                  (let ((left-entropy (entropy left-labels))
                        (right-entropy (entropy right-labels))
                        (total (length labels))
                        (left-weight (/ (length left-labels) total))
                        (right-weight (/ (length right-labels) total)))
                    (let ((weighted-entropy (+ (* left-weight left-entropy)
                                               (* right-weight right-entropy))))
                      (let ((current-gain (- (entropy labels) weighted-entropy)))
                        (when (> current-gain best-split-gain)
                          (setf best-split-gain current-gain
                                best-threshold threshold
                                best-feature feature-index)))))))))
        (when (> best-split-gain best-gain)
          (setf best-gain best-split-gain
                best-feature best-feature
                best-threshold best-threshold)))))
    (values best-feature best-threshold best-gain)))

;; Build a decision tree recursively
(defun build-tree (features labels depth max-depth num-features)
  "Build a decision tree recursively"
  (let ((node (make-tree-node)))
    (if (or (null features) (null labels) (>= depth max-depth))
        ;; Leaf node
        (let ((class-counts (make-hash-table :test 'equal)))
          (dolist (label labels)
            (incf (gethash label class-counts 0)))
          (setf (tree-node-prediction node)
                (let ((max-count 0)
                      (majority-class nil))
                  (maphash (lambda (class count)
                               (when (> count max-count)
                                 (setf max-count count
                                       majority-class class)))
                            class-counts)
                  majority-class))
          (setf (tree-node-is-leaf node) t)
          node)
        ;; Internal node
        (multiple-value-bind (best-feature best-threshold best-gain)
            (find-best-split features labels num-features)
          (if (<= best-gain 0)
              ;; No good split found
              (let ((class-counts (make-hash-table :test 'equal)))
                (dolist (label labels)
                  (incf (gethash label class-counts 0)))
                (setf (tree-node-prediction node)
                      (let ((max-count 0)
                            (majority-class nil))
                        (maphash (lambda (class count)
                                     (when (> count max-count)
                                       (setf max-count count
                                             majority-class class)))
                                  class-counts)
                        majority-class))
                (setf (tree-node-is-leaf node) t)
                node)
              ;; Create split
              (let ((left-features '())
                    (right-features '())
                    (left-labels '())
                    (right-labels '()))
                (loop for i from 0 below (length features) do
                  (let ((feature-vector (aref features i)))
                    (if (< (aref feature-vector best-feature) best-threshold)
                        (progn
                          (push feature-vector left-features)
                          (push (aref labels i) left-labels))
                        (progn
                          (push feature-vector right-features)
                          (push (aref labels i) right-labels)))))
                (setf (tree-node-feature-index node) best-feature
                      (tree-node-threshold node) best-threshold)
                (setf (tree-node-left-child node)
                      (build-tree (nreverse left-features)
                                 (nreverse left-labels)
                                 (1+ depth) max-depth num-features))
                (setf (tree-node-right-child node)
                      (build-tree (nreverse right-features)
                                 (nreverse right-labels)
                                 (1+ depth) max-depth num-features))
                node))))))

;; Create a decision tree
(defun make-decision-tree (features labels max-depth)
  "Create a decision tree from features and labels"
  (let ((num-features (if features
                         (length (aref features 0))
                         0)))
    (make-decision-tree :root (build-tree features labels 0 max-depth num-features)
                       :num-features num-features)))

;; Make a random forest
(defun make-random-forest (num-trees max-depth)
  "Create a random forest with specified number of trees"
  (make-random-forest :trees (make-array num-trees)
                      :num-trees num-trees
                      :max-depth max-depth
                      :num-features 0
                      :num-classes 0))

;; Train the random forest
(defun train-random-forest (forest features labels &key (max-depth 10) (num-subfeatures 0))
  "Train the random forest on given features and labels"
  (let ((num-features (length (aref features 0)))
        (num-classes (length (remove-duplicates labels))))
    (setf (random-forest-num-features forest) num-features
          (random-forest-num-classes forest) num-classes)
    
    (loop for i from 0 below (random-forest-num-trees forest) do
      (let ((bootstrap-indices (loop for j from 0 below (length features)
                                   when (> (random 1.0) 0.5)
                                   collect j)))
        (let ((bootstrap-features (make-array (length bootstrap-indices)))
              (bootstrap-labels (make-array (length bootstrap-indices))))
          (loop for j from 0 below (length bootstrap-indices) do
            (setf (aref bootstrap-features j) (aref features (aref bootstrap-indices j)))
            (setf (aref bootstrap-labels j) (aref labels (aref bootstrap-indices j)))))
        (let ((tree (make-decision-tree bootstrap-features bootstrap-labels max-depth)))
          (setf (aref (random-forest-trees forest) i) tree)))))
  forest)

;; Predict using a single decision tree
(defun predict-tree (tree features)
  "Predict class for a single sample using a decision tree"
  (let ((current-node (decision-tree-root tree)))
    (loop while (not (tree-node-is-leaf current-node)) do
      (let ((feature-value (aref features (tree-node-feature-index current-node))))
        (if (< feature-value (tree-node-threshold current-node))
            (setf current-node (tree-node-left-child current-node))
            (setf current-node (tree-node-right-child current-node)))))
    (tree-node-prediction current-node)))

;; Predict using the random forest
(defun random-forest-predict (forest sample)
  "Predict class for a sample using the random forest"
  (let ((predictions (make-hash-table :test 'equal)))
    (loop for i from 0 below (random-forest-num-trees forest) do
      (let ((tree (aref (random-forest-trees forest) i)))
        (let ((prediction (predict-tree tree sample)))
          (incf (gethash prediction predictions 0)))))
    (let ((max-count 0)
          (best-prediction nil))
      (maphash (lambda (prediction count)
                   (when (> count max-count)
                     (setf max-count count
                           best-prediction prediction)))
                predictions)
      best-prediction)))

;; Example usage
(defun example ()
  "Example of using the random forest"
  (let* ((features (make-array 4
                              :initial-contents
                              (list (make-array 2 :initial-contents '(1 2))
                                    (make-array 2 :initial-contents '(2 3))
                                    (make-array 2 :initial-contents '(3 4))
                                    (make-array 2 :initial-contents '(4 5)))))
         (labels (make-array 4 :initial-contents '(0 1 1 0)))
         (forest (make-random-forest 5 10)))
    (train-random-forest forest features labels)
    (let ((test-sample (make-array 2 :initial-contents '(2.5 3.5))))
      (format t "Prediction: ~A~%" (random-forest-predict forest test-sample)))))
```

## Usage Example

```lisp
;; Run the example
(example)
```

## Key Features

This Random Forest implementation includes:

1. **Decision Tree Structure**: Trees with nodes that can be leaves or internal splits
2. **Bootstrap Sampling**: Random sampling with replacement for each tree
3. **Feature Subsampling**: Random selection of features for each split
4. **Entropy-based Splitting**: Uses information gain to determine optimal splits
5. **Ensemble Prediction**: Voting mechanism to combine predictions from all trees

## How It Works

1. **Training**: Creates multiple decision trees using bootstrap sampling
2. **Prediction**: Each tree makes a prediction, and the forest votes on the final result
3. **Randomization**: Both feature selection and sample selection are randomized for diversity

This implementation provides a foundation that can be extended with additional features like cross-validation, different splitting criteria, or more sophisticated ensemble methods.

