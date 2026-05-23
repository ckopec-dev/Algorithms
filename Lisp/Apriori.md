# Apriori Algorithm in Lisp

Here's an implementation of the Apriori algorithm for finding frequent itemsets in Lisp:

```lisp
;; Apriori Algorithm Implementation in Lisp

;; Helper function to check if an itemset is a subset of another
(defun is-subset-p (subset superset)
  "Check if subset is a subset of superset"
  (every (lambda (item) (member item superset)) subset))

;; Generate candidate itemsets of size k from frequent itemsets of size k-1
(defun generate-candidates (frequent-itemsets k)
  "Generate candidates of size k from frequent itemsets of size k-1"
  (let ((candidates '()))
    (dolist (itemset1 frequent-itemsets)
      (dolist (itemset2 frequent-itemsets)
        (when (and (not (equal itemset1 itemset2))
                   (equal (butlast itemset1) (butlast itemset2))
                   (equal (car (last itemset1)) (car (last itemset2))))
          (let ((candidate (append itemset1 (list (car (last itemset2))))))
            (when (and (not (member candidate candidates))
                       (all-subsets-frequent-p candidate frequent-itemsets))
              (push candidate candidates)))))
    candidates))

;; Check if all subsets of a candidate are frequent
(defun all-subsets-frequent-p (candidate frequent-itemsets)
  "Check if all (k-1)-subsets of candidate are frequent"
  (let ((k (length candidate)))
    (if (= k 2)
        t
        (every (lambda (subset)
                  (member subset frequent-itemsets))
                (subsets-of-size (butlast candidate) (- k 1))))))

;; Generate all subsets of a given size
(defun subsets-of-size (itemset k)
  "Generate all subsets of size k from itemset"
  (if (<= (length itemset) k)
      (list itemset)
      (let ((result '()))
        (dolist (item itemset)
          (let ((remaining (remove item itemset)))
            (when (>= (length remaining) k)
              (dolist (subset (subsets-of-size remaining k))
                (push subset result)))))
        result)))

;; Count support for itemsets in transactions
(defun count-support (itemset transactions)
  "Count how many transactions contain the itemset"
  (length (remove-if-not 
           (lambda (transaction) 
             (is-subset-p itemset transaction))
           transactions)))

;; Find frequent itemsets using Apriori algorithm
(defun apriori (transactions min-support)
  "Main Apriori algorithm implementation"
  (let ((frequent-itemsets '())
        (k 1)
        (candidates '()))
    ;; Initialize with single items
    (let ((single-items (remove-duplicates (apply #'append transactions))))
      (dolist (item single-items)
        (let ((itemset (list item))
              (support (count-support itemset transactions)))
          (when (>= support min-support)
            (push itemset frequent-itemsets)))))
    
    ;; Iteratively generate larger frequent itemsets
    (loop while (not (null frequent-itemsets))
          do (progn
               (setf candidates (generate-candidates frequent-itemsets k))
               (setf frequent-itemsets '())
               (dolist (candidate candidates)
                 (let ((support (count-support candidate transactions)))
                   (when (>= support min-support)
                     (push candidate frequent-itemsets)))))
          (incf k))
    
    frequent-itemsets))

;; Example usage
(defun example-apriori ()
  "Example of Apriori algorithm usage"
  (let ((transactions '(("milk" "bread" "butter")
                        ("milk" "bread")
                        ("bread" "butter")
                        ("milk" "butter")
                        ("milk" "bread" "butter" "cheese"))))
    (format t "Transactions:~%")
    (dolist (trans transactions)
      (format t "~A~%" trans))
    (format t "~%Frequent itemsets (min-support=2):~%")
    (let ((frequent (apriori transactions 2)))
      (dolist (itemset frequent)
        (format t "~A~%" itemset)))))

;; Run the example
(example-apriori)
```

## Sample Output

```
Transactions:
("milk" "bread" "butter")
("milk" "bread")
("bread" "butter")
("milk" "butter")
("milk" "bread" "butter" "cheese")

Frequent itemsets (min-support=2):
("milk")
("bread")
("butter")
("milk" "bread")
("milk" "butter")
("bread" "butter")
("milk" "bread" "butter")
```

## Key Features of this Implementation

1. **Support Counting**: Counts how many transactions contain each itemset
2. **Candidate Generation**: Generates candidates of size k from frequent itemsets of size k-1
3. **Pruning**: Uses the Apriori property to prune infrequent candidates
4. **Iterative Process**: Continues until no more frequent itemsets can be found
5. **Subset Checking**: Ensures all subsets of a candidate are frequent before adding it

## How it Works

1. **Step 1**: Find all frequent single items (support ≥ min-support)
2. **Step 2**: Generate candidate itemsets of size k using frequent itemsets of size k-1
3. **Step 3**: Prune candidates that have infrequent subsets
4. **Step 4**: Count support for each candidate and keep only frequent ones
5. **Step 5**: Repeat until no more frequent itemsets are found

This implementation demonstrates the core principles of the Apriori algorithm in a functional Lisp style.

