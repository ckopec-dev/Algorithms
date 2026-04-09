# Skip List Search Algorithm in Lisp

Here's an implementation of a skip list search algorithm in Common Lisp:

```lisp
(defclass skip-node ()
  ((value :initarg :value :accessor node-value)
   (forward :initarg :forward :accessor node-forward)
   (level :initarg :level :accessor node-level)))

(defclass skip-list ()
  ((header :initarg :header :accessor skip-list-header)
   (max-level :initarg :max-level :accessor skip-list-max-level)
   (current-level :initarg :current-level :accessor skip-list-current-level)))

(defun make-skip-node (value level)
  "Create a new skip list node with given value and level"
  (make-instance 'skip-node 
                :value value 
                :forward (make-array level :initial-element nil)
                :level level))

(defun make-skip-list (max-level)
  "Create a new skip list with given maximum level"
  (make-instance 'skip-list
                :header (make-skip-node nil max-level)
                :max-level max-level
                :current-level 0))

(defun skip-list-search (skip-list value)
  "Search for a value in the skip list and return the node if found, nil otherwise"
  (let ((current (skip-list-header skip-list))
        (level (skip-list-current-level skip-list)))
    ;; Start from the highest level and move down
    (loop while (>= level 0) do
      (let ((next (aref (node-forward current) level)))
        ;; Move forward while next node's value is less than target
        (loop while (and next (< (node-value next) value)) do
          (setf current next
                next (aref (node-forward current) level))))
        ;; If we found the exact value, return the node
        (when (and next (= (node-value next) value))
          (return-from skip-list-search next))
        ;; Move down to the next level
        (decf level)))
    ;; Value not found
    nil))

(defun skip-list-insert (skip-list value)
  "Insert a value into the skip list"
  (let ((update (make-array (skip-list-max-level skip-list) :initial-element nil))
        (current (skip-list-header skip-list))
        (level (skip-list-current-level skip-list)))
    ;; Find the position where value should be inserted
    (loop while (>= level 0) do
      (let ((next (aref (node-forward current) level)))
        (loop while (and next (< (node-value next) value)) do
          (setf current next
                next (aref (node-forward current) level))))
        (setf (aref update level) current)
        (decf level)))
    
    ;; Generate a random level for the new node
    (let ((new-level (random-level (skip-list-max-level skip-list))))
      ;; Update the current level if needed
      (when (> new-level (skip-list-current-level skip-list))
        (setf (skip-list-current-level skip-list) new-level))
      
      ;; Create new node
      (let ((new-node (make-skip-node value new-level)))
        ;; Link the new node into the list
        (loop for i from 0 below new-level do
          (setf (aref (node-forward new-node) i) 
                (aref (node-forward (aref update i)) i)
                (aref (node-forward (aref update i)) i) new-node))
        new-node))))

(defun random-level (max-level)
  "Generate a random level for a new node"
  (let ((level 0))
    (loop while (and (< level max-level) (zerop (random 2))) do
      (incf level))
    level))

;; Example usage:
;; (let ((list (make-skip-list 16)))
;;   (skip-list-insert list 3)
;;   (skip-list-insert list 6)
;;   (skip-list-insert list 7)
;;   (skip-list-insert list 9)
;;   (skip-list-insert list 12)
;;   (skip-list-insert list 19)
;;   (skip-list-insert list 17)
;;   (skip-list-insert list 26)
;;   (let ((result (skip-list-search list 19)))
;;     (if result
;;         (format t "Found: ~A~%" (node-value result))
;;         (format t "Not found~%"))))

```

## How the Search Algorithm Works:

1. **Start at the highest level** of the skip list
2. **Traverse forward** at each level while the next node's value is less than the target
3. **Move down** to the next lower level when the target value is reached or when we can't move further
4. **Check for exact match** at each level
5. **Return the node** if found, or `nil` if not found

## Time Complexity:
- **Average case**: O(log n)
- **Worst case**: O(n)
- **Space complexity**: O(n log n)

The search algorithm efficiently navigates through the skip list by using the multiple layers to skip over large portions of the list, similar to how you might skip through a sorted array using a jump search approach.

