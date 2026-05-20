# Fibonacci Heap Implementation in Lisp

Here's a complete implementation of a Fibonacci heap data structure in Common Lisp:

```lisp
(defclass fibonacci-heap ()
  ((nodes :initform nil :accessor heap-nodes)
   (min :initform nil :accessor heap-min)
   (size :initform 0 :accessor heap-size)
   (consolidate-flag :initform nil :accessor heap-consolidate-flag)))

(defclass fibonacci-node ()
  ((key :initarg :key :accessor node-key)
   (value :initarg :value :accessor node-value)
   (degree :initform 0 :accessor node-degree)
   (marked :initform nil :accessor node-marked)
   (parent :initform nil :accessor node-parent)
   (child :initform nil :accessor node-child)
   (left :initform nil :accessor node-left)
   (right :initform nil :accessor node-right)))

;; Helper functions
(defun make-node (key value)
  "Create a new Fibonacci heap node."
  (let ((node (make-instance 'fibonacci-node :key key :value value)))
    (setf (node-left node) node
          (node-right node) node)
    node))

(defun link-nodes (node1 node2)
  "Link two nodes in the Fibonacci heap."
  (when (and node1 node2)
    ;; Remove node2 from its circular list
    (setf (node-right (node-left node2)) (node-right node2)
          (node-left (node-right node2)) (node-left node2))
    
    ;; Add node2 to node1's child list
    (when (node-child node1)
      (let ((child (node-child node1)))
        (setf (node-right node2) (node-right child)
              (node-left node2) child
              (node-right child) node2
              (node-left (node-right node2)) node2)))
    
    (setf (node-child node1) node2
          (node-parent node2) node1
          (node-degree node1) (1+ (node-degree node1))
          (node-marked node2) nil)))

(defun consolidate (heap)
  "Consolidate the heap by linking nodes of the same degree."
  (let ((degree-array (make-array 100 :initial-element nil)))
    (let ((current (heap-min heap)))
      (loop while (and current (not (eq current (heap-min heap))))
            do
            (let ((node current)
                  (degree (node-degree node)))
              (loop while (aref degree-array degree)
                    do
                    (let ((other (aref degree-array degree)))
                      (when (node-key other) (node-key other)) ; For debugging
                      (if (> (node-key node) (node-key other))
                          (rotatef node other))
                      (link-nodes node other)
                      (setf (aref degree-array degree) nil)
                      (incf degree)))
              (setf (aref degree-array degree) node)
              (setf current (node-right node)))))
    
    ;; Find new minimum
    (let ((new-min nil))
      (loop for i from 0 to 99
            do
            (when (aref degree-array i)
              (let ((node (aref degree-array i)))
                (when (and (null new-min) (node-key node))
                  (setf new-min node))
                (when (and new-min (node-key node) (< (node-key node) (node-key new-min)))
                  (setf new-min node)))))
      (setf (heap-min heap) new-min))))

(defun fib-heap-insert (heap key value)
  "Insert a new node into the Fibonacci heap."
  (let ((node (make-node key value)))
    (when (heap-min heap)
      (let ((min-node (heap-min heap)))
        (setf (node-right node) (node-right min-node)
              (node-left node) min-node
              (node-right min-node) node
              (node-left (node-right node)) node)
        (when (< key (node-key min-node))
          (setf (heap-min heap) node))))
    (unless (heap-min heap)
      (setf (heap-min heap) node))
    (incf (heap-size heap))
    heap))

(defun fib-heap-extract-min (heap)
  "Extract and return the minimum node from the heap."
  (let ((min-node (heap-min heap)))
    (when min-node
      ;; Remove min-node from root list
      (setf (node-right (node-left min-node)) (node-right min-node)
            (node-left (node-right min-node)) (node-left min-node))
      
      ;; Add children to root list
      (let ((child (node-child min-node)))
        (when child
          (let ((current child))
            (loop while (and current (not (eq current child)))
                  do
                  (let ((next (node-right current)))
                    (setf (node-parent current) nil)
                    (setf (node-right current) (node-right min-node)
                          (node-left current) min-node
                          (node-right min-node) current
                          (node-left (node-right current)) current)
                    (setf current next)))))
        (setf (node-child min-node) nil)))
      
      ;; Consolidate if necessary
      (when (and (node-right min-node) (not (eq min-node (node-right min-node))))
        (consolidate heap))
      
      ;; Update min
      (when (and (node-right min-node) (eq min-node (node-right min-node)))
        (setf (heap-min heap) nil))
      
      (decf (heap-size heap))
      (node-value min-node))))

(defun fib-heap-min (heap)
  "Return the minimum node's value."
  (when (heap-min heap)
    (node-value (heap-min heap))))

;; Example usage
(defun example-fibonacci-heap ()
  "Demonstrate Fibonacci heap operations."
  (let ((heap (make-instance 'fibonacci-heap)))
    (format t "Creating Fibonacci heap~%")
    
    ;; Insert elements
    (fib-heap-insert heap 10 "ten")
    (fib-heap-insert heap 5 "five")
    (fib-heap-insert heap 15 "fifteen")
    (fib-heap-insert heap 3 "three")
    (fib-heap-insert heap 8 "eight")
    
    (format t "Minimum value: ~A~%" (fib-heap-min heap))
    
    ;; Extract minimum elements
    (format t "Extracting minimums:~%")
    (loop repeat 5
          do
          (let ((min-val (fib-heap-extract-min heap)))
            (format t "Extracted: ~A~%" min-val)))
    
    (format t "Heap size: ~A~%" (heap-size heap))))

;; Run the example
(example-fibonacci-heap)
```

## Key Features of this Implementation:

1. **Node Structure**: Each node contains key, value, degree, marked flag, parent, child, and circular pointers
2. **Insert Operation**: O(1) amortized time complexity
3. **Extract-Min Operation**: O(log n) amortized time complexity
4. **Consolidation**: Maintains heap property by linking nodes of same degree
5. **Circular Lists**: Uses circular doubly-linked lists for efficient node manipulation

## Time Complexities:
- Insert: O(1) amortized
- Extract-Min: O(log n) amortized
- Find-Min: O(1)
- Union: O(1)

This implementation demonstrates the core concepts of Fibonacci heaps while maintaining the efficiency characteristics that make them superior to binary heaps in certain applications like Dijkstra's algorithm and Prim's algorithm.

