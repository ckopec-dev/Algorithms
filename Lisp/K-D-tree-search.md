# K-D Tree Search Algorithm in Lisp

Here's an implementation of a K-D tree search algorithm in Common Lisp:

```lisp
;; Define a K-D tree node structure
(defstruct kdtree-node
  (point nil)           ; The point stored in this node
  (left nil)            ; Left subtree
  (right nil)           ; Right subtree
  (split-dimension 0))   ; Dimension used for splitting

;; Create a new K-D tree node
(defun make-kdtree-node (point left right split-dimension)
  (make-kdtree-node :point point
                    :left left
                    :right right
                    :split-dimension split-dimension))

;; Calculate Euclidean distance between two points
(defun euclidean-distance (point1 point2)
  (let ((sum 0))
    (loop for i from 0 below (length point1)
          do (let ((diff (- (aref point1 i) (aref point2 i))))
               (incf sum (* diff diff))))
    (sqrt sum)))

;; Build K-D tree from a list of points
(defun build-kdtree (points &optional dimension)
  "Build a K-D tree from a list of points"
  (if (null points)
      nil
    (let* ((n (length points))
           (split-dim (or dimension 0))
           (sorted-points (sort (copy-sequence points)
                               (lambda (a b)
                                 (< (aref a split-dim) (aref b split-dim)))))
           (median (floor n 2))
           (median-point (aref sorted-points median))
           (left-points (subseq sorted-points 0 median))
           (right-points (subseq sorted-points (1+ median))))
      (make-kdtree-node 
       :point median-point
       :left (build-kdtree left-points (mod (1+ split-dim) (length (first points))))
       :right (build-kdtree right-points (mod (1+ split-dim) (length (first points))))
       :split-dimension split-dim))))

;; K-D tree nearest neighbor search
(defun kdtree-search-nearest (root target-point)
  "Find the nearest neighbor to target-point in the K-D tree"
  (if (null root)
      nil
    (let ((best-node root)
          (best-distance (euclidean-distance (kdtree-node-point root) target-point)))
      (kdtree-search-nearest-helper root target-point best-node best-distance))))

(defun kdtree-search-nearest-helper (node target-point best-node best-distance)
  "Helper function for nearest neighbor search"
  (if (null node)
      (values best-node best-distance)
    (let* ((current-point (kdtree-node-point node))
           (current-distance (euclidean-distance current-point target-point))
           (split-dim (kdtree-node-split-dimension node))
           (split-value (aref current-point split-dim))
           (target-value (aref target-point split-dim))
           (left-child (kdtree-node-left node))
           (right-child (kdtree-node-right node))
           (new-best-node best-node)
           (new-best-distance best-distance))
      ;; Update best if current node is closer
      (when (< current-distance best-distance)
        (setf new-best-node node
              new-best-distance current-distance))
      
      ;; Determine which subtree to search first
      (let ((search-first (if (< target-value split-value)
                              left-child
                              right-child))
            (search-second (if (< target-value split-value)
                               right-child
                               left-child)))
        ;; Search the first subtree
        (multiple-value-bind (best1-node best1-dist)
            (kdtree-search-nearest-helper search-first target-point new-best-node new-best-distance)
          ;; Check if we need to search the second subtree
          (let ((distance-to-hyperplane (abs (- target-value split-value))))
            (if (< distance-to-hyperplane best1-dist)
                (multiple-value-bind (best2-node best2-dist)
                    (kdtree-search-nearest-helper search-second target-point best1-node best1-dist)
                  (if (< best2-dist best1-dist)
                      (values best2-node best2-dist)
                      (values best1-node best1-dist)))
                (values best1-node best1-dist))))))))

;; Example usage
(defun example-kdtree-search ()
  "Example of using the K-D tree search"
  (let* ((points #((2 3) (5 4) (9 6) (4 7) (8 1) (7 2)))
         (tree (build-kdtree points))
         (target-point (make-array 2 (list 3 5))))
    (format t "Points: ~A~%" points)
    (format t "Target point: ~A~%" target-point)
    (multiple-value-bind (nearest-node nearest-distance)
        (kdtree-search-nearest tree target-point)
      (format t "Nearest neighbor: ~A~%" (kdtree-node-point nearest-node))
      (format t "Distance: ~A~%" nearest-distance))))

;; Run the example
(example-kdtree-search)
```

## Key Features of this Implementation:

1. **K-D Tree Node Structure**: Uses `defstruct` to define nodes with point data, left/right children, and split dimension
2. **Distance Calculation**: Implements Euclidean distance between points
3. **Tree Building**: Recursively builds the tree by sorting points along alternating dimensions
4. **Nearest Neighbor Search**: Implements the optimized search algorithm that prunes branches when not needed
5. **Optimization**: Only searches the second subtree if the hyperplane distance is smaller than the current best distance

## Time Complexity:
- **Building**: O(n log n) average case
- **Search**: O(log n) average case, O(n) worst case

This implementation demonstrates the core concepts of K-D tree search in a functional Lisp style while maintaining good performance characteristics.

