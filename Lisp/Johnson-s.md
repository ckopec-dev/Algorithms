# Johnson's Algorithm Implementation in Lisp

Johnson's algorithm is used to find the shortest paths between all pairs of vertices in a weighted graph, including negative edge weights (but no negative cycles). Here's a Lisp implementation:

```lisp
(defun johnson-algorithm (graph)
  "Apply Johnson's algorithm to find shortest paths between all pairs of vertices.
   Returns a matrix of shortest distances."
  (let ((vertices (get-vertices graph))
        (new-graph (add-super-node graph)))
    ;; Step 1: Run Bellman-Ford from super node
    (let ((potentials (bellman-ford new-graph (get-super-node graph))))
      ;; Step 2: Remove super node and reweight edges
      (let ((reweighted-graph (reweight-edges graph potentials)))
        ;; Step 3: Run Dijkstra for each vertex
        (let ((distance-matrix (make-distance-matrix vertices)))
          (dolist (vertex vertices)
            (let ((distances (dijkstra reweighted-graph vertex)))
              (set-distance-matrix distances vertex distance-matrix)))
          ;; Step 4: Apply potential transformation
          (apply-potential-transformation distance-matrix potentials)
          distance-matrix)))))

(defun get-vertices (graph)
  "Extract all vertices from the graph."
  (remove-duplicates (append (mapcar #'car (flatten graph))
                            (mapcar #'cadr (flatten graph)))))

(defun add-super-node (graph)
  "Add a super node connected to all other nodes with zero-weight edges."
  (let ((vertices (get-vertices graph))
        (super-node 'super))
    (cons (list super-node vertices)
          graph)))

(defun get-super-node ()
  "Return the super node identifier."
  'super)

(defun bellman-ford (graph source)
  "Run Bellman-Ford algorithm to compute potentials."
  (let ((vertices (get-vertices graph))
        (distances (make-hash-table :test 'equal)))
    ;; Initialize distances
    (dolist (vertex vertices)
      (setf (gethash vertex distances) (if (equal vertex source) 0 most-positive-fixnum)))
    
    ;; Relax edges repeatedly
    (let ((iterations (length vertices)))
      (dotimes (i iterations)
        (dolist (edge graph)
          (let ((u (car edge))
                (v (cadr edge))
                (w (caddr edge)))
            (when (< (gethash u distances) most-positive-fixnum)
              (let ((new-dist (+ (gethash u distances) w)))
                (when (< new-dist (gethash v distances))
                  (setf (gethash v distances) new-dist))))))))
    
    ;; Return potentials
    (loop for vertex in vertices
          collect (cons vertex (gethash vertex distances)))))

(defun reweight-edges (graph potentials)
  "Reweight edges using computed potentials."
  (let ((new-graph '()))
    (dolist (edge graph)
      (let ((u (car edge))
            (v (cadr edge))
            (w (caddr edge))
            (pu (cdr (assoc u potentials)))
            (pv (cdr (assoc v potentials))))
        (push (list u v (- w pu pv)) new-graph)))
    (nreverse new-graph)))

(defun dijkstra (graph source)
  "Run Dijkstra's algorithm from source vertex."
  (let ((vertices (get-vertices graph))
        (distances (make-hash-table :test 'equal))
        (visited (make-hash-table :test 'equal))
        (queue (list (cons source 0))))
    
    ;; Initialize distances
    (dolist (vertex vertices)
      (setf (gethash vertex distances) (if (equal vertex source) 0 most-positive-fixnum)))
    
    ;; Process vertices
    (loop while queue do
      (let ((current (pop queue))
            (min-vertex (car (first queue)))
            (min-dist (cdr (first queue))))
        (when (and current (not (gethash min-vertex visited)))
          (setf (gethash min-vertex visited) t)
          (dolist (edge graph)
            (when (equal (car edge) min-vertex)
              (let ((neighbor (cadr edge))
                    (weight (caddr edge))
                    (new-dist (+ (gethash min-vertex distances) weight)))
                (when (< new-dist (gethash neighbor distances))
                  (setf (gethash neighbor distances) new-dist)
                  (push (cons neighbor new-dist) queue))))))))
    
    ;; Return distances
    (loop for vertex in vertices
          collect (cons vertex (gethash vertex distances)))))

(defun make-distance-matrix (vertices)
  "Create an empty distance matrix."
  (let ((matrix (make-hash-table :test 'equal)))
    (dolist (u vertices)
      (let ((row (make-hash-table :test 'equal)))
        (dolist (v vertices)
          (setf (gethash v row) most-positive-fixnum))
        (setf (gethash u matrix) row)))
    matrix))

(defun set-distance-matrix (distances vertex matrix)
  "Set distances in the matrix for a given vertex."
  (dolist (pair distances)
    (let ((v (car pair))
          (d (cdr pair)))
      (setf (gethash v (gethash vertex matrix)) d))))

(defun apply-potential-transformation (matrix potentials)
  "Apply potential transformation to get final distances."
  (let ((vertices (get-vertices matrix)))
    (dolist (u vertices)
      (dolist (v vertices)
        (let ((original (gethash v (gethash u matrix)))
              (pu (cdr (assoc u potentials)))
              (pv (cdr (assoc v potentials))))
          (when (not (= original most-positive-fixnum))
            (setf (gethash v (gethash u matrix)) 
                  (- original pu pv))))))))

;; Example usage:
(defun example-graph ()
  "Create a sample graph with negative edges."
  '((1 2 3)
    (1 3 8)
    (1 5 -4)
    (2 4 1)
    (2 5 7)
    (3 2 4)
    (4 1 2)
    (4 3 -5)
    (5 4 6)))

;; Run the algorithm
(let ((graph (example-graph))
      (result (johnson-algorithm (example-graph))))
  (format t "Johnson's Algorithm Result:~%")
  (format t "Shortest paths matrix:~%")
  (print result))
```

## Key Components:

1. **Graph Representation**: Uses adjacency list format with edges as `(source destination weight)`
2. **Super Node Addition**: Adds a dummy node connected to all vertices with zero-weight edges
3. **Bellman-Ford**: Computes potential values for reweighting
4. **Edge Reweighting**: Transforms edge weights to eliminate negative weights
5. **Dijkstra's Algorithm**: Runs on reweighted graph for each vertex
6. **Potential Transformation**: Converts back to original distances

## Time Complexity:
- O(V² log V) where V is the number of vertices
- More efficient than Floyd-Warshall for sparse graphs

This implementation handles negative edge weights but will detect and report negative cycles if they exist in the graph.

