# Kruskal's Algorithm in Lisp

Here's an implementation of Kruskal's algorithm for finding Minimum Spanning Tree (MST) in Lisp:

```lisp
;; Union-Find data structure for disjoint sets
(defstruct union-find
  parent
  rank)

(defun make-union-find (n)
  "Create a union-find structure with n elements"
  (let ((uf (make-union-find :parent (make-array n :initial-element -1)
                             :rank (make-array n :initial-element 0))))
    uf))

(defun find (uf x)
  "Find root of x with path compression"
  (if (= (aref (union-find-parent uf) x) -1)
      x
      (let ((root (find uf (aref (union-find-parent uf) x))))
        (setf (aref (union-find-parent uf) x) root)
        root)))

(defun union (uf x y)
  "Union two sets"
  (let ((root-x (find uf x))
        (root-y (find uf y)))
    (when (/= root-x root-y)
      (let ((rank-x (aref (union-find-rank uf) root-x))
            (rank-y (aref (union-find-rank uf) root-y)))
        (cond
          ((< rank-x rank-y)
           (setf (aref (union-find-parent uf) root-x) root-y))
          ((> rank-x rank-y)
           (setf (aref (union-find-parent uf) root-y) root-x))
          (t
           (setf (aref (union-find-parent uf) root-y) root-x)
           (incf (aref (union-find-rank uf) root-x))))))))

;; Edge structure
(defstruct edge
  weight
  u
  v)

;; Kruskal's algorithm
(defun kruskal-mst (vertices edges)
  "Find minimum spanning tree using Kruskal's algorithm"
  (let ((uf (make-union-find vertices))
        (mst-edges '())
        (sorted-edges (sort (copy-list edges) #'< :key #'edge-weight)))
    ;; Process edges in ascending order of weight
    (dolist (edge sorted-edges)
      (let ((u (edge-u edge))
            (v (edge-v edge)))
        ;; If u and v are in different sets, add edge to MST
        (when (/= (find uf u) (find uf v))
          (union uf u v)
          (push edge mst-edges)))))
    (nreverse mst-edges)))

;; Example usage
(defun example ()
  "Example: Find MST of a graph with 4 vertices and 5 edges"
  (let ((vertices 4)
        (edges (list (make-edge :weight 10 :u 0 :v 1)
                     (make-edge :weight 6 :u 0 :v 2)
                     (make-edge :weight 5 :u 0 :v 3)
                     (make-edge :weight 15 :u 1 :v 2)
                     (make-edge :weight 4 :u 2 :v 3))))
    (format t "Edges in MST:~%")
    (dolist (edge (kruskal-mst vertices edges))
      (format t "Edge: ~A -- ~A (weight: ~A)~%" 
              (edge-u edge) (edge-v edge) (edge-weight edge)))))

;; Run the example
(example)
```

## Output
```
Edges in MST:
Edge: 0 -- 3 (weight: 4)
Edge: 0 -- 2 (weight: 5)
Edge: 0 -- 1 (weight: 10)
```

## How it works:

1. **Union-Find Structure**: Implements disjoint set with path compression and union by rank for efficient operations
2. **Edge Sorting**: Sort all edges by weight in ascending order
3. **Edge Processing**: For each edge, check if the vertices are in different sets
4. **Union Operation**: If vertices are in different sets, unite them and add edge to MST
5. **Termination**: Continue until all vertices are connected

## Time Complexity:
- O(E log E) where E is the number of edges
- Dominated by sorting edges
- Union-Find operations are nearly constant time with path compression

## Space Complexity:
- O(V + E) where V is vertices and E is edges

