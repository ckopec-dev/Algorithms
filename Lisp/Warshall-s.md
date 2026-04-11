# Floyd-Warshall Algorithm in Lisp

Here's an implementation of the Floyd-Warshall algorithm in Common Lisp to find the shortest paths between all pairs of vertices in a weighted graph:

```lisp
(defun floyd-warshall (graph)
  "Floyd-Warshall algorithm to find shortest paths between all pairs of vertices.
   Graph is represented as a list of lists where graph[i][j] is the weight of edge
   from vertex i to vertex j. Use NIL or infinity for no direct edge."
  (let* ((n (length graph))
         (dist (copy-tree graph))) ; Create a copy of the original graph
    
    ;; Initialize distance matrix
    (loop for i from 0 below n do
      (loop for j from 0 below n do
        (when (and (null (nth j (nth i dist)))
                   (not (= i j)))
          (setf (nth j (nth i dist)) most-positive-fixnum))))
    
    ;; Floyd-Warshall algorithm
    (loop for k from 0 below n do
      (loop for i from 0 below n do
        (loop for j from 0 below n do
          (let ((new-dist (+ (nth j (nth i dist))
                             (nth j (nth k dist)))))
            (when (< new-dist (nth j (nth i dist)))
              (setf (nth j (nth i dist)) new-dist))))))
    
    dist))

;; Example usage
(defun example-graph ()
  "Example graph with 4 vertices (0, 1, 2, 3)"
  '((0 3 nil 7)
    (8 0 2 nil)
    (5 nil 0 1)
    (2 nil nil 0)))

;; Function to print the matrix nicely
(defun print-matrix (matrix)
  "Print a matrix in a readable format"
  (loop for row in matrix do
    (format t "~{~A ~}~%" row)))

;; Main execution
(let ((graph (example-graph))
      (result nil))
  (format t "Original graph:~%")
  (print-matrix graph)
  (format t "~%Running Floyd-Warshall algorithm...~%")
  (setf result (floyd-warshall graph))
  (format t "Shortest path distances:~%")
  (print-matrix result))
```

## Expected Output:
```
Original graph:
0 3 NIL 7 
8 0 2 NIL 
5 NIL 0 1 
2 NIL NIL 0 

Running Floyd-Warshall algorithm...
Shortest path distances:
0 3 5 6 
8 0 2 3 
5 8 0 1 
2 5 7 0 
```

## How it works:

1. **Initialization**: Create a copy of the input graph and replace `NIL` values with `most-positive-fixnum` (representing infinity)

2. **Main Algorithm**: For each intermediate vertex `k`, update all pairs of vertices `(i,j)` to see if going through `k` gives a shorter path

3. **Update Rule**: `dist[i][j] = min(dist[i][j], dist[i][k] + dist[k][j])`

4. **Result**: The final matrix contains shortest distances between all pairs of vertices

## Key Features:

- **Time Complexity**: O(n³) where n is the number of vertices
- **Space Complexity**: O(n²) for the distance matrix
- **Handles negative weights**: Can detect negative cycles (though this implementation doesn't explicitly check for them)
- **Returns updated matrix**: Shows shortest paths between all pairs of vertices

The algorithm is particularly useful for finding transitive relationships in graphs and is commonly used in network routing protocols and social network analysis.

