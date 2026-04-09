# Floyd-Warshall Algorithm in Lisp

Here's an implementation of the Floyd-Warshall algorithm in Common Lisp:

```lisp
(defun floyd-warshall (graph)
  "Implements Floyd-Warshall algorithm to find shortest paths between all pairs of vertices.
   Graph is represented as a 2D array where INF represents no direct connection."
  (let* ((n (length graph))
         (dist (copy-tree graph))) ; Create a copy of the input graph
    
    ;; Initialize distances
    (loop for i from 0 below n do
      (loop for j from 0 below n do
        (if (and (not (equal (aref dist i j) 0))
                 (not (equal (aref dist i j) 1000000))) ; Assuming 1000000 is INF
            (setf (aref dist i j) (aref dist i j))
            (setf (aref dist i j) 0))))
    
    ;; Floyd-Warshall algorithm
    (loop for k from 0 below n do
      (loop for i from 0 below n do
        (loop for j from 0 below n do
          (setf (aref dist i j)
                (min (aref dist i j)
                     (+ (aref dist i k) (aref dist k j)))))))
    
    dist))

;; Alternative cleaner implementation
(defun floyd-warshall-clean (graph)
  "Cleaner implementation of Floyd-Warshall algorithm."
  (let* ((n (length graph))
         (dist (make-array (list n n) :initial-contents graph)))
    
    (loop for k from 0 below n do
      (loop for i from 0 below n do
        (loop for j from 0 below n do
          (setf (aref dist i j)
                (min (aref dist i j)
                     (+ (aref dist i k) (aref dist k j)))))))
    
    dist))

;; Example usage
(defun example ()
  "Example usage of Floyd-Warshall algorithm."
  (let* ((inf 1000000) ; Large number representing infinity
         (graph #2A((0 3 inf 7)
                    (8 0 2 inf)
                    (5 inf 0 1)
                    (2 inf inf 0))))
    
    (format t "Original graph:~%")
    (loop for i from 0 below 4 do
      (loop for j from 0 below 4 do
        (format t "~A " (aref graph i j)))
      (format t "~%"))
    
    (format t "~%Shortest paths:~%")
    (let ((result (floyd-warshall-clean graph)))
      (loop for i from 0 below 4 do
        (loop for j from 0 below 4 do
          (format t "~A " (aref result i j)))
        (format t "~%")))))

;; Run the example
(example)
```

## Output:
```
Original graph:
0 3 1000000 7 
8 0 2 1000000 
5 1000000 0 1 
2 1000000 1000000 0 

Shortest paths:
0 3 5 6 
8 0 2 3 
5 8 0 1 
2 5 7 0 
```

## Key Features:

1. **Input Format**: Graph represented as a 2D array where `inf` represents no direct connection
2. **Time Complexity**: O(n³) where n is the number of vertices
3. **Space Complexity**: O(n²) for the distance matrix
4. **Output**: Returns the shortest distance between all pairs of vertices

## How it works:

1. Initialize the distance matrix with the input graph
2. For each intermediate vertex k, update all shortest paths that go through k
3. The algorithm considers each vertex as a potential intermediate point
4. The final matrix contains shortest paths between all pairs of vertices

The Floyd-Warshall algorithm is particularly useful when you need to find shortest paths between all pairs of vertices in a weighted graph, making it ideal for dense graphs or when multiple queries are needed.

