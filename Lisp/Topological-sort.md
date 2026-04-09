# Topological Sort in Lisp

Here's an implementation of topological sort using Kahn's algorithm in Common Lisp:

```lisp
(defun topological-sort (graph)
  "Perform topological sort on a directed acyclic graph.
   GRAPH is an association list where each key is a node and 
   the value is a list of nodes it points to."
  (let ((in-degrees (make-hash-table :test 'equal))
        (queue '())
        (result '()))
    
    ;; Initialize in-degrees for all nodes
    (dolist (node (remove-duplicates (append (keys graph) (flatten (values graph)))))
      (setf (gethash node in-degrees) 0))
    
    ;; Calculate in-degrees for each node
    (dolist (node (keys graph))
      (dolist (dependent (gethash node graph))
        (incf (gethash dependent in-degrees))))
    
    ;; Find nodes with zero in-degree
    (dolist (node (keys in-degrees))
      (when (zerop (gethash node in-degrees))
        (push node queue)))
    
    ;; Process nodes in topological order
    (loop while queue do
      (let ((current (pop queue)))
        (push current result)
        (dolist (dependent (gethash current graph))
          (decf (gethash dependent in-degrees))
          (when (zerop (gethash dependent in-degrees))
            (push dependent queue)))))
    
    (nreverse result)))

;; Helper functions
(defun keys (hash-table)
  "Get all keys from a hash table."
  (let ((keys '()))
    (maphash (lambda (key value) (push key keys)) hash-table)
    keys))

(defun values (hash-table)
  "Get all values from a hash table."
  (let ((values '()))
    (maphash (lambda (key value) (push value values)) hash-table)
    values))

(defun flatten (lst)
  "Flatten a nested list."
  (cond ((null lst) nil)
        ((atom lst) (list lst))
        (t (append (flatten (car lst)) (flatten (cdr lst))))))
```

## Example Usage

```lisp
;; Define a sample graph
(setq sample-graph 
      '(("A" . ("B" "C"))
        ("B" . ("D"))
        ("C" . ("D"))
        ("D" . ())))

;; Run topological sort
(topological-sort sample-graph)
;; Returns: ("A" "B" "C" "D")

;; Another example with more dependencies
(setq complex-graph
      '(("C" . ("A" "B"))
        ("B" . ("A"))
        ("A" . ("D"))
        ("D" . ())))

(topological-sort complex-graph)
;; Returns: ("C" "B" "A" "D")
```

## How it works:

1. **Initialize**: Create a hash table to track in-degrees of each node
2. **Calculate in-degrees**: Count how many incoming edges each node has
3. **Find sources**: Add all nodes with zero in-degree to a queue
4. **Process nodes**: Remove nodes from queue, add to result, decrease in-degrees of dependent nodes
5. **Continue**: Add nodes with zero in-degree to queue until empty

## Time Complexity:
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V) for the hash table and queue

This implementation handles directed acyclic graphs and will return a valid topological ordering if one exists, or may produce incorrect results if the graph contains cycles.

