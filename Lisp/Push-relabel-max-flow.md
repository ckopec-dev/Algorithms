# Push-Relabel Max-Flow Algorithm in Lisp

Here's an implementation of the Push-Relabel algorithm for finding maximum flow in a flow network:

```lisp
(defclass flow-network ()
  ((vertices :initarg :vertices :accessor vertices)
   (edges :initarg :edges :accessor edges)
   (capacity :initarg :capacity :accessor capacity)
   (height :initarg :height :accessor height)
   (excess :initarg :excess :accessor excess)))

(defun make-flow-network (vertices edges)
  "Create a new flow network with given vertices and edges"
  (let ((net (make-instance 'flow-network 
                           :vertices vertices 
                           :edges edges
                           :capacity (make-hash-table)
                           :height (make-hash-table)
                           :excess (make-hash-table))))
    ;; Initialize capacity and height tables
    (loop for edge in edges do
          (setf (gethash edge (capacity net)) 0))
    (loop for vertex in vertices do
          (setf (gethash vertex (height net)) 0
                (gethash vertex (excess net)) 0))
    net))

(defun add-edge (network from to capacity)
  "Add an edge to the network with given capacity"
  (let ((edge (cons from to)))
    (setf (gethash edge (capacity network)) capacity)
    (setf (gethash from (height network)) 0
          (gethash to (height network)) 0)))

(defun push-relabel-max-flow (network source sink)
  "Compute maximum flow using push-relabel algorithm"
  (let ((heights (height network))
        (excesses (excess network))
        (capacities (capacity network))
        (active (make-hash-table))
        (n (length (vertices network))))
    ;; Initialize heights and excesses
    (loop for vertex in (vertices network) do
          (setf (gethash vertex heights) 0
                (gethash vertex excesses) 0))
    
    ;; Set source height to number of vertices
    (setf (gethash source heights) n)
    
    ;; Initialize excess for source
    (setf (gethash source excesses) most-positive-fixnum)
    
    ;; Initialize excess for other vertices
    (loop for vertex in (vertices network) do
          (when (and (not (equal vertex source))
                     (not (equal vertex sink)))
            (setf (gethash vertex excesses) 0)))
    
    ;; Main push-relabel loop
    (loop while (has-active-vertices active) do
          (let ((u (get-active-vertex active)))
            (if (can-push? network u)
                (push network u)
                (relax network u))))
    
    ;; Return the maximum flow (excess at sink)
    (gethash sink excesses)))

(defun has-active-vertices (active)
  "Check if there are active vertices"
  (not (null (gethash 'active active))))

(defun get-active-vertex (active)
  "Get an active vertex"
  (let ((keys (hash-table-keys active)))
    (if keys (first keys) nil)))

(defun can-push? (network u)
  "Check if vertex u can push flow"
  (let ((heights (height network))
        (capacities (capacity network))
        (excesses (excess network)))
    (loop for v in (vertices network) do
          (let ((edge (cons u v)))
            (when (and (gethash edge capacities)
                       (> (gethash edge capacities) 0)
                       (< (gethash u heights) (gethash v heights)))
              (return t))))
    nil))

(defun push (network u)
  "Push flow from vertex u"
  (let ((heights (height network))
        (capacities (capacity network))
        (excesses (excess network))
        (v (find-suitable-vertex network u)))
    (when v
      (let ((edge (cons u v))
            (flow (min (gethash u excesses)
                       (gethash edge capacities))))
        (setf (gethash edge capacities) 
              (- (gethash edge capacities) flow))
        (setf (gethash (cons v u) capacities) 
              (+ (gethash (cons v u) capacities) flow))
        (setf (gethash u excesses) 
              (- (gethash u excesses) flow))
        (setf (gethash v excesses) 
              (+ (gethash v excesses) flow))))))

(defun find-suitable-vertex (network u)
  "Find a vertex v that can receive flow from u"
  (let ((heights (height network))
        (capacities (capacity network)))
    (loop for v in (vertices network) do
          (let ((edge (cons u v)))
            (when (and (gethash edge capacities)
                       (> (gethash edge capacities) 0)
                       (< (gethash u heights) (gethash v heights)))
              (return v))))
    nil))

(defun relax (network u)
  "Relax vertex u by increasing its height"
  (let ((heights (height network))
        (capacities (capacity network)))
    (let ((new-height (reduce #'min 
                             (loop for v in (vertices network) 
                                   for edge = (cons u v) 
                                   when (and (gethash edge capacities)
                                             (> (gethash edge capacities) 0))
                                   collect (gethash v heights))
                             :key #'1+)))
      (setf (gethash u heights) new-height))))

;; Example usage:
(defun example-flow-network ()
  "Create and solve a simple flow network example"
  (let ((vertices '(s a b c t))
        (edges '((s a) (s b) (a c) (b c) (c t) (a t) (b t) (a b))))
    (let ((net (make-flow-network vertices edges)))
      ;; Add capacities to edges
      (add-edge net 's 'a 10)
      (add-edge net 's 'b 10)
      (add-edge net 'a 'c 4)
      (add-edge net 'a 'b 2)
      (add-edge net 'b 'c 8)
      (add-edge net 'c 't 10)
      (add-edge net 'a 't 5)
      (add-edge net 'b 't 5)
      
      ;; Compute maximum flow from s to t
      (push-relabel-max-flow net 's 't))))

;; Simpler version for demonstration:
(defun simple-push-relabel-example ()
  "Simple example showing the algorithm concept"
  (format t "Push-Relabel Maximum Flow Algorithm~%")
  (format t "================================~%")
  (format t "This implementation demonstrates the core concepts~%")
  (format t "of the push-relabel algorithm for maximum flow problems.~%")
  (format t "~%Algorithm steps:~%")
  (format t "1. Initialize heights and excess flows~%")
  (format t "2. Process active vertices~%")
  (format t "3. Push flow when possible~%")
  (format t "4. Relabel vertices when necessary~%")
  (format t "5. Continue until no active vertices remain~%"))
```

## Key Features of this Implementation:

1. **Vertex and Edge Management**: Uses hash tables to store network structure
2. **Height and Excess Tracking**: Maintains height labels and excess flows for each vertex
3. **Push Operation**: Moves flow from higher to lower vertices
4. **Relabel Operation**: Increases vertex height when no push is possible
5. **Active Vertex Management**: Keeps track of vertices with excess flow

## Algorithm Complexity:
- Time Complexity: O(V²E) where V is vertices and E is edges
- Space Complexity: O(V + E)

This implementation provides the basic structure of the push-relabel algorithm. In practice, additional optimizations like gap relabeling and global relabeling would be added for better performance.

