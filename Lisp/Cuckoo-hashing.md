# Cuckoo Hashing in Lisp

Here's an implementation of the Cuckoo hashing algorithm in Common Lisp:

```lisp
(defclass cuckoo-hash-table ()
  ((table1 :initarg :table1 :accessor table1)
   (table2 :initarg :table2 :accessor table2)
   (size :initarg :size :accessor size)
   (hash-functions :initarg :hash-functions :accessor hash-functions)
   (count :initarg :count :accessor count)))

(defun make-cuckoo-hash-table (size)
  "Create a new cuckoo hash table with given size"
  (let ((table1 (make-array size :initial-element nil))
        (table2 (make-array size :initial-element nil))
        (hash-fns (list (lambda (key size) (mod (sxhash key) size))
                        (lambda (key size) (mod (hash key) size)))))
    (make-instance 'cuckoo-hash-table
                   :table1 table1
                   :table2 table2
                   :size size
                   :hash-functions hash-fns
                   :count 0)))

(defun cuckoo-insert (table key value)
  "Insert key-value pair into cuckoo hash table"
  (let ((size (size table))
        (table1 (table1 table))
        (table2 (table2 table))
        (hash-fns (hash-functions table)))
    (loop
      with current-key = key
      with current-value = value
      with current-table = table1
      with current-index = (funcall (first hash-fns) key size)
      with depth = 0
      while (< depth 100)  ; Prevent infinite loop
      do
        (let ((old-key (aref current-table current-index)))
          (if (null old-key)
              (progn
                (setf (aref current-table current-index) 
                      (cons current-key current-value))
                (incf (count table))
                (return))
              ;; Evict existing key and continue
              (let ((old-value (cdr (aref current-table current-index)))))
                (setf (aref current-table current-index) 
                      (cons current-key current-value))
                (setf current-key old-key)
                (setf current-value old-value)
                (setf current-table (if (eq current-table table1) table2 table1))
                (setf current-index (funcall (first hash-fns) current-key size))
                (incf depth)))
      finally (error "Cuckoo hashing failed - too many relocations"))))

(defun cuckoo-find (table key)
  "Find value for given key in cuckoo hash table"
  (let ((size (size table))
        (table1 (table1 table))
        (table2 (table2 table))
        (hash-fns (hash-functions table)))
    (let ((index1 (funcall (first hash-fns) key size))
          (index2 (funcall (second hash-fns) key size)))
      (let ((entry1 (aref table1 index1))
            (entry2 (aref table2 index2))))
        (if (and entry1 (eq (car entry1) key))
            (cdr entry1)
            (if (and entry2 (eq (car entry2) key))
                (cdr entry2)
                nil))))))

(defun cuckoo-delete (table key)
  "Delete key from cuckoo hash table"
  (let ((size (size table))
        (table1 (table1 table))
        (table2 (table2 table))
        (hash-fns (hash-functions table)))
    (let ((index1 (funcall (first hash-fns) key size))
          (index2 (funcall (second hash-fns) key size))))
      (let ((entry1 (aref table1 index1))
            (entry2 (aref table2 index2))))
        (if (and entry1 (eq (car entry1) key))
            (progn
              (setf (aref table1 index1) nil)
              (decf (count table))
              t)
            (if (and entry2 (eq (car entry2) key))
                (progn
                  (setf (aref table2 index2) nil)
                  (decf (count table))
                  t)
                nil))))))

;; Example usage:
(defun example-cuckoo-hash ()
  "Demonstrate cuckoo hash table usage"
  (let ((cht (make-cuckoo-hash-table 8)))
    (format t "Created cuckoo hash table of size ~A~%" (size cht))
    
    ;; Insert some values
    (cuckoo-insert cht 'apple 'red)
    (cuckoo-insert cht 'banana 'yellow)
    (cuckoo-insert cht 'cherry 'red)
    (cuckoo-insert cht 'date 'brown)
    
    ;; Find values
    (format t "Found apple: ~A~%" (cuckoo-find cht 'apple))
    (format t "Found banana: ~A~%" (cuckoo-find cht 'banana))
    
    ;; Delete a value
    (cuckoo-delete cht 'banana)
    (format t "Found banana after deletion: ~A~%" (cuckoo-find cht 'banana))
    
    (format t "Table count: ~A~%" (count cht))))

;; Run the example
(example-cuckoo-hash)
```

## Key Features of this Implementation:

1. **Two Hash Tables**: Uses two separate hash tables to store elements
2. **Two Hash Functions**: Employs two different hash functions for collision resolution
3. **Cuckoo Relocation**: When a collision occurs, the existing element is evicted and reinserted
4. **Cycle Detection**: Prevents infinite loops with a maximum relocation depth
5. **Basic Operations**: Insert, find, and delete functionality

## How Cuckoo Hashing Works:

1. Each key is hashed using two different hash functions
2. The key is placed in one of the two possible positions
3. If the position is occupied, the existing key is evicted and moved to its alternative position
4. This process continues until either the key is placed or a maximum number of relocations is reached
5. If relocations exceed the limit, the table may need to be resized or rehashed

This implementation provides a foundation for understanding how cuckoo hashing works, though a production version would include more sophisticated error handling and optimization techniques.

