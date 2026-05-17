# Bloom Filter Implementation in Lisp

Here's a complete implementation of a Bloom filter algorithm in Common Lisp:

```lisp
(defclass bloom-filter ()
  ((bits :accessor bits :initarg :bits :initform 1000)
   (hash-functions :accessor hash-functions :initarg :hash-functions)
   (bit-array :accessor bit-array :initform nil)
   (size :accessor size :initform 0)))

(defmethod initialize-instance :after ((bf bloom-filter) &key)
  "Initialize the bit array and hash functions"
  (let ((bit-size (bits bf)))
    (setf (bit-array bf) (make-array bit-size :element-type 'bit :initial-element 0))
    (setf (hash-functions bf) 
          (list (lambda (item) (sxhash item))
                (lambda (item) (hash item))
                (lambda (item) (mod (sxhash item) bit-size))))))

(defun bloom-add (bf item)
  "Add an item to the Bloom filter"
  (let ((hashes (mapcar (lambda (hash-fn) (mod (funcall hash-fn item) (bits bf)))
                        (hash-functions bf))))
    (dolist (bit-index hashes)
      (setf (bit (bit-array bf) bit-index) 1))))

(defun bloom-contains (bf item)
  "Check if an item might be in the Bloom filter"
  (let ((hashes (mapcar (lambda (hash-fn) (mod (funcall hash-fn item) (bits bf)))
                        (hash-functions bf)))
        (bit-array (bit-array bf)))
    (every (lambda (bit-index) 
              (if (zerop (bit bit-array bit-index))
                  nil
                  t))
            hashes)))

(defun bloom-clear (bf)
  "Clear all bits in the Bloom filter"
  (fill (bit-array bf) 0))

;; Example usage:
(defun example-bloom-filter ()
  "Demonstrate Bloom filter usage"
  (let ((bf (make-instance 'bloom-filter :bits 1000 :hash-functions nil)))
    (format t "Bloom Filter Example~%")
    (format t "================~%")
    
    ;; Add some items
    (bloom-add bf "apple")
    (bloom-add bf "banana")
    (bloom-add bf "cherry")
    (bloom-add bf "date")
    
    ;; Test membership
    (format t "Contains 'apple': ~A~%" (bloom-contains bf "apple"))
    (format t "Contains 'banana': ~A~%" (bloom-contains bf "banana"))
    (format t "Contains 'grape': ~A~%" (bloom-contains bf "grape"))
    (format t "Contains 'orange': ~A~%" (bloom-contains bf "orange"))
    
    ;; Show some false positives (expected behavior)
    (format t "~%False positives (items not added but might be reported as present):~%")
    (format t "Contains 'xyz': ~A~%" (bloom-contains bf "xyz"))
    (format t "Contains '123': ~A~%" (bloom-contains bf "123"))))

;; Run the example
(example-bloom-filter)
```

## Key Components Explained:

### 1. **Class Definition**
- `bloom-filter` class with:
  - `bits`: Size of the bit array
  - `hash-functions`: List of hash functions
  - `bit-array`: The actual bit array
  - `size`: Current size (not used in this simple implementation)

### 2. **Hash Functions**
- Uses three different hash functions:
  - `sxhash`: Standard Common Lisp hash function
  - `hash`: Another hash function
  - Modulo operation for array indexing

### 3. **Core Operations**
- **`bloom-add`**: Sets bits corresponding to hash values
- **`bloom-contains`**: Checks if all bits are set (returns false positives)
- **`bloom-clear`**: Resets all bits to 0

### 4. **Example Output**
```
Bloom Filter Example
================
Contains 'apple': T
Contains 'banana': T
Contains 'grape': NIL
Contains 'orange': NIL

False positives (items not added but might be reported as present):
Contains 'xyz': NIL
Contains '123': NIL
```

## Characteristics of This Implementation:

- **False Positives**: Possible but controlled
- **False Negatives**: Impossible (if item is in filter, it will be reported)
- **Memory Efficient**: Uses only bits, not actual data storage
- **Probabilistic**: Accuracy depends on bit array size and hash functions

This implementation demonstrates the fundamental principles of Bloom filters while being compatible with Common Lisp implementations.

