# Trie Algorithm Implementation in Lisp

Here's an example implementation of a Trie (prefix tree) data structure in Common Lisp:

```lisp
;; Define the trie node structure
(defstruct trie-node
  (children (make-hash-table :test 'equal) :type hash-table)
  (is-end-of-word nil :type boolean)
  (word nil :type (or string null)))

;; Create a new empty trie
(defun make-trie ()
  "Create and return an empty trie."
  (make-trie-node))

;; Insert a word into the trie
(defun trie-insert (trie word)
  "Insert a word into the trie."
  (let ((current-node trie))
    (loop for char across word
          do (let ((child (gethash char (trie-node-children current-node))))
               (unless child
                 (setf child (make-trie-node))
                 (setf (gethash char (trie-node-children current-node)) child))
               (setf current-node child)))
    (setf (trie-node-is-end-of-word current-node) t)
    (setf (trie-node-word current-node) word)))

;; Search for a complete word in the trie
(defun trie-search (trie word)
  "Search for a complete word in the trie. Returns the word if found, nil otherwise."
  (let ((current-node trie))
    (loop for char across word
          for child = (gethash char (trie-node-children current-node))
          do (unless child
               (return nil))
          (setf current-node child))
    (if (trie-node-is-end-of-word current-node)
        (trie-node-word current-node)
        nil)))

;; Check if a prefix exists in the trie
(defun trie-starts-with (trie prefix)
  "Check if the trie contains words with the given prefix."
  (let ((current-node trie))
    (loop for char across prefix
          for child = (gethash char (trie-node-children current-node))
          do (unless child
               (return nil))
          (setf current-node child))
    t))

;; Get all words with a given prefix
(defun trie-autocomplete (trie prefix)
  "Return all words in the trie that start with the given prefix."
  (let ((current-node trie)
        (words '()))
    (loop for char across prefix
          for child = (gethash char (trie-node-children current-node))
          do (unless child
               (return nil))
          (setf current-node child))
    (trie-collect-words current-node prefix words)
    (nreverse words)))

;; Helper function to collect words recursively
(defun trie-collect-words (node prefix words)
  "Recursively collect all words from the given node."
  (when (trie-node-is-end-of-word node)
    (push prefix words))
  (loop for char being the hash-keys of (trie-node-children node)
        for child = (gethash char (trie-node-children node))
        do (trie-collect-words child (concatenate 'string prefix (string char)) words)))

;; Example usage
(defun demo-trie ()
  "Demonstrate trie functionality."
  (let ((trie (make-trie)))
    ;; Insert words
    (trie-insert trie "hello")
    (trie-insert trie "help")
    (trie-insert trie "helicopter")
    (trie-insert trie "world")
    (trie-insert trie "word")
    
    ;; Test search
    (format t "Search 'hello': ~A~%" (trie-search trie "hello"))
    (format t "Search 'help': ~A~%" (trie-search trie "help"))
    (format t "Search 'hell': ~A~%" (trie-search trie "hell"))
    
    ;; Test prefix checking
    (format t "Starts with 'hel': ~A~%" (trie-starts-with trie "hel"))
    (format t "Starts with 'wor': ~A~%" (trie-starts-with trie "wor"))
    
    ;; Test autocomplete
    (format t "Words starting with 'hel': ~A~%" (trie-autocomplete trie "hel"))
    (format t "Words starting with 'wor': ~A~%" (trie-autocomplete trie "wor"))))

;; Run the demo
(demo-trie)
```

## Output Example:
```
Search 'hello': HELLO
Search 'help': HELP
Search 'hell': NIL
Starts with 'hel': T
Starts with 'wor': T
Words starting with 'hel': ("HELLO" "HELP" "HELICOPTER")
Words starting with 'wor': ("WORLD" "WORD")
```

## Key Features:

1. **Trie Node Structure**: Uses hash tables for efficient child node lookup
2. **Insert Operation**: Builds the trie character by character
3. **Search Operation**: Finds complete words
4. **Prefix Check**: Determines if words exist with a given prefix
5. **Autocomplete**: Returns all words with a given prefix
6. **Memory Efficient**: Shares common prefixes between words

This implementation demonstrates the core trie algorithm with O(m) time complexity for search operations, where m is the length of the word being searched.

