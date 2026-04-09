# Treap Implementation in Python

A treap is a binary search tree that maintains both the BST property and a heap property. Each node has a key (for BST) and a priority (for heap). The tree is ordered by keys in BST fashion and by priorities in heap fashion.

```python
import random

class TreapNode:
    def __init__(self, key, priority=None):
        self.key = key
        self.priority = priority if priority is not None else random.randint(1, 1000)
        self.left = None
        self.right = None

class Treap:
    def __init__(self):
        self.root = None
    
    def _rotate_right(self, y):
        """Right rotation"""
        x = y.left
        T2 = x.right
        
        # Perform rotation
        x.right = y
        y.left = T2
        
        return x
    
    def _rotate_left(self, x):
        """Left rotation"""
        y = x.right
        T2 = y.left
        
        # Perform rotation
        y.left = x
        x.right = T2
        
        return y
    
    def _insert(self, node, key):
        """Internal insert function with heap property maintenance"""
        if not node:
            return TreapNode(key)
        
        if key < node.key:
            node.left = self._insert(node.left, key)
            
            # Heap property violation - rotate right
            if node.left and node.left.priority > node.priority:
                node = self._rotate_right(node)
        else:
            node.right = self._insert(node.right, key)
            
            # Heap property violation - rotate left
            if node.right and node.right.priority > node.priority:
                node = self._rotate_left(node)
        
        return node
    
    def insert(self, key):
        """Insert a key into the treap"""
        self.root = self._insert(self.root, key)
    
    def _search(self, node, key):
        """Search for a key in the treap"""
        if not node or node.key == key:
            return node
        
        if key < node.key:
            return self._search(node.left, key)
        else:
            return self._search(node.right, key)
    
    def search(self, key):
        """Search for a key in the treap"""
        return self._search(self.root, key)
    
    def _delete(self, node, key):
        """Internal delete function"""
        if not node:
            return node
        
        if key < node.key:
            node.left = self._delete(node.left, key)
        elif key > node.key:
            node.right = self._delete(node.right, key)
        else:
            # Node to be deleted found
            if not node.left:
                return node.right
            elif not node.right:
                return node.left
            
            # Node with two children
            # Get the inorder successor (smallest in right subtree)
            if node.left.priority > node.right.priority:
                node = self._rotate_right(node)
                node.right = self._delete(node.right, key)
            else:
                node = self._rotate_left(node)
                node.left = self._delete(node.left, key)
        
        return node
    
    def delete(self, key):
        """Delete a key from the treap"""
        self.root = self._delete(self.root, key)
    
    def _inorder(self, node, result):
        """Inorder traversal for printing"""
        if node:
            self._inorder(node.left, result)
            result.append((node.key, node.priority))
            self._inorder(node.right, result)
    
    def inorder(self):
        """Return inorder traversal of the treap"""
        result = []
        self._inorder(self.root, result)
        return result
    
    def print_tree(self):
        """Print the treap structure"""
        def _print_tree(node, level=0, prefix="Root: "):
            if node is not None:
                print(" " * (level * 4) + prefix + f"Key: {node.key}, Priority: {node.priority}")
                if node.left is not None or node.right is not None:
                    if node.left:
                        _print_tree(node.left, level + 1, "L--- ")
                    else:
                        print(" " * ((level + 1) * 4) + "L--- None")
                    if node.right:
                        _print_tree(node.right, level + 1, "R--- ")
                    else:
                        print(" " * ((level + 1) * 4) + "R--- None")
        
        if self.root is not None:
            _print_tree(self.root)
        else:
            print("Empty treap")

# Example usage
if __name__ == "__main__":
    # Create a treap
    treap = Treap()
    
    # Insert keys
    keys = [50, 30, 70, 20, 40, 60, 80]
    print("Inserting keys:", keys)
    
    for key in keys:
        treap.insert(key)
    
    print("\nTreap structure after insertion:")
    treap.print_tree()
    
    print("\nInorder traversal (key, priority):")
    print(treap.inorder())
    
    # Search for keys
    print("\nSearching for keys:")
    print(f"Search 40: {'Found' if treap.search(40) else 'Not found'}")
    print(f"Search 25: {'Found' if treap.search(25) else 'Not found'}")
    
    # Delete a key
    print("\nDeleting key 30:")
    treap.delete(30)
    print("Treap structure after deletion:")
    treap.print_tree()
    
    print("\nInorder traversal after deletion:")
    print(treap.inorder())
```

## Key Features of This Treap Implementation:

1. **Node Structure**: Each node contains key, priority, and left/right child pointers
2. **Insertion**: Maintains both BST property (keys) and heap property (priorities)
3. **Rotation Operations**: Left and right rotations to maintain heap property
4. **Search**: Standard BST search with O(log n) average complexity
5. **Deletion**: Complex operation that maintains both properties
6. **Visualization**: Methods to print the tree structure

## Time Complexities:
- **Search**: O(log n) average, O(n) worst case
- **Insert**: O(log n) average, O(n) worst case  
- **Delete**: O(log n) average, O(n) worst case

The treap combines the benefits of binary search trees and heaps, making it useful for dynamic sets where we need both fast search and fast insertion/deletion operations.

