# Binary Search Tree Implementation in Python

```python
class TreeNode:
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

class BST:
    def __init__(self):
        self.root = None
    
    def insert(self, val):
        """Insert a value into the BST"""
        if not self.root:
            self.root = TreeNode(val)
        else:
            self._insert_recursive(self.root, val)
    
    def _insert_recursive(self, node, val):
        if val < node.val:
            if node.left is None:
                node.left = TreeNode(val)
            else:
                self._insert_recursive(node.left, val)
        elif val > node.val:
            if node.right is None:
                node.right = TreeNode(val)
            else:
                self._insert_recursive(node.right, val)
    
    def search(self, val):
        """Search for a value in the BST"""
        return self._search_recursive(self.root, val)
    
    def _search_recursive(self, node, val):
        if not node or node.val == val:
            return node
        
        if val < node.val:
            return self._search_recursive(node.left, val)
        else:
            return self._search_recursive(node.right, val)
    
    def inorder_traversal(self):
        """Return inorder traversal of the BST"""
        result = []
        self._inorder_recursive(self.root, result)
        return result
    
    def _inorder_recursive(self, node, result):
        if node:
            self._inorder_recursive(node.left, result)
            result.append(node.val)
            self._inorder_recursive(node.right, result)

# Example usage
if __name__ == "__main__":
    # Create a BST and insert values
    bst = BST()
    values = [50, 30, 70, 20, 40, 60, 80]
    
    print("Inserting values:", values)
    for val in values:
        bst.insert(val)
    
    # Search for values
    print("\nSearching for values:")
    print(f"Search 40: {'Found' if bst.search(40) else 'Not found'}")
    print(f"Search 25: {'Found' if bst.search(25) else 'Not found'}")
    
    # Inorder traversal (should give sorted order)
    print("\nInorder traversal:", bst.inorder_traversal())
```

## Output:
```
Inserting values: [50, 30, 70, 20, 40, 60, 80]

Searching for values:
Search 40: Found
Search 25: Not found

Inorder traversal: [20, 30, 40, 50, 60, 70, 80]
```

## Key Features:

- **Insert Operation**: Maintains BST property (left subtree < node < right subtree)
- **Search Operation**: Efficient O(log n) search time in balanced trees
- **Inorder Traversal**: Returns elements in sorted order
- **Recursive Implementation**: Clean and easy to understand

## Time Complexities:
- **Search**: O(log n) average, O(n) worst case
- **Insert**: O(log n) average, O(n) worst case  
- **Space**: O(n) for storing n nodes

