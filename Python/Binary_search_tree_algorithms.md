```python
class TreeNode:
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

def insert(root, val):
    """Insert a value into BST"""
    if not root:
        return TreeNode(val)
    
    if val < root.val:
        root.left = insert(root.left, val)
    elif val > root.val:
        root.right = insert(root.right, val)
    
    return root

def search(root, val):
    """Search for a value in BST"""
    if not root or root.val == val:
        return root
    
    if val < root.val:
        return search(root.left, val)
    else:
        return search(root.right, val)

def find_min(root):
    """Find minimum value in BST"""
    while root.left:
        root = root.left
    return root.val

def find_max(root):
    """Find maximum value in BST"""
    while root.right:
        root = root.right
    return root.val

def delete(root, val):
    """Delete a value from BST"""
    if not root:
        return root
    
    if val < root.val:
        root.left = delete(root.left, val)
    elif val > root.val:
        root.right = delete(root.right, val)
    else:
        # Node to be deleted found
        if not root.left:
            return root.right
        elif not root.right:
            return root.left
        
        # Node with two children: Get inorder successor
        min_val = find_min(root.right)
        root.val = min_val
        root.right = delete(root.right, min_val)
    
    return root

def inorder_traversal(root):
    """Inorder traversal (returns sorted list)"""
    result = []
    if root:
        result.extend(inorder_traversal(root.left))
        result.append(root.val)
        result.extend(inorder_traversal(root.right))
    return result

# Example usage
if __name__ == "__main__":
    # Create BST: 50, 30, 70, 20, 40, 60, 80
    root = None
    values = [50, 30, 70, 20, 40, 60, 80]
    
    for val in values:
        root = insert(root, val)
    
    print("Inorder traversal:", inorder_traversal(root))
    # Output: [20, 30, 40, 50, 60, 70, 80]
    
    # Search for value
    result = search(root, 40)
    print("Search 40:", result.val if result else None)
    # Output: 40
    
    # Find min and max
    print("Minimum value:", find_min(root))
    # Output: 20
    print("Maximum value:", find_max(root))
    # Output: 80
    
    # Delete a node
    root = delete(root, 30)
    print("After deleting 30:", inorder_traversal(root))
    # Output: [20, 40, 50, 60, 70, 80]
```

This implementation demonstrates:

1. **TreeNode class** - Basic structure for BST nodes
2. **Insert function** - Adds new values maintaining BST property
3. **Search function** - Finds a value in the tree
4. **Find minimum/maximum** - Gets extreme values efficiently
5. **Delete function** - Removes nodes handling all cases (leaf, one child, two children)
6. **Inorder traversal** - Returns values in sorted order

Key BST properties maintained:
- Left subtree contains only values less than root
- Right subtree contains only values greater than root
- All operations have O(log n) average time complexity for balanced trees

