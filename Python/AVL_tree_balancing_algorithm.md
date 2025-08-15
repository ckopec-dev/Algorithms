# AVL Tree Balancing Algorithm

## Overview
An AVL tree is a self-balancing binary search tree where the difference between heights of left and right subtrees cannot be more than one for all nodes.

## Python Implementation

```python
class TreeNode:
    def __init__(self, key):
        self.key = key
        self.left = None
        self.right = None
        self.height = 1

class AVLTree:
    def get_height(self, node):
        if not node:
            return 0
        return node.height
    
    def get_balance(self, node):
        if not node:
            return 0
        return self.get_height(node.left) - self.get_height(node.right)
    
    def update_height(self, node):
        if node:
            node.height = 1 + max(self.get_height(node.left), 
                                 self.get_height(node.right))
    
    def right_rotate(self, y):
        x = y.left
        T2 = x.right
        
        # Perform rotation
        x.right = y
        y.left = T2
        
        # Update heights
        self.update_height(y)
        self.update_height(x)
        
        return x
    
    def left_rotate(self, x):
        y = x.right
        T2 = y.left
        
        # Perform rotation
        y.left = x
        x.right = T2
        
        # Update heights
        self.update_height(x)
        self.update_height(y)
        
        return y
    
    def insert(self, root, key):
        # Step 1: Perform normal BST insertion
        if not root:
            return TreeNode(key)
        
        if key < root.key:
            root.left = self.insert(root.left, key)
        elif key > root.key:
            root.right = self.insert(root.right, key)
        else:
            # Duplicate keys not allowed
            return root
        
        # Step 2: Update height of current node
        self.update_height(root)
        
        # Step 3: Get balance factor
        balance = self.get_balance(root)
        
        # Step 4: Perform rotations if unbalanced
        
        # Left Left Case
        if balance > 1 and key < root.left.key:
            return self.right_rotate(root)
        
        # Right Right Case
        if balance < -1 and key > root.right.key:
            return self.left_rotate(root)
        
        # Left Right Case
        if balance > 1 and key > root.left.key:
            root.left = self.left_rotate(root.left)
            return self.right_rotate(root)
        
        # Right Left Case
        if balance < -1 and key < root.right.key:
            root.right = self.right_rotate(root.right)
            return self.left_rotate(root)
        
        return root
    
    def pre_order(self, root):
        if not root:
            return []
        
        result = []
        result.append(root.key)
        result.extend(self.pre_order(root.left))
        result.extend(self.pre_order(root.right))
        
        return result

# Example usage
avl = AVLTree()
root = None

# Insert nodes
keys = [10, 20, 30, 40, 50, 25]
for key in keys:
    root = avl.insert(root, key)

print("Pre-order traversal of the constructed AVL tree is:")
print(avl.pre_order(root))
```

## Algorithm Steps

1. **Insertion**: Perform normal BST insertion
2. **Height Update**: Update height of all ancestors
3. **Balance Factor**: Calculate balance factor for each node
4. **Rotation**: Apply appropriate rotation if balance factor > 1 or < -1:
   - **Right Rotation** (LL case)
   - **Left Rotation** (RR case)
   - **Left-Right Rotation** (LR case)
   - **Right-Left Rotation** (RL case)

## Time Complexity
- Insertion: O(log n)
- Deletion: O(log n)
- Search: O(log n)

## Space Complexity
- O(n) where n is the number of nodes

The AVL tree maintains its balance property automatically through rotations during insertion and deletion operations.

