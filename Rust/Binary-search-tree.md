# Binary Search Tree in Rust

Here's a complete implementation of a Binary Search Tree in Rust with common operations:

```rust
// Define the structure for a tree node
#[derive(Debug, Clone)]
struct TreeNode {
    value: i32,
    left: Option<Box<TreeNode>>,
    right: Option<Box<TreeNode>>,
}

impl TreeNode {
    // Create a new tree node
    fn new(value: i32) -> Self {
        TreeNode {
            value,
            left: None,
            right: None,
        }
    }
}

// Binary Search Tree implementation
struct BinarySearchTree {
    root: Option<Box<TreeNode>>,
}

impl BinarySearchTree {
    // Create a new empty BST
    fn new() -> Self {
        BinarySearchTree { root: None }
    }

    // Insert a value into the BST
    fn insert(&mut self, value: i32) {
        self.root = Self::insert_recursive(self.root.take(), value);
    }

    // Recursive helper for insertion
    fn insert_recursive(node: Option<Box<TreeNode>>, value: i32) -> Option<Box<TreeNode>> {
        match node {
            None => Some(Box::new(TreeNode::new(value))),
            Some(mut current_node) => {
                if value < current_node.value {
                    current_node.left = Self::insert_recursive(current_node.left, value);
                } else if value > current_node.value {
                    current_node.right = Self::insert_recursive(current_node.right, value);
                }
                // If value equals current_node.value, we don't insert (no duplicates)
                Some(current_node)
            }
        }
    }

    // Search for a value in the BST
    fn search(&self, value: i32) -> bool {
        Self::search_recursive(&self.root, value)
    }

    // Recursive helper for search
    fn search_recursive(node: &Option<Box<TreeNode>>, value: i32) -> bool {
        match node {
            None => false,
            Some(current_node) => {
                if value == current_node.value {
                    true
                } else if value < current_node.value {
                    Self::search_recursive(&current_node.left, value)
                } else {
                    Self::search_recursive(&current_node.right, value)
                }
            }
        }
    }

    // In-order traversal (returns values in sorted order)
    fn inorder_traversal(&self) -> Vec<i32> {
        let mut result = Vec::new();
        Self::inorder_recursive(&self.root, &mut result);
        result
    }

    // Recursive helper for in-order traversal
    fn inorder_recursive(node: &Option<Box<TreeNode>>, result: &mut Vec<i32>) {
        if let Some(current_node) = node {
            Self::inorder_recursive(&current_node.left, result);
            result.push(current_node.value);
            Self::inorder_recursive(&current_node.right, result);
        }
    }

    // Find minimum value in the BST
    fn find_min(&self) -> Option<i32> {
        let mut current = &self.root;
        while let Some(node) = current {
            if node.left.is_none() {
                return Some(node.value);
            }
            current = &node.left;
        }
        None
    }

    // Find maximum value in the BST
    fn find_max(&self) -> Option<i32> {
        let mut current = &self.root;
        while let Some(node) = current {
            if node.right.is_none() {
                return Some(node.value);
            }
            current = &node.right;
        }
        None
    }
}

// Example usage
fn main() {
    let mut bst = BinarySearchTree::new();
    
    // Insert values
    println!("Inserting values: 50, 30, 70, 20, 40, 60, 80");
    bst.insert(50);
    bst.insert(30);
    bst.insert(70);
    bst.insert(20);
    bst.insert(40);
    bst.insert(60);
    bst.insert(80);
    
    // Search for values
    println!("Searching for 40: {}", bst.search(40)); // true
    println!("Searching for 25: {}", bst.search(25)); // false
    
    // In-order traversal (should print sorted values)
    println!("In-order traversal: {:?}", bst.inorder_traversal());
    
    // Find min and max
    println!("Minimum value: {:?}", bst.find_min()); // Some(20)
    println!("Maximum value: {:?}", bst.find_max()); // Some(80)
}
```

## Output
```
Inserting values: 50, 30, 70, 20, 40, 60, 80
Searching for 40: true
Searching for 25: false
In-order traversal: [20, 30, 40, 50, 60, 70, 80]
Minimum value: Some(20)
Maximum value: Some(80)
```

## Key Features

1. **Insert Operation**: Maintains BST property (left subtree < node < right subtree)
2. **Search Operation**: Efficient O(log n) search time in balanced trees
3. **In-order Traversal**: Returns values in sorted order
4. **Find Min/Max**: Efficiently finds minimum and maximum values
5. **Memory Management**: Uses `Box` for heap allocation and `Option` for optional children

## Time Complexities
- **Search**: O(log n) average, O(n) worst case
- **Insert**: O(log n) average, O(n) worst case
- **Traversal**: O(n)

This implementation demonstrates Rust's ownership system, pattern matching, and safe memory management while providing a complete BST data structure.

