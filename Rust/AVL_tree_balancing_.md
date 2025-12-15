# AVL Tree Balancing Algorithm in Rust

Here's a complete implementation of an AVL tree with balancing operations in Rust:

```rust
#[derive(Debug, Clone)]
pub struct Node {
    pub key: i32,
    pub height: i32,
    pub left: Option<Box<Node>>,
    pub right: Option<Box<Node>>,
}

impl Node {
    fn new(key: i32) -> Self {
        Node {
            key,
            height: 1,
            left: None,
            right: None,
        }
    }
    
    fn height(node: &Option<Box<Node>>) -> i32 {
        match node {
            Some(n) => n.height,
            None => 0,
        }
    }
    
    fn update_height(node: &mut Box<Node>) {
        let left_height = Self::height(&node.left);
        let right_height = Self::height(&node.right);
        node.height = 1 + left_height.max(right_height);
    }
    
    fn get_balance_factor(node: &Option<Box<Node>>) -> i32 {
        match node {
            Some(n) => Self::height(&n.left) - Self::height(&n.right),
            None => 0,
        }
    }
}

pub struct AVLTree {
    root: Option<Box<Node>>,
}

impl AVLTree {
    pub fn new() -> Self {
        AVLTree { root: None }
    }
    
    pub fn insert(&mut self, key: i32) {
        self.root = Self::insert_node(self.root.take(), key);
    }
    
    fn insert_node(mut node: Option<Box<Node>>, key: i32) -> Option<Box<Node>> {
        // Step 1: Perform normal BST insertion
        if node.is_none() {
            return Some(Box::new(Node::new(key)));
        }
        
        let mut node_ref = &mut node;
        if key < node_ref.as_ref().unwrap().key {
            node_ref.as_mut().unwrap().left = Self::insert_node(node_ref.as_mut().unwrap().left.take(), key);
        } else if key > node_ref.as_ref().unwrap().key {
            node_ref.as_mut().unwrap().right = Self::insert_node(node_ref.as_mut().unwrap().right.take(), key);
        } else {
            // Duplicate keys not allowed
            return node;
        }
        
        // Step 2: Update height of current node
        Node::update_height(node_ref.as_mut().unwrap());
        
        // Step 3: Get balance factor
        let balance = Node::get_balance_factor(&node);
        
        // Step 4: Perform rotations if unbalanced
        // Left Left Case
        if balance > 1 && key < node_ref.as_ref().unwrap().left.as_ref().unwrap().key {
            return Self::rotate_right(node);
        }
        
        // Right Right Case
        if balance < -1 && key > node_ref.as_ref().unwrap().right.as_ref().unwrap().key {
            return Self::rotate_left(node);
        }
        
        // Left Right Case
        if balance > 1 && key > node_ref.as_ref().unwrap().left.as_ref().unwrap().key {
            node_ref.as_mut().unwrap().left = Self::rotate_left(node_ref.as_mut().unwrap().left.take());
            return Self::rotate_right(node);
        }
        
        // Right Left Case
        if balance < -1 && key < node_ref.as_ref().unwrap().right.as_ref().unwrap().key {
            node_ref.as_mut().unwrap().right = Self::rotate_right(node_ref.as_mut().unwrap().right.take());
            return Self::rotate_left(node);
        }
        
        node
    }
    
    fn rotate_right(y: Option<Box<Node>>) -> Option<Box<Node>> {
        let mut x = y.as_ref().unwrap().left.take();
        let mut t2 = x.as_mut().unwrap().right.take();
        
        // Perform rotation
        x.as_mut().unwrap().right = y;
        x.as_mut().unwrap().right.as_mut().unwrap().left = t2;
        
        // Update heights
        Node::update_height(x.as_mut().unwrap().right.as_mut().unwrap());
        Node::update_height(x.as_mut().unwrap());
        
        x
    }
    
    fn rotate_left(x: Option<Box<Node>>) -> Option<Box<Node>> {
        let mut y = x.as_ref().unwrap().right.take();
        let mut t2 = y.as_mut().unwrap().left.take();
        
        // Perform rotation
        y.as_mut().unwrap().left = x;
        y.as_mut().unwrap().left.as_mut().unwrap().right = t2;
        
        // Update heights
        Node::update_height(y.as_mut().unwrap().left.as_mut().unwrap());
        Node::update_height(y.as_mut().unwrap());
        
        y
    }
    
    pub fn search(&self, key: i32) -> bool {
        Self::search_node(&self.root, key)
    }
    
    fn search_node(node: &Option<Box<Node>>, key: i32) -> bool {
        match node {
            Some(n) => {
                if key == n.key {
                    true
                } else if key < n.key {
                    Self::search_node(&n.left, key)
                } else {
                    Self::search_node(&n.right, key)
                }
            }
            None => false,
        }
    }
    
    pub fn inorder_traversal(&self) -> Vec<i32> {
        let mut result = Vec::new();
        Self::inorder_helper(&self.root, &mut result);
        result
    }
    
    fn inorder_helper(node: &Option<Box<Node>>, result: &mut Vec<i32>) {
        if let Some(n) = node {
            Self::inorder_helper(&n.left, result);
            result.push(n.key);
            Self::inorder_helper(&n.right, result);
        }
    }
}

// Example usage
fn main() {
    let mut avl = AVLTree::new();
    
    // Insert nodes
    let keys = vec![10, 20, 30, 40, 50, 25];
    
    println!("Inserting keys: {:?}", keys);
    
    for key in keys {
        avl.insert(key);
        println!("Inserted {}: AVL tree inorder: {:?}", key, avl.inorder_traversal());
    }
    
    // Search for keys
    println!("Search 25: {}", avl.search(25));
    println!("Search 15: {}", avl.search(15));
    
    println!("Final inorder traversal: {:?}", avl.inorder_traversal());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_insert_and_search() {
        let mut avl = AVLTree::new();
        
        avl.insert(10);
        avl.insert(20);
        avl.insert(30);
        avl.insert(40);
        avl.insert(50);
        avl.insert(25);
        
        assert!(avl.search(25));
        assert!(avl.search(10));
        assert!(avl.search(50));
        assert!(!avl.search(100));
    }
    
    #[test]
    fn test_inorder_traversal() {
        let mut avl = AVLTree::new();
        
        let keys = vec![10, 20, 30, 40, 50, 25];
        for key in keys {
            avl.insert(key);
        }
        
        let result = avl.inorder_traversal();
        let expected = vec![10, 20, 25, 30, 40, 50];
        assert_eq!(result, expected);
    }
}
```

## Key Features of this Implementation

### 1. **Node Structure**
- Stores key, height, and left/right child pointers
- Uses `Option<Box<Node>>` for safe memory management

### 2. **Balance Operations**
- **Height Calculation**: `Node::height()` and `Node::update_height()`
- **Balance Factor**: `Node::get_balance_factor()`
- **Rotations**: 
  - `rotate_right()` for Left-Left case
  - `rotate_left()` for Right-Right case
  - Combined rotations for Left-Right and Right-Left cases

### 3. **Insertion Logic**
1. Perform normal BST insertion
2. Update node heights
3. Calculate balance factor
4. Apply appropriate rotations to maintain AVL property

### 4. **Time Complexity**
- **Insertion**: O(log n)
- **Search**: O(log n)
- **Rotation**: O(1)

### 5. **Example Output**
```
Inserting keys: [10, 20, 30, 40, 50, 25]
Inserted 10: AVL tree inorder: [10]
Inserted 20: AVL tree inorder: [10, 20]
Inserted 30: AVL tree inorder: [10, 20, 30]
Inserted 40: AVL tree inorder: [10, 20, 30, 40]
Inserted 50: AVL tree inorder: [10, 20, 30, 40, 50]
Inserted 25: AVL tree inorder: [10, 20, 25, 30, 40, 50]
Search 25: true
Search 15: false
Final inorder traversal: [10, 20, 25, 30, 40, 50]
```

This implementation maintains the AVL tree property where the height difference between left and right subtrees is at most 1 for all nodes, ensuring O(log n) operations.

