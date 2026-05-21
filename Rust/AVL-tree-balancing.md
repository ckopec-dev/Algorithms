# AVL Tree Balancing Algorithm in Rust

Here's a complete implementation of an AVL tree with balancing operations in Rust:

```rust
#[derive(Debug, Clone)]
pub struct AVLNode<T: Ord + Clone> {
    pub data: T,
    pub height: i32,
    pub left: Option<Box<AVLNode<T>>>,
    pub right: Option<Box<AVLNode<T>>>,
}

#[derive(Debug)]
pub struct AVLTree<T: Ord + Clone> {
    pub root: Option<Box<AVLNode<T>>>,
}

impl<T: Ord + Clone> AVLNode<T> {
    fn new(data: T) -> Self {
        AVLNode {
            data,
            height: 1,
            left: None,
            right: None,
        }
    }

    fn height(node: &Option<Box<AVLNode<T>>>) -> i32 {
        match node {
            Some(n) => n.height,
            None => 0,
        }
    }

    fn update_height(node: &mut Box<AVLNode<T>>) {
        let left_height = Self::height(&node.left);
        let right_height = Self::height(&node.right);
        node.height = 1 + std::cmp::max(left_height, right_height);
    }

    fn get_balance_factor(node: &Option<Box<AVLNode<T>>>) -> i32 {
        match node {
            Some(n) => Self::height(&n.left) - Self::height(&n.right),
            None => 0,
        }
    }
}

impl<T: Ord + Clone> AVLTree<T> {
    pub fn new() -> Self {
        AVLTree { root: None }
    }

    pub fn insert(&mut self, data: T) {
        self.root = Self::insert_node(self.root.take(), data);
    }

    fn insert_node(mut node: Option<Box<AVLNode<T>>>, data: T) -> Option<Box<AVLNode<T>>> {
        // Standard BST insertion
        match node {
            None => return Some(Box::new(AVLNode::new(data))),
            Some(mut n) => {
                if data < n.data {
                    n.left = Self::insert_node(n.left, data);
                } else if data > n.data {
                    n.right = Self::insert_node(n.right, data);
                } else {
                    // Duplicate values not allowed
                    return Some(n);
                }

                // Update height
                AVLNode::update_height(&mut n);

                // Get balance factor
                let balance = AVLNode::get_balance_factor(&Some(n.clone()));

                // Perform rotations if needed

                // Left Left Case
                if balance > 1 && data < n.left.as_ref().unwrap().data {
                    return Some(Self::rotate_right(n));
                }

                // Right Right Case
                if balance < -1 && data > n.right.as_ref().unwrap().data {
                    return Some(Self::rotate_left(n));
                }

                // Left Right Case
                if balance > 1 && data > n.left.as_ref().unwrap().data {
                    n.left = Some(Self::rotate_left(n.left.take().unwrap()));
                    return Some(Self::rotate_right(n));
                }

                // Right Left Case
                if balance < -1 && data < n.right.as_ref().unwrap().data {
                    n.right = Some(Self::rotate_right(n.right.take().unwrap()));
                    return Some(Self::rotate_left(n));
                }

                Some(n)
            }
        }
    }

    fn rotate_right(y: Box<AVLNode<T>>) -> Box<AVLNode<T>> {
        let mut x = y.left.unwrap();
        let t2 = x.right.take();

        // Perform rotation
        x.right = Some(y);
        x.right.as_mut().unwrap().left = t2;

        // Update heights
        AVLNode::update_height(&mut x.right.as_mut().unwrap());
        AVLNode::update_height(&mut x);

        x
    }

    fn rotate_left(x: Box<AVLNode<T>>) -> Box<AVLNode<T>> {
        let mut y = x.right.unwrap();
        let t2 = y.left.take();

        // Perform rotation
        y.left = Some(x);
        y.left.as_mut().unwrap().right = t2;

        // Update heights
        AVLNode::update_height(&mut y.left.as_mut().unwrap());
        AVLNode::update_height(&mut y);

        y
    }

    pub fn search(&self, data: &T) -> bool {
        Self::search_node(&self.root, data)
    }

    fn search_node(node: &Option<Box<AVLNode<T>>>, data: &T) -> bool {
        match node {
            None => false,
            Some(n) => {
                if data < &n.data {
                    Self::search_node(&n.left, data)
                } else if data > &n.data {
                    Self::search_node(&n.right, data)
                } else {
                    true
                }
            }
        }
    }

    pub fn print_inorder(&self) {
        Self::inorder_traversal(&self.root);
        println!();
    }

    fn inorder_traversal(node: &Option<Box<AVLNode<T>>>) {
        if let Some(n) = node {
            Self::inorder_traversal(&n.left);
            print!("{} ", n.data);
            Self::inorder_traversal(&n.right);
        }
    }
}

// Example usage
fn main() {
    let mut avl = AVLTree::new();
    
    // Insert elements that will cause imbalance
    println!("Inserting elements: 10, 20, 30, 40, 50, 25");
    
    avl.insert(10);
    avl.insert(20);
    avl.insert(30);
    avl.insert(40);
    avl.insert(50);
    avl.insert(25);
    
    println!("Inorder traversal of AVL tree:");
    avl.print_inorder();
    
    // Test search
    println!("Search for 25: {}", avl.search(&25));
    println!("Search for 15: {}", avl.search(&15));
}
```

## Key Components Explained:

### 1. **Node Structure**
- `data`: The value stored in the node
- `height`: Height of the node for balance calculation
- `left` and `right`: Pointers to left and right children

### 2. **Balance Operations**
- **Height Update**: `update_height()` recalculates node height
- **Balance Factor**: `get_balance_factor()` computes balance (left_height - right_height)
- **Rotations**: Four rotation types for rebalancing:
  - Right rotation (LL case)
  - Left rotation (RR case)
  - Left-Right rotation (LR case)
  - Right-Left rotation (RL case)

### 3. **Insertion Process**
1. Standard BST insertion
2. Update heights of affected nodes
3. Calculate balance factors
4. Perform necessary rotations to maintain AVL property

### 4. **Time Complexity**
- **Insertion**: O(log n)
- **Search**: O(log n)
- **Rotation**: O(1)

This implementation maintains the AVL property where the height difference between left and right subtrees is at most 1 for all nodes.

