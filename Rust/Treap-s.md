# Treap Implementation in Rust

```rust
use std::rc::Rc;
use std::cell::RefCell;
use std::cmp::Ordering;

#[derive(Debug, Clone)]
struct Node {
    key: i32,
    priority: u32,
    left: Option<Rc<RefCell<Node>>>,
    right: Option<Rc<RefCell<Node>>>,
}

impl Node {
    fn new(key: i32, priority: u32) -> Self {
        Node {
            key,
            priority,
            left: None,
            right: None,
        }
    }
}

struct Treap {
    root: Option<Rc<RefCell<Node>>>,
}

impl Treap {
    fn new() -> Self {
        Treap { root: None }
    }

    fn rotate_right(node: Rc<RefCell<Node>>) -> Rc<RefCell<Node>> {
        let mut node_ref = node.borrow_mut();
        let left_child = node_ref.left.take().unwrap();
        let mut left_ref = left_child.borrow_mut();
        
        node_ref.left = left_ref.right.take();
        left_ref.right = Some(node);
        
        drop(left_ref);
        drop(node_ref);
        
        left_child
    }

    fn rotate_left(node: Rc<RefCell<Node>>) -> Rc<RefCell<Node>> {
        let mut node_ref = node.borrow_mut();
        let right_child = node_ref.right.take().unwrap();
        let mut right_ref = right_child.borrow_mut();
        
        node_ref.right = right_ref.left.take();
        right_ref.left = Some(node);
        
        drop(right_ref);
        drop(node_ref);
        
        right_child
    }

    fn insert(&mut self, key: i32, priority: u32) {
        self.root = Self::insert_node(self.root.take(), key, priority);
    }

    fn insert_node(
        root: Option<Rc<RefCell<Node>>>,
        key: i32,
        priority: u32,
    ) -> Option<Rc<RefCell<Node>>> {
        match root {
            None => Some(Rc::new(RefCell::new(Node::new(key, priority)))),
            Some(node) => {
                let mut node_ref = node.borrow_mut();
                match key.cmp(&node_ref.key) {
                    Ordering::Less => {
                        node_ref.left = Self::insert_node(node_ref.left.take(), key, priority);
                        let left_child = node_ref.left.as_ref().unwrap().clone();
                        if left_child.borrow().priority > node_ref.priority {
                            drop(node_ref);
                            return Some(Self::rotate_right(node));
                        }
                    }
                    Ordering::Greater => {
                        node_ref.right = Self::insert_node(node_ref.right.take(), key, priority);
                        let right_child = node_ref.right.as_ref().unwrap().clone();
                        if right_child.borrow().priority > node_ref.priority {
                            drop(node_ref);
                            return Some(Self::rotate_left(node));
                        }
                    }
                    Ordering::Equal => {
                        // Key already exists, don't insert duplicate
                        drop(node_ref);
                        return Some(node);
                    }
                }
                drop(node_ref);
                Some(node)
            }
        }
    }

    fn search(&self, key: i32) -> bool {
        Self::search_node(&self.root, key)
    }

    fn search_node(root: &Option<Rc<RefCell<Node>>>, key: i32) -> bool {
        match root {
            None => false,
            Some(node) => {
                let node_ref = node.borrow();
                match key.cmp(&node_ref.key) {
                    Ordering::Less => Self::search_node(&node_ref.left, key),
                    Ordering::Greater => Self::search_node(&node_ref.right, key),
                    Ordering::Equal => true,
                }
            }
        }
    }

    fn inorder_traversal(&self) -> Vec<i32> {
        let mut result = Vec::new();
        Self::inorder_helper(&self.root, &mut result);
        result
    }

    fn inorder_helper(root: &Option<Rc<RefCell<Node>>>, result: &mut Vec<i32>) {
        if let Some(node) = root {
            let node_ref = node.borrow();
            Self::inorder_helper(&node_ref.left, result);
            result.push(node_ref.key);
            Self::inorder_helper(&node_ref.right, result);
        }
    }

    fn print_tree(&self) {
        if let Some(root) = &self.root {
            Self::print_node(root, "", true);
        } else {
            println!("Empty tree");
        }
    }

    fn print_node(node: &Rc<RefCell<Node>>, prefix: &str, is_last: bool) {
        if let Some(node_ref) = node.as_ref().borrow().left.as_ref() {
            Self::print_node(node_ref, &format!("{}{}", prefix, if is_last { "    " } else { "│   " }), false);
        }

        let node_ref = node.borrow();
        println!("{}{}", prefix, if is_last { "└── " } else { "├── " });
        println!("{}Key: {}, Priority: {}", prefix, node_ref.key, node_ref.priority);

        if let Some(node_ref) = node.as_ref().borrow().right.as_ref() {
            Self::print_node(node_ref, &format!("{}{}", prefix, if is_last { "    " } else { "│   " }), true);
        }
    }
}

fn main() {
    let mut treap = Treap::new();
    
    // Insert elements with different priorities
    println!("Inserting elements into Treap:");
    treap.insert(10, 5);
    treap.insert(5, 15);
    treap.insert(15, 10);
    treap.insert(3, 20);
    treap.insert(7, 8);
    treap.insert(12, 12);
    treap.insert(18, 3);
    
    println!("Treap structure:");
    treap.print_tree();
    
    println!("\nInorder traversal (should be sorted):");
    let inorder = treap.inorder_traversal();
    println!("{:?}", inorder);
    
    println!("\nSearching for elements:");
    println!("Search 7: {}", treap.search(7));
    println!("Search 20: {}", treap.search(20));
    println!("Search 15: {}", treap.search(15));
    
    // Insert duplicate key (should not be inserted)
    println!("\nInserting duplicate key 10:");
    treap.insert(10, 100);
    println!("Inorder traversal after duplicate insertion:");
    println!("{:?}", treap.inorder_traversal());
}
```

## Key Features of this Treap Implementation:

1. **Node Structure**: Each node contains key, priority, and left/right child pointers
2. **Rotation Operations**: Implements left and right rotations to maintain heap property
3. **Insertion**: Maintains both BST property (keys) and heap property (priorities)
4. **Search**: Standard binary search tree search
5. **Traversal**: Inorder traversal for sorted output
6. **Visualization**: Pretty-print tree structure for debugging

## Time Complexities:
- **Insertion**: O(log n) average, O(n) worst case
- **Search**: O(log n) average, O(n) worst case
- **Space**: O(n) for n nodes

## Key Properties:
- **BST Property**: For any node, all keys in left subtree are smaller, all keys in right subtree are larger
- **Heap Property**: For any node, priority is greater than or equal to both children's priorities
- **Randomization**: Priority values are typically assigned randomly to ensure good expected performance

