# Morris Traversal in Rust

Morris traversal is an algorithm for traversing binary trees without using recursion or a stack. It uses the tree's structure itself to keep track of the traversal path.

```rust
use std::rc::Rc;
use std::cell::RefCell;

// Definition for a binary tree node
#[derive(Debug, Clone)]
pub struct TreeNode {
    pub val: i32,
    pub left: Option<Rc<RefCell<TreeNode>>>,
    pub right: Option<Rc<RefCell<TreeNode>>>,
}

impl TreeNode {
    #[inline]
    pub fn new(val: i32) -> Self {
        TreeNode {
            val,
            left: None,
            right: None,
        }
    }
}

pub struct MorrisTraversal;

impl MorrisTraversal {
    /// Morris traversal for inorder traversal
    pub fn inorder_traversal(root: Option<Rc<RefCell<TreeNode>>>) -> Vec<i32> {
        let mut result = Vec::new();
        let mut current = root;
        
        while let Some(node) = current {
            let node_ref = node.borrow();
            
            // If left is None, visit current node and go to right
            if node_ref.left.is_none() {
                result.push(node_ref.val);
                current = node_ref.right.clone();
            } else {
                // Find the inorder predecessor
                let mut predecessor = node_ref.left.as_ref().unwrap().clone();
                let mut predecessor_ref = predecessor.borrow();
                
                // Go to the rightmost node of the left subtree
                while predecessor_ref.right.is_some() && 
                      predecessor_ref.right.as_ref().unwrap() != &node {
                    let next = predecessor_ref.right.as_ref().unwrap().clone();
                    predecessor = next;
                    predecessor_ref = predecessor.borrow();
                }
                
                // If right of predecessor is None, make current as right child
                if predecessor_ref.right.is_none() {
                    predecessor_ref.right = Some(node.clone());
                    current = node_ref.left.clone();
                } else {
                    // If right is current, we've visited left subtree
                    // So we remove the link and visit current node
                    predecessor_ref.right = None;
                    result.push(node_ref.val);
                    current = node_ref.right.clone();
                }
            }
        }
        
        result
    }
    
    /// Morris traversal for preorder traversal
    pub fn preorder_traversal(root: Option<Rc<RefCell<TreeNode>>>) -> Vec<i32> {
        let mut result = Vec::new();
        let mut current = root;
        
        while let Some(node) = current {
            let node_ref = node.borrow();
            
            // If left is None, visit current node and go to right
            if node_ref.left.is_none() {
                result.push(node_ref.val);
                current = node_ref.right.clone();
            } else {
                // Find the inorder predecessor
                let mut predecessor = node_ref.left.as_ref().unwrap().clone();
                let mut predecessor_ref = predecessor.borrow();
                
                // Go to the rightmost node of the left subtree
                while predecessor_ref.right.is_some() && 
                      predecessor_ref.right.as_ref().unwrap() != &node {
                    let next = predecessor_ref.right.as_ref().unwrap().clone();
                    predecessor = next;
                    predecessor_ref = predecessor.borrow();
                }
                
                // If right of predecessor is None, make current as right child
                if predecessor_ref.right.is_none() {
                    result.push(node_ref.val); // Visit current node in preorder
                    predecessor_ref.right = Some(node.clone());
                    current = node_ref.left.clone();
                } else {
                    // If right is current, we've visited left subtree
                    // So we remove the link and go to right
                    predecessor_ref.right = None;
                    current = node_ref.right.clone();
                }
            }
        }
        
        result
    }
}

// Example usage
fn main() {
    // Create a sample binary tree:
    //       1
    //      / \
    //     2   3
    //    / \
    //   4   5
    
    let root = Rc::new(RefCell::new(TreeNode::new(1)));
    let left = Rc::new(RefCell::new(TreeNode::new(2)));
    let right = Rc::new(RefCell::new(TreeNode::new(3)));
    let left_left = Rc::new(RefCell::new(TreeNode::new(4)));
    let left_right = Rc::new(RefCell::new(TreeNode::new(5)));
    
    root.borrow_mut().left = Some(left.clone());
    root.borrow_mut().right = Some(right.clone());
    left.borrow_mut().left = Some(left_left.clone());
    left.borrow_mut().right = Some(left_right.clone());
    
    // Inorder traversal: 4 2 5 1 3
    let inorder_result = MorrisTraversal::inorder_traversal(Some(root.clone()));
    println!("Inorder traversal: {:?}", inorder_result);
    
    // Preorder traversal: 1 2 4 5 3
    let preorder_result = MorrisTraversal::preorder_traversal(Some(root.clone()));
    println!("Preorder traversal: {:?}", preorder_result);
}
```

## How Morris Traversal Works

The Morris traversal algorithm works by temporarily modifying the tree structure to create links back to previously visited nodes:

1. **For Inorder Traversal**:
   - If current node has no left child, visit it and move to right
   - If current node has left child, find the inorder predecessor (rightmost node in left subtree)
   - If predecessor's right is None, create a link back to current node and move to left
   - If predecessor's right points to current node, remove the link, visit current node, and move to right

2. **For Preorder Traversal**:
   - Similar logic but visits the current node before moving to the left subtree

## Key Features

- **Time Complexity**: O(n) where n is the number of nodes
- **Space Complexity**: O(1) - no extra space except for variables
- **No recursion or stack needed**
- **Modifies tree temporarily** during traversal (but restores it)

This algorithm is particularly useful when you want to traverse a binary tree with constant space complexity.

