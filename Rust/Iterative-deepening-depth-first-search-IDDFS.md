# Iterative Deepening Depth-First Search (IDDFS) in Rust

Here's a complete implementation of IDDFS algorithm in Rust:

```rust
use std::collections::HashSet;

#[derive(Debug, Clone)]
struct Node {
    value: i32,
    children: Vec<Node>,
}

impl Node {
    fn new(value: i32) -> Self {
        Node {
            value,
            children: Vec::new(),
        }
    }
    
    fn add_child(&mut self, child: Node) {
        self.children.push(child);
    }
}

fn dfs_with_limit(node: &Node, target: i32, depth_limit: usize, current_depth: usize) -> Option<Vec<i32>> {
    // If we've exceeded the depth limit, return None
    if current_depth > depth_limit {
        return None;
    }
    
    // If we found the target, return the path
    if node.value == target {
        return Some(vec![node.value]);
    }
    
    // Recursively search in children
    for child in &node.children {
        if let Some(mut path) = dfs_with_limit(child, target, depth_limit, current_depth + 1) {
            path.insert(0, node.value);
            return Some(path);
        }
    }
    
    None
}

fn iddfs(root: &Node, target: i32) -> Option<Vec<i32>> {
    let mut depth = 0;
    
    loop {
        // Try DFS with current depth limit
        if let Some(path) = dfs_with_limit(root, target, depth, 0) {
            return Some(path);
        }
        
        depth += 1;
        
        // Prevent infinite loop - set a reasonable maximum depth
        if depth > 1000 {
            break;
        }
    }
    
    None
}

fn main() {
    // Create a sample tree
    //       1
    //    /  |  \
    //   2   3   4
    //  /|   |   |\
    // 5 6  7   8 9
    
    let mut root = Node::new(1);
    let mut node2 = Node::new(2);
    let mut node3 = Node::new(3);
    let mut node4 = Node::new(4);
    
    node2.add_child(Node::new(5));
    node2.add_child(Node::new(6));
    node3.add_child(Node::new(7));
    node4.add_child(Node::new(8));
    node4.add_child(Node::new(9));
    
    root.add_child(node2);
    root.add_child(node3);
    root.add_child(node4);
    
    // Test IDDFS
    println!("Searching for value 7:");
    match iddfs(&root, 7) {
        Some(path) => println!("Found path: {:?}", path),
        None => println!("Value not found"),
    }
    
    println!("\nSearching for value 9:");
    match iddfs(&root, 9) {
        Some(path) => println!("Found path: {:?}", path),
        None => println!("Value not found"),
    }
    
    println!("\nSearching for value 10 (not in tree):");
    match iddfs(&root, 10) {
        Some(path) => println!("Found path: {:?}", path),
        None => println!("Value not found"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_iddfs_found() {
        let mut root = Node::new(1);
        let mut node2 = Node::new(2);
        let mut node3 = Node::new(3);
        
        node2.add_child(Node::new(5));
        node2.add_child(Node::new(6));
        node3.add_child(Node::new(7));
        
        root.add_child(node2);
        root.add_child(node3);
        
        let result = iddfs(&root, 7);
        assert!(result.is_some());
        assert_eq!(result.unwrap(), vec![1, 3, 7]);
    }
    
    #[test]
    fn test_iddfs_not_found() {
        let root = Node::new(1);
        let result = iddfs(&root, 2);
        assert!(result.is_none());
    }
}
```

## How IDDFS Works

1. **Iterative Process**: IDDFS starts with a depth limit of 0 and gradually increases it
2. **Depth-First Search**: At each depth limit, it performs a regular DFS
3. **Complete Search**: When a depth limit is reached, it increases the limit and tries again
4. **Optimal Solution**: Finds the shallowest path to the target

## Key Features

- **Memory Efficient**: Uses only O(d) space where d is the depth
- **Optimal**: Finds the shortest path (like BFS) but with less memory usage
- **Complete**: Will find a solution if one exists
- **Time Complexity**: O(b^d) where b is branching factor and d is depth

## Example Output

```
Searching for value 7:
Found path: [1, 3, 7]

Searching for value 9:
Found path: [1, 4, 9]

Searching for value 10 (not in tree):
Value not found
```

This implementation demonstrates how IDDFS combines the benefits of both DFS (low memory usage) and BFS (completeness and optimality) by systematically increasing depth limits.

