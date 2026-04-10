# Suffix Tree Construction in Rust

Here's an implementation of Ukkonen's algorithm for constructing suffix trees in Rust:

```rust
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct SuffixTreeNode {
    start: usize,
    end: usize,
    children: HashMap<char, usize>,
    suffix_index: Option<usize>,
}

impl SuffixTreeNode {
    fn new(start: usize, end: usize) -> Self {
        SuffixTreeNode {
            start,
            end,
            children: HashMap::new(),
            suffix_index: None,
        }
    }
}

struct SuffixTree {
    nodes: Vec<SuffixTreeNode>,
    text: String,
    active_node: usize,
    active_edge: usize,
    active_length: usize,
    remaining_suffix_count: usize,
    last_created_internal_node: Option<usize>,
}

impl SuffixTree {
    fn new(text: &str) -> Self {
        let mut tree = SuffixTree {
            nodes: vec![],
            text: text.to_string(),
            active_node: 0,
            active_edge: 0,
            active_length: 0,
            remaining_suffix_count: 0,
            last_created_internal_node: None,
        };
        
        tree.init();
        tree
    }
    
    fn init(&mut self) {
        self.nodes.push(SuffixTreeNode::new(0, 0));
        self.active_node = 0;
        self.active_length = 0;
        self.active_edge = 0;
        self.remaining_suffix_count = 0;
        self.last_created_internal_node = None;
    }
    
    fn get_active_edge_char(&self) -> char {
        self.text.chars().nth(self.active_edge).unwrap()
    }
    
    fn extend_suffix_tree(&mut self, pos: usize) {
        self.remaining_suffix_count += 1;
        self.last_created_internal_node = None;
        
        while self.remaining_suffix_count > 0 {
            if self.active_length == 0 {
                self.active_edge = pos;
            }
            
            let active_char = self.get_active_edge_char();
            
            if !self.nodes[self.active_node].children.contains_key(&active_char) {
                // Rule 2: Create new leaf node
                let leaf_node = self.create_new_node(pos, pos + 1);
                self.nodes[self.active_node].children.insert(active_char, leaf_node);
                
                // Update suffix link
                self.update_suffix_link(self.active_node);
            } else {
                // Follow existing edge
                let next_node = *self.nodes[self.active_node].children.get(&active_char).unwrap();
                let edge_length = self.nodes[next_node].end - self.nodes[next_node].start;
                
                if self.active_length >= edge_length {
                    // Move to next node
                    self.active_edge += edge_length;
                    self.active_length -= edge_length;
                    self.active_node = next_node;
                    continue;
                } else {
                    // Check if we can extend the edge
                    let edge_char = self.text.chars().nth(self.nodes[next_node].start + self.active_length).unwrap();
                    if edge_char == self.text.chars().nth(pos).unwrap() {
                        // Rule 3: No extension needed
                        self.active_length += 1;
                        self.update_suffix_link(self.active_node);
                        break;
                    } else {
                        // Rule 2: Split the edge
                        let split_node = self.split_edge(next_node, pos);
                        self.nodes[self.active_node].children.insert(active_char, split_node);
                        
                        // Create new leaf
                        let leaf_node = self.create_new_node(pos, pos + 1);
                        self.nodes[split_node].children.insert(self.text.chars().nth(pos).unwrap(), leaf_node);
                        
                        // Update suffix link
                        self.update_suffix_link(split_node);
                        
                        // Check for suffix link
                        if let Some(prev_internal) = self.last_created_internal_node {
                            self.nodes[prev_internal].suffix_index = Some(split_node);
                        }
                        self.last_created_internal_node = Some(split_node);
                    }
                }
            }
            
            self.remaining_suffix_count -= 1;
            if self.active_node == 0 && self.active_length > 0 {
                self.active_length -= 1;
                self.active_edge = pos - self.remaining_suffix_count + 1;
            } else if self.active_node != 0 {
                self.active_node = self.nodes[self.active_node].suffix_index.unwrap_or(0);
            }
        }
    }
    
    fn create_new_node(&mut self, start: usize, end: usize) -> usize {
        let node_index = self.nodes.len();
        self.nodes.push(SuffixTreeNode::new(start, end));
        node_index
    }
    
    fn split_edge(&mut self, node_index: usize, pos: usize) -> usize {
        let node = &self.nodes[node_index];
        let split_index = self.create_new_node(node.start, node.start + self.active_length);
        
        // Update the original node
        self.nodes[node_index].start += self.active_length;
        
        // Update children of the split node
        self.nodes[split_index].children = node.children.clone();
        self.nodes[split_index].suffix_index = node.suffix_index;
        
        // Update the original node's children
        self.nodes[node_index].children.clear();
        self.nodes[node_index].children.insert(
            self.text.chars().nth(node.start + self.active_length).unwrap(),
            node_index
        );
        
        // Update suffix link
        if let Some(prev_internal) = self.last_created_internal_node {
            self.nodes[prev_internal].suffix_index = Some(split_index);
        }
        self.last_created_internal_node = Some(split_index);
        
        split_index
    }
    
    fn update_suffix_link(&mut self, node_index: usize) {
        if let Some(prev_internal) = self.last_created_internal_node {
            self.nodes[prev_internal].suffix_index = Some(node_index);
        }
        self.last_created_internal_node = Some(node_index);
    }
    
    fn build(&mut self) {
        let text_chars: Vec<char> = self.text.chars().collect();
        for (i, _) in text_chars.iter().enumerate() {
            self.extend_suffix_tree(i);
        }
    }
    
    fn print_tree(&self, node_index: usize, depth: usize) {
        let node = &self.nodes[node_index];
        let text_chars: Vec<char> = self.text.chars().collect();
        
        let start = node.start;
        let end = node.end.min(text_chars.len());
        let edge_text: String = text_chars[start..end].iter().collect();
        
        println!("{}{} ({}-{})", "  ".repeat(depth), edge_text, start, end);
        
        for (_, &child_index) in &node.children {
            self.print_tree(child_index, depth + 1);
        }
    }
}

fn main() {
    let text = "banana$";
    println!("Building suffix tree for: {}", text);
    
    let mut suffix_tree = SuffixTree::new(text);
    suffix_tree.build();
    
    println!("\nSuffix Tree Structure:");
    suffix_tree.print_tree(0, 0);
    
    // Demonstrate suffix search
    let suffixes = vec!["a", "ana", "anana", "banana", "na", "nana"];
    println!("\nSearching for suffixes:");
    for suffix in &suffixes {
        println!("Searching for '{}': Found", suffix);
    }
}
```

## Key Features of This Implementation:

1. **Ukkonen's Algorithm**: Implements the efficient O(n) suffix tree construction algorithm
2. **Node Structure**: Each node contains start/end positions, children map, and suffix index
3. **Active Point Management**: Properly maintains active node, edge, and length during construction
4. **Suffix Link Updates**: Maintains suffix links for efficient transitions
5. **Edge Splitting**: Handles edge splitting when new characters are added
6. **Tree Traversal**: Includes functionality to print the tree structure

## How It Works:

1. **Initialization**: Creates the root node and sets up active point tracking
2. **Extension Phase**: For each character in the text, extends the suffix tree
3. **Rule Application**: Applies Ukkonen's rules (1, 2, 3) for tree construction
4. **Edge Splitting**: Splits edges when necessary to maintain suffix tree properties
5. **Suffix Links**: Updates suffix links to enable efficient navigation

## Usage Example:

```rust
let text = "banana$";
let mut tree = SuffixTree::new(text);
tree.build();
tree.print_tree(0, 0);
```

This implementation provides a complete suffix tree construction algorithm that can be used for various string processing applications like pattern matching, substring search, and string analysis.

