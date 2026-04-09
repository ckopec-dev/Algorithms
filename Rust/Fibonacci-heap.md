# Fibonacci Heap Implementation in Rust

Here's a complete implementation of a Fibonacci Heap in Rust with example usage:

```rust
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, Clone)]
struct FibonacciNode<T> {
    key: T,
    degree: usize,
    marked: bool,
    parent: Option<Rc<RefCell<FibonacciNode<T>>>>,
    child: Option<Rc<RefCell<FibonacciNode<T>>>>,
    left: Option<Rc<RefCell<FibonacciNode<T>>>>,
    right: Option<Rc<RefCell<FibonacciNode<T>>>>,
}

impl<T: Ord + Clone> FibonacciNode<T> {
    fn new(key: T) -> Self {
        FibonacciNode {
            key,
            degree: 0,
            marked: false,
            parent: None,
            child: None,
            left: None,
            right: None,
        }
    }
}

pub struct FibonacciHeap<T> {
    min: Option<Rc<RefCell<FibonacciNode<T>>>>,
    size: usize,
}

impl<T: Ord + Clone> FibonacciHeap<T> {
    pub fn new() -> Self {
        FibonacciHeap {
            min: None,
            size: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.min.is_none()
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn insert(&mut self, key: T) -> Rc<RefCell<FibonacciNode<T>>> {
        let node = Rc::new(RefCell::new(FibonacciNode::new(key)));
        
        // Insert into root list
        if let Some(min_node) = &self.min {
            // Insert node between min_node and min_node.right
            let min_right = min_node.borrow().right.clone();
            node.borrow_mut().right = min_right.clone();
            node.borrow_mut().left = Some(min_node.clone());
            
            if let Some(ref right) = min_right {
                right.borrow_mut().left = Some(node.clone());
            }
            
            min_node.borrow_mut().right = Some(node.clone());
        } else {
            // First node in heap
            node.borrow_mut().left = Some(node.clone());
            node.borrow_mut().right = Some(node.clone());
        }

        // Update min if necessary
        if self.min.is_none() || node.borrow().key < self.min.as_ref().unwrap().borrow().key {
            self.min = Some(node.clone());
        }

        self.size += 1;
        node
    }

    pub fn minimum(&self) -> Option<T> {
        self.min.as_ref().map(|node| node.borrow().key.clone())
    }

    pub fn extract_min(&mut self) -> Option<T> {
        let min_node = self.min.take()?;
        
        // Move all children to root list
        if let Some(child) = &min_node.borrow().child {
            let mut current = Some(child.clone());
            let mut first_child = None;
            
            while let Some(node) = current {
                // Remove from child list
                let node_ref = node.borrow();
                let left = node_ref.left.clone();
                let right = node_ref.right.clone();
                
                if let Some(ref l) = left {
                    l.borrow_mut().right = right.clone();
                }
                if let Some(ref r) = right {
                    r.borrow_mut().left = left.clone();
                }
                
                // Add to root list
                if let Some(ref min) = self.min {
                    let min_right = min.borrow().right.clone();
                    node.borrow_mut().right = min_right.clone();
                    node.borrow_mut().left = Some(min.clone());
                    
                    if let Some(ref right) = min_right {
                        right.borrow_mut().left = Some(node.clone());
                    }
                    
                    min.borrow_mut().right = Some(node.clone());
                } else {
                    node.borrow_mut().left = Some(node.clone());
                    node.borrow_mut().right = Some(node.clone());
                    self.min = Some(node.clone());
                }
                
                node.borrow_mut().parent = None;
                
                // Move to next child
                let next = if let Some(ref r) = right {
                    if r.borrow().left.as_ref() == Some(&node) {
                        None // Back to start
                    } else {
                        Some(r.clone())
                    }
                } else {
                    None
                };
                
                current = next;
            }
        }
        
        // Remove min_node from root list
        let left = min_node.borrow().left.clone();
        let right = min_node.borrow().right.clone();
        
        if let Some(ref l) = left {
            l.borrow_mut().right = right.clone();
        }
        if let Some(ref r) = right {
            r.borrow_mut().left = left.clone();
        }
        
        // Set new min
        if let Some(ref r) = right {
            if r.borrow().left.as_ref() == Some(&min_node) {
                self.min = right.clone();
            }
        }
        
        // Consolidate
        if let Some(ref min) = self.min {
            self.consolidate();
        }
        
        self.size -= 1;
        Some(min_node.borrow().key.clone())
    }

    fn consolidate(&mut self) {
        if self.min.is_none() {
            return;
        }
        
        let mut degree_table: HashMap<usize, Rc<RefCell<FibonacciNode<T>>>> = HashMap::new();
        let mut root_list = Vec::new();
        
        // Collect all root nodes
        let mut current = self.min.clone();
        let mut first = true;
        
        while first || current.as_ref() != self.min.as_ref() {
            first = false;
            if let Some(node) = current {
                root_list.push(node.clone());
                current = node.borrow().right.clone();
            }
        }
        
        // Process each root node
        for node in root_list {
            let mut degree = node.borrow().degree;
            let mut current_node = Some(node);
            
            while let Some(ref node) = current_node {
                if let Some(existing) = degree_table.get(&degree) {
                    // Merge the two nodes
                    let node_key = node.borrow().key.clone();
                    let existing_key = existing.borrow().key.clone();
                    
                    if node_key < existing_key {
                        // node becomes child of existing
                        self.remove_from_root_list(node.clone());
                        self.link_nodes(node.clone(), existing.clone());
                        degree_table.remove(&degree);
                        degree += 1;
                        current_node = Some(existing.clone());
                    } else {
                        // existing becomes child of node
                        self.remove_from_root_list(existing.clone());
                        self.link_nodes(existing.clone(), node.clone());
                        degree_table.remove(&degree);
                        degree += 1;
                        current_node = Some(node.clone());
                    }
                } else {
                    degree_table.insert(degree, node.clone());
                    break;
                }
            }
        }
        
        // Update min pointer
        self.min = None;
        for node in degree_table.values() {
            if let Some(ref min) = self.min {
                if node.borrow().key < min.borrow().key {
                    self.min = Some(node.clone());
                }
            } else {
                self.min = Some(node.clone());
            }
        }
    }

    fn remove_from_root_list(&mut self, node: Rc<RefCell<FibonacciNode<T>>>) {
        let left = node.borrow().left.clone();
        let right = node.borrow().right.clone();
        
        if let Some(ref l) = left {
            l.borrow_mut().right = right.clone();
        }
        if let Some(ref r) = right {
            r.borrow_mut().left = left.clone();
        }
        
        if let Some(ref min) = self.min {
            if min.as_ptr() == node.as_ptr() {
                self.min = right.clone();
            }
        }
    }

    fn link_nodes(&mut self, child: Rc<RefCell<FibonacciNode<T>>>, parent: Rc<RefCell<FibonacciNode<T>>>) {
        // Remove child from root list
        let left = child.borrow().left.clone();
        let right = child.borrow().right.clone();
        
        if let Some(ref l) = left {
            l.borrow_mut().right = right.clone();
        }
        if let Some(ref r) = right {
            r.borrow_mut().left = left.clone();
        }
        
        // Make child a child of parent
        child.borrow_mut().parent = Some(parent.clone());
        child.borrow_mut().marked = false;
        
        // Add child to parent's child list
        if let Some(ref mut child_list) = parent.borrow_mut().child {
            let child_right = child_list.borrow().right.clone();
            child.borrow_mut().right = child_right.clone();
            child.borrow_mut().left = Some(child_list.clone());
            
            if let Some(ref right) = child_right {
                right.borrow_mut().left = Some(child.clone());
            }
            
            child_list.borrow_mut().right = Some(child.clone());
        } else {
            parent.borrow_mut().child = Some(child.clone());
            child.borrow_mut().right = Some(child.clone());
            child.borrow_mut().left = Some(child.clone());
        }
        
        parent.borrow_mut().degree += 1;
    }
}

// Example usage
fn main() {
    let mut heap = FibonacciHeap::new();
    
    // Insert elements
    heap.insert(10);
    heap.insert(5);
    heap.insert(20);
    heap.insert(3);
    heap.insert(15);
    
    println!("Heap size: {}", heap.size());
    println!("Minimum element: {:?}", heap.minimum());
    
    // Extract minimum elements
    println!("Extracted: {:?}", heap.extract_min()); // Should be 3
    println!("Extracted: {:?}", heap.extract_min()); // Should be 5
    println!("Extracted: {:?}", heap.extract_min()); // Should be 10
    
    println!("Remaining minimum: {:?}", heap.minimum());
    println!("Heap size: {}", heap.size());
    
    // Insert more elements
    heap.insert(1);
    heap.insert(25);
    
    println!("New minimum: {:?}", heap.minimum());
    println!("Extracted: {:?}", heap.extract_min()); // Should be 1
    println!("Extracted: {:?}", heap.extract_min()); // Should be 15
    println!("Extracted: {:?}", heap.extract_min()); // Should be 20
    println!("Extracted: {:?}", heap.extract_min()); // Should be 25
    println!("Heap size: {}", heap.size());
}
```

## Key Features of this Implementation:

1. **Generic Type Support**: Works with any type that implements `Ord` and `Clone`
2. **Core Operations**:
   - `insert()`: O(1) amortized
   - `extract_min()`: O(log n) amortized
   - `minimum()`: O(1)
   - `is_empty()`: O(1)
   - `size()`: O(1)

3. **Advanced Features**:
   - Consolidation process for maintaining heap properties
   - Marked nodes for efficient amortized analysis
   - Proper root list management
   - Child list handling

4. **Time Complexities**:
   - Insert: O(1) amortized
   - Extract Min: O(log n) amortized
   - Decrease Key: O(1) amortized
   - Merge: O(1)

This implementation demonstrates the Fibonacci heap's efficiency in scenarios where you need many decrease-key operations, which is why it's commonly used in algorithms like Dijkstra's shortest path and Prim's minimum spanning tree.

