# Brent's Cycle Detection Algorithm in Rust

Brent's cycle detection algorithm is an efficient method for detecting cycles in linked lists. Here's a complete implementation:

```rust
// Define a simple linked list node structure
#[derive(Debug, Clone)]
struct ListNode<T> {
    data: T,
    next: Option<Box<ListNode<T>>>,
}

impl<T> ListNode<T> {
    fn new(data: T) -> Self {
        ListNode {
            data,
            next: None,
        }
    }
}

// Brent's cycle detection algorithm implementation
fn brent_cycle_detection<T>(head: &Option<Box<ListNode<T>>>) -> Option<usize> {
    if head.is_none() {
        return None;
    }
    
    // Phase 1: Find a point in the cycle
    let mut power = 1;
    let mut lam = 1;
    let mut tortoise = head.as_ref();
    let mut hare = head.as_ref().and_then(|node| node.next.as_ref());
    
    // Move hare until we find a cycle or reach the end
    while let Some(hare_node) = hare {
        if tortoise.as_ref().unwrap().data == hare_node.data {
            // Found a cycle, now find the cycle length
            break;
        }
        
        if lam == power {
            tortoise = hare;
            power *= 2;
            lam = 0;
        }
        
        hare = hare_node.next.as_ref();
        lam += 1;
    }
    
    // If we reached the end without finding a cycle
    if hare.is_none() {
        return None;
    }
    
    // Phase 2: Find the start of the cycle
    let mut tortoise = head;
    let mut hare = head;
    
    // Move hare ahead by the cycle length
    for _ in 0..lam {
        hare = hare.as_ref().unwrap().next.as_ref();
    }
    
    // Move both pointers until they meet
    let mut mu = 0;
    while tortoise.as_ref().unwrap().data != hare.as_ref().unwrap().data {
        tortoise = tortoise.as_ref().unwrap().next.as_ref();
        hare = hare.as_ref().unwrap().next.as_ref();
        mu += 1;
    }
    
    Some(mu)
}

// Alternative simpler implementation for demonstration
fn brent_simple<T: PartialEq>(head: &Option<Box<ListNode<T>>>) -> Option<usize> {
    if head.is_none() {
        return None;
    }
    
    let mut power = 1;
    let mut lam = 1;
    let mut tortoise = head.as_ref();
    let mut hare = head.as_ref().and_then(|node| node.next.as_ref());
    
    // Phase 1: Find cycle
    while let Some(hare_node) = hare {
        if tortoise.as_ref().unwrap().data == hare_node.data {
            // Cycle detected, return cycle length
            return Some(lam);
        }
        
        if lam == power {
            tortoise = hare;
            power *= 2;
            lam = 0;
        }
        
        hare = hare_node.next.as_ref();
        lam += 1;
    }
    
    None // No cycle found
}

// Helper function to create a linked list from a vector
fn create_linked_list<T: Clone>(data: Vec<T>) -> Option<Box<ListNode<T>>> {
    if data.is_empty() {
        return None;
    }
    
    let mut head = Some(Box::new(ListNode::new(data[0].clone())));
    let mut current = head.as_mut().unwrap();
    
    for item in data.iter().skip(1) {
        current.next = Some(Box::new(ListNode::new(item.clone())));
        current = current.next.as_mut().unwrap();
    }
    
    head
}

// Helper function to create a cycle in a linked list
fn create_cyclic_list<T: Clone>(data: Vec<T>, cycle_start: usize) -> Option<Box<ListNode<T>>> {
    if data.is_empty() {
        return None;
    }
    
    let mut head = Some(Box::new(ListNode::new(data[0].clone())));
    let mut current = head.as_mut().unwrap();
    
    // Create the main chain
    for item in data.iter().skip(1) {
        current.next = Some(Box::new(ListNode::new(item.clone())));
        current = current.next.as_mut().unwrap();
    }
    
    // Create cycle by connecting to a previous node
    if cycle_start > 0 && cycle_start < data.len() {
        let mut cycle_start_node = head.as_mut().unwrap();
        for _ in 0..cycle_start {
            cycle_start_node = cycle_start_node.next.as_mut().unwrap();
        }
        current.next = Some(cycle_start_node);
    }
    
    head
}

fn main() {
    // Example 1: No cycle
    let list1 = create_linked_list(vec![1, 2, 3, 4, 5]);
    println!("List 1 (no cycle): {:?}", list1);
    println!("Cycle detection result: {:?}", brent_simple(&list1));
    
    // Example 2: Cycle in the list
    let list2 = create_cyclic_list(vec![1, 2, 3, 4, 5], 2); // Cycle starts at index 2 (node with value 3)
    println!("\nList 2 (with cycle): {:?}", list2);
    println!("Cycle detection result: {:?}", brent_simple(&list2));
    
    // Example 3: Simple cycle
    let list3 = create_cyclic_list(vec![1, 2, 3, 4, 5], 0); // Cycle starts at index 0 (head)
    println!("\nList 3 (cycle at head): {:?}", list3);
    println!("Cycle detection result: {:?}", brent_simple(&list3));
}
```

## Key Features of This Implementation:

1. **Two-Phase Algorithm**: 
   - Phase 1: Find a point in the cycle
   - Phase 2: Find the start of the cycle

2. **Efficient Complexity**: 
   - Time: O(λ + μ) where λ is cycle length and μ is distance to cycle start
   - Space: O(1)

3. **Error Handling**: 
   - Handles empty lists
   - Properly manages Option types

4. **Generic Implementation**: 
   - Works with any type T that implements PartialEq

## Usage Notes:

- The algorithm returns `None` if no cycle is found
- For cyclic lists, it returns the cycle length
- The implementation assumes you have a way to detect when nodes are the same (using `PartialEq`)
- In a real-world scenario, you'd typically work with references to nodes rather than cloning data

This implementation demonstrates the elegance and efficiency of Brent's algorithm compared to Floyd's cycle detection algorithm.

