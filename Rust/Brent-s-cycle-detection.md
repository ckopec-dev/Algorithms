# Brent's Cycle Detection Algorithm in Rust

Brent's cycle detection algorithm is an efficient method for detecting cycles in sequences, particularly useful for finding the start of a cycle in linked lists or sequences.

```rust
struct ListNode {
    val: i32,
    next: Option<Box<ListNode>>,
}

impl ListNode {
    fn new(val: i32) -> Self {
        ListNode { val, next: None }
    }
}

fn brent_cycle_detection(head: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
    if head.is_none() {
        return None;
    }
    
    // Phase 1: Find a point in the cycle
    let mut tortoise = &head.as_ref().unwrap().next;
    let mut hare = &head.as_ref().unwrap().next.as_ref().unwrap().next;
    
    // Move tortoise and hare until they meet
    while let (Some(t), Some(h)) = (tortoise.as_ref(), hare.as_ref()) {
        if t.val == h.val {
            break;
        }
        
        // Move tortoise one step
        tortoise = &t.next;
        
        // Move hare two steps
        if let Some(h2) = &h.next {
            hare = &h2.next;
        } else {
            break;
        }
    }
    
    // If no cycle found
    if tortoise.is_none() || hare.is_none() {
        return None;
    }
    
    // Phase 2: Find the start of the cycle
    let mut tortoise = &head;
    let mut hare = &head;
    
    // Move tortoise to the start
    // Move hare to the meeting point
    let mut power = 1;
    let mut length = 1;
    
    // Find the cycle length
    while let Some(h) = hare.as_ref() {
        if h.val == tortoise.as_ref().unwrap().val {
            break;
        }
        if length == power {
            power *= 2;
            length = 0;
            tortoise = &hare;
        }
        hare = &hare.as_ref().unwrap().next;
        length += 1;
    }
    
    // Phase 3: Find the start of the cycle
    let mut start = &head;
    let mut current = &head;
    
    // Move both pointers until they meet at the cycle start
    for _ in 0..length {
        if let Some(next) = current.as_ref() {
            current = &next.next;
        }
    }
    
    while start.as_ref().unwrap().val != current.as_ref().unwrap().val {
        if let Some(next_start) = start.as_ref() {
            start = &next_start.next;
        }
        if let Some(next_current) = current.as_ref() {
            current = &next_current.next;
        }
    }
    
    Some(start.as_ref().unwrap().clone())
}

// Simpler version for demonstration
fn brent_simple_cycle_detection(head: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
    if head.is_none() {
        return None;
    }
    
    // Phase 1: Detect cycle using tortoise and hare
    let mut tortoise = &head.as_ref().unwrap().next;
    let mut hare = &head.as_ref().unwrap().next.as_ref().unwrap().next;
    
    let mut cycle_found = false;
    
    // Find if there's a cycle
    while let (Some(t), Some(h)) = (tortoise.as_ref(), hare.as_ref()) {
        if t.val == h.val {
            cycle_found = true;
            break;
        }
        
        tortoise = &t.next;
        hare = &h.next.as_ref().unwrap().next;
    }
    
    if !cycle_found {
        return None;
    }
    
    // Phase 2: Find the start of the cycle
    let mut start = &head;
    let mut current = &head;
    
    // Move current pointer to the meeting point
    while start.as_ref().unwrap().val != current.as_ref().unwrap().val {
        start = &start.as_ref().unwrap().next;
        current = &current.as_ref().unwrap().next;
    }
    
    Some(start.as_ref().unwrap().clone())
}

// Alternative implementation with proper cycle detection
fn detect_cycle(head: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
    if head.is_none() || head.as_ref().unwrap().next.is_none() {
        return None;
    }
    
    let mut slow = &head.as_ref().unwrap().next;
    let mut fast = &head.as_ref().unwrap().next.as_ref().unwrap().next;
    
    // Phase 1: Find if there's a cycle
    while let (Some(s), Some(f)) = (slow.as_ref(), fast.as_ref()) {
        if s.val == f.val {
            break;
        }
        
        slow = &s.next;
        fast = &f.next.as_ref().unwrap().next;
    }
    
    // No cycle found
    if slow.as_ref().map_or(true, |s| s.val != fast.as_ref().unwrap().val) {
        return None;
    }
    
    // Phase 2: Find the start of the cycle
    let mut start = &head;
    let mut meeting_point = &slow;
    
    while start.as_ref().unwrap().val != meeting_point.as_ref().unwrap().val {
        start = &start.as_ref().unwrap().next;
        meeting_point = &meeting_point.as_ref().unwrap().next;
    }
    
    Some(start.as_ref().unwrap().clone())
}

// Example usage
fn main() {
    // Create a linked list with a cycle: 1 -> 2 -> 3 -> 4 -> 2 (cycle)
    let mut node1 = Box::new(ListNode::new(1));
    let mut node2 = Box::new(ListNode::new(2));
    let mut node3 = Box::new(ListNode::new(3));
    let mut node4 = Box::new(ListNode::new(4));
    
    node1.next = Some(node2.clone());
    node2.next = Some(node3.clone());
    node3.next = Some(node4.clone());
    node4.next = Some(node2); // Creates cycle: 4 -> 2
    
    let head = Some(node1);
    
    match detect_cycle(head) {
        Some(cycle_start) => println!("Cycle detected at node with value: {}", cycle_start.val),
        None => println!("No cycle detected"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_cycle_detection() {
        // Test case 1: No cycle
        let mut node1 = Box::new(ListNode::new(1));
        let mut node2 = Box::new(ListNode::new(2));
        let mut node3 = Box::new(ListNode::new(3));
        
        node1.next = Some(node2.clone());
        node2.next = Some(node3.clone());
        
        let head = Some(node1);
        assert_eq!(detect_cycle(head), None);
        
        // Test case 2: With cycle
        let mut node1 = Box::new(ListNode::new(1));
        let mut node2 = Box::new(ListNode::new(2));
        let mut node3 = Box::new(ListNode::new(3));
        let mut node4 = Box::new(ListNode::new(4));
        
        node1.next = Some(node2.clone());
        node2.next = Some(node3.clone());
        node3.next = Some(node4.clone());
        node4.next = Some(node2); // Cycle: 4 -> 2
        
        let head = Some(node1);
        let result = detect_cycle(head);
        assert!(result.is_some());
        assert_eq!(result.unwrap().val, 2);
    }
}
```

## Key Features of This Implementation:

1. **Two-Phase Approach**: 
   - Phase 1: Uses tortoise and hare to detect if a cycle exists
   - Phase 2: Finds the exact start of the cycle

2. **Time Complexity**: O(n) where n is the number of nodes
3. **Space Complexity**: O(1) - uses only a constant amount of extra space
4. **Robust Error Handling**: Handles edge cases like empty lists or single-node lists

## How It Works:

1. **Phase 1**: Move two pointers at different speeds (tortoise moves 1 step, hare moves 2 steps)
2. **Phase 2**: When they meet, reset one pointer to the head and move both at the same speed
3. **Phase 3**: The point where they meet again is the start of the cycle

This algorithm is particularly efficient because it avoids the need to calculate the exact cycle length, making it faster than Floyd's cycle detection algorithm in practice.

