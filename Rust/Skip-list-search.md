# Skip List Search Algorithm in Rust

Here's an implementation of a skip list search algorithm in Rust:

```rust
use std::cmp::Ordering;

#[derive(Debug, Clone)]
struct SkipListNode<T> {
    value: T,
    forward: Vec<*mut SkipListNode<T>>,
}

impl<T> SkipListNode<T> {
    fn new(value: T, level: usize) -> Self {
        SkipListNode {
            value,
            forward: vec![std::ptr::null_mut(); level + 1],
        }
    }
}

struct SkipList<T> {
    head: *mut SkipListNode<T>,
    max_level: usize,
    current_level: usize,
}

impl<T: Ord> SkipList<T> {
    fn new(max_level: usize) -> Self {
        let head = Box::into_raw(Box::new(SkipListNode::new(T::default(), max_level)));
        SkipList {
            head,
            max_level,
            current_level: 0,
        }
    }

    fn search(&self, target: &T) -> Option<&T> {
        unsafe {
            let mut current = self.head;
            
            // Start from the highest level and move down
            for level in (0..=self.current_level).rev() {
                // Move forward while the next node's value is less than target
                while !(*current).forward[level].is_null() {
                    let next = &*(*current).forward[level];
                    match next.value.cmp(target) {
                        Ordering::Less => current = (*current).forward[level],
                        Ordering::Equal => return Some(&next.value),
                        Ordering::Greater => break,
                    }
                }
            }
            
            // Move to the next node at level 0
            current = (*current).forward[0];
            
            // Check if we found the target
            if !current.is_null() && (*current).value == *target {
                Some(&(*current).value)
            } else {
                None
            }
        }
    }

    fn insert(&mut self, value: T) {
        unsafe {
            let mut update = vec![std::ptr::null_mut(); self.max_level + 1];
            let mut current = self.head;
            
            // Find the position to insert
            for level in (0..=self.current_level).rev() {
                while !(*current).forward[level].is_null() {
                    let next = &*(*current).forward[level];
                    if next.value < value {
                        current = (*current).forward[level];
                    } else {
                        break;
                    }
                }
                update[level] = current;
            }
            
            // Generate random level for the new node
            let new_level = self.random_level();
            
            if new_level > self.current_level {
                self.current_level = new_level;
            }
            
            // Create new node
            let new_node = Box::into_raw(Box::new(SkipListNode::new(value, new_level)));
            
            // Insert the new node
            for level in 0..=new_level {
                (*new_node).forward[level] = (*update[level]).forward[level];
                (*update[level]).forward[level] = new_node;
            }
        }
    }

    fn random_level(&self) -> usize {
        // Simple random level generation (in practice, use a better RNG)
        let mut level = 0;
        while level < self.max_level && rand::random::<bool>() {
            level += 1;
        }
        level
    }
}

impl<T> Drop for SkipList<T> {
    fn drop(&mut self) {
        unsafe {
            let mut current = self.head;
            while !current.is_null() {
                let next = (*current).forward[0];
                drop(Box::from_raw(current));
                current = next;
            }
        }
    }
}

// Example usage
fn main() {
    let mut skip_list = SkipList::<i32>::new(4);
    
    // Insert some values
    skip_list.insert(3);
    skip_list.insert(6);
    skip_list.insert(7);
    skip_list.insert(9);
    skip_list.insert(12);
    skip_list.insert(19);
    skip_list.insert(17);
    skip_list.insert(26);
    
    // Search for values
    println!("Searching for 19: {:?}", skip_list.search(&19)); // Some(19)
    println!("Searching for 15: {:?}", skip_list.search(&15)); // None
    println!("Searching for 3: {:?}", skip_list.search(&3));   // Some(3)
    println!("Searching for 26: {:?}", skip_list.search(&26)); // Some(26)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_skip_list_search() {
        let mut skip_list = SkipList::<i32>::new(4);
        
        // Insert values
        skip_list.insert(1);
        skip_list.insert(3);
        skip_list.insert(5);
        skip_list.insert(7);
        skip_list.insert(9);
        
        // Test existing values
        assert_eq!(skip_list.search(&1), Some(&1));
        assert_eq!(skip_list.search(&5), Some(&5));
        assert_eq!(skip_list.search(&9), Some(&9));
        
        // Test non-existing values
        assert_eq!(skip_list.search(&2), None);
        assert_eq!(skip_list.search(&4), None);
        assert_eq!(skip_list.search(&10), None);
    }
}
```

## Key Features of this Implementation:

1. **Skip List Structure**: Uses multiple levels of linked lists for efficient searching
2. **Search Algorithm**: 
   - Starts from the highest level
   - Moves forward while values are less than target
   - Drops down to lower levels when necessary
   - Final check at level 0 for exact match

3. **Time Complexity**: O(log n) average case for search operations
4. **Space Complexity**: O(n) for storing nodes

## How Search Works:

1. Begin at the highest level of the skip list
2. Move forward as long as the next node's value is less than the target
3. When the next node's value is greater than or equal to target, drop down to the next lower level
4. Repeat until reaching level 0
5. Check if the current node matches the target value

The algorithm efficiently reduces the search space by using the "skip" links at higher levels, making it much faster than linear search in sorted arrays or linked lists.

