# Patience Sorting Algorithm in Rust

Here's an implementation of the patience sorting algorithm in Rust:

```rust
fn patience_sort(arr: &mut Vec<i32>) {
    if arr.is_empty() {
        return;
    }
    
    let mut piles: Vec<Vec<i32>> = Vec::new();
    
    // Step 1: Create piles by placing each element on the leftmost pile
    // where the top element is greater than or equal to the current element
    for &element in arr {
        let mut placed = false;
        
        // Try to place element on existing piles
        for pile in &mut piles {
            if pile.last().unwrap() >= &element {
                pile.push(element);
                placed = true;
                break;
            }
        }
        
        // If element couldn't be placed on any existing pile, create a new pile
        if !placed {
            piles.push(vec![element]);
        }
    }
    
    // Step 2: Merge all piles back into the original array
    // This is done by repeatedly taking the smallest element from the top of all piles
    let mut result = Vec::new();
    
    // Create a min-heap to efficiently get the smallest element
    let mut heap = std::collections::BinaryHeap::new();
    
    // Initialize heap with first element of each pile (in reverse order for min-heap)
    for (i, pile) in piles.iter().enumerate() {
        if !pile.is_empty() {
            heap.push(std::cmp::Reverse((pile.last().unwrap(), i)));
        }
    }
    
    // Extract elements from heap and build result
    while !heap.is_empty() {
        let smallest = heap.pop().unwrap().0;
        let pile_index = smallest.1;
        let value = smallest.0;
        
        result.push(value);
        
        // Remove the element from its pile
        piles[pile_index].pop();
        
        // If pile is not empty, add its new top element to heap
        if !piles[pile_index].is_empty() {
            let new_top = piles[pile_index].last().unwrap();
            heap.push(std::cmp::Reverse((*new_top, pile_index)));
        }
    }
    
    // Copy result back to original array
    *arr = result;
}

// Alternative simpler implementation using merge approach
fn patience_sort_simple(arr: &mut Vec<i32>) {
    if arr.is_empty() {
        return;
    }
    
    let mut piles: Vec<Vec<i32>> = Vec::new();
    
    // Create piles
    for &element in arr {
        let mut placed = false;
        
        for pile in &mut piles {
            if pile.last().unwrap_or(&i32::MAX) >= &element {
                pile.push(element);
                placed = true;
                break;
            }
        }
        
        if !placed {
            piles.push(vec![element]);
        }
    }
    
    // Merge piles
    let mut result = Vec::new();
    let mut pile_indices = vec![0; piles.len()];
    
    while result.len() < arr.len() {
        let mut min_val = i32::MAX;
        let mut min_pile = 0;
        
        // Find minimum element among tops of all piles
        for (i, pile) in piles.iter().enumerate() {
            if pile_indices[i] < pile.len() {
                let current_val = pile[pile_indices[i]];
                if current_val < min_val {
                    min_val = current_val;
                    min_pile = i;
                }
            }
        }
        
        result.push(min_val);
        pile_indices[min_pile] += 1;
    }
    
    *arr = result;
}

fn main() {
    let mut arr1 = vec![3, 7, 2, 9, 1, 5, 8, 4, 6];
    println!("Original array: {:?}", arr1);
    
    patience_sort_simple(&mut arr1);
    println!("Sorted array:   {:?}", arr1);
    
    // Test with another array
    let mut arr2 = vec![5, 2, 8, 1, 9, 3];
    println!("\nOriginal array: {:?}", arr2);
    
    patience_sort_simple(&mut arr2);
    println!("Sorted array:   {:?}", arr2);
}
```

## How Patience Sorting Works:

1. **Create Piles**: Place each element on the leftmost pile where the top element is greater than or equal to the current element
2. **Merge Piles**: Repeatedly extract the smallest element from the top of all piles and build the sorted result

## Example Execution:

For array `[3, 7, 2, 9, 1, 5, 8, 4, 6]`:

**Step 1 - Creating Piles:**
```
Pile 1: [3, 2, 1]
Pile 2: [7, 5, 4]
Pile 3: [9, 8, 6]
```

**Step 2 - Merging:**
- Extract 1 (smallest)
- Extract 4 (next smallest)
- Extract 5 (next smallest)
- Continue until all elements are extracted

**Result:** `[1, 2, 3, 4, 5, 6, 7, 8, 9]`

## Time and Space Complexity:

- **Time Complexity**: O(n log n) where n is the number of elements
- **Space Complexity**: O(n) for storing the piles

The patience sorting algorithm is particularly useful because it's stable and has a time complexity that's optimal for comparison-based sorting algorithms.

