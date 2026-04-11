# Boyer-Moore Majority Vote Algorithm in Rust

```rust
fn boyer_moore_majority_vote(nums: &[i32]) -> Option<i32> {
    if nums.is_empty() {
        return None;
    }
    
    let mut candidate = nums[0];
    let mut count = 1;
    
    // Phase 1: Find candidate
    for &num in nums.iter().skip(1) {
        if num == candidate {
            count += 1;
        } else {
            count -= 1;
            if count == 0 {
                candidate = num;
                count = 1;
            }
        }
    }
    
    // Phase 2: Verify candidate is actually majority
    let mut candidate_count = 0;
    for &num in nums {
        if num == candidate {
            candidate_count += 1;
        }
    }
    
    if candidate_count > nums.len() / 2 {
        Some(candidate)
    } else {
        None
    }
}

fn main() {
    // Example 1: Array with majority element
    let nums1 = [3, 2, 3];
    match boyer_moore_majority_vote(&nums1) {
        Some(candidate) => println!("Majority element: {}", candidate),
        None => println!("No majority element found"),
    }
    
    // Example 2: Array with majority element
    let nums2 = [2, 2, 1, 1, 1, 2, 2];
    match boyer_moore_majority_vote(&nums2) {
        Some(candidate) => println!("Majority element: {}", candidate),
        None => println!("No majority element found"),
    }
    
    // Example 3: Array without majority element
    let nums3 = [1, 2, 3, 4, 5];
    match boyer_moore_majority_vote(&nums3) {
        Some(candidate) => println!("Majority element: {}", candidate),
        None => println!("No majority element found"),
    }
    
    // Example 4: Single element array
    let nums4 = [5];
    match boyer_moore_majority_vote(&nums4) {
        Some(candidate) => println!("Majority element: {}", candidate),
        None => println!("No majority element found"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_majority_vote() {
        assert_eq!(boyer_moore_majority_vote(&[3, 2, 3]), Some(3));
        assert_eq!(boyer_moore_majority_vote(&[2, 2, 1, 1, 1, 2, 2]), Some(2));
        assert_eq!(boyer_moore_majority_vote(&[1, 2, 3, 4, 5]), None);
        assert_eq!(boyer_moore_majority_vote(&[5]), Some(5));
        assert_eq!(boyer_moore_majority_vote(&[]), None);
    }
}
```

## How it works:

1. **Phase 1 - Candidate Selection**: 
   - Start with the first element as candidate
   - For each subsequent element:
     - If it matches the candidate, increment count
     - If it doesn't match, decrement count
     - If count reaches 0, replace candidate with current element

2. **Phase 2 - Verification**:
   - Count occurrences of the candidate
   - Return candidate only if it appears more than n/2 times

## Time and Space Complexity:
- **Time Complexity**: O(n) - two passes through the array
- **Space Complexity**: O(1) - only using constant extra space

## Output:
```
Majority element: 3
Majority element: 2
No majority element found
Majority element: 5
```

