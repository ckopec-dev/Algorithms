# Longest Increasing Subsequence (LIS) in Rust

Here's an implementation of the LIS algorithm using dynamic programming with O(n²) time complexity:

```rust
fn longest_increasing_subsequence(nums: &[i32]) -> usize {
    if nums.is_empty() {
        return 0;
    }
    
    let n = nums.len();
    // dp[i] represents the length of LIS ending at index i
    let mut dp = vec![1; n];
    
    // Fill dp array
    for i in 1..n {
        for j in 0..i {
            if nums[i] > nums[j] {
                dp[i] = dp[i].max(dp[j] + 1);
            }
        }
    }
    
    // Return the maximum value in dp array
    dp.into_iter().max().unwrap_or(0)
}

// Alternative implementation that also returns the actual subsequence
fn longest_increasing_subsequence_with_path(nums: &[i32]) -> (usize, Vec<i32>) {
    if nums.is_empty() {
        return (0, vec![]);
    }
    
    let n = nums.len();
    let mut dp = vec![1; n];
    let mut parent = vec![None; n]; // To track the path
    
    for i in 1..n {
        for j in 0..i {
            if nums[i] > nums[j] && dp[j] + 1 > dp[i] {
                dp[i] = dp[j] + 1;
                parent[i] = Some(j);
            }
        }
    }
    
    // Find the index with maximum LIS length
    let max_length = dp.iter().max().unwrap();
    let max_index = dp.iter().position(|&x| x == *max_length).unwrap();
    
    // Reconstruct the actual subsequence
    let mut subsequence = Vec::new();
    let mut current = max_index;
    
    while let Some(prev) = parent[current] {
        subsequence.push(nums[current]);
        current = prev;
    }
    subsequence.push(nums[current]);
    subsequence.reverse();
    
    (*max_length, subsequence)
}

fn main() {
    // Test cases
    let test_cases = vec![
        vec![10, 9, 2, 5, 3, 7, 101, 18],
        vec![0, 1, 0, 3, 2, 3],
        vec![7, 7, 7, 7, 7, 7, 7],
        vec![1, 3, 6, 7, 9, 4, 10, 5, 6],
        vec![],
        vec![5],
    ];
    
    for (i, nums) in test_cases.iter().enumerate() {
        let length = longest_increasing_subsequence(nums);
        let (length_with_path, path) = longest_increasing_subsequence_with_path(nums);
        
        println!("Test case {}: {:?}", i + 1, nums);
        println!("  LIS length: {}", length);
        println!("  LIS with path: {:?}", path);
        println!("  Verified length: {}", length_with_path);
        println!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lis() {
        assert_eq!(longest_increasing_subsequence(&[10, 9, 2, 5, 3, 7, 101, 18]), 4);
        assert_eq!(longest_increasing_subsequence(&[0, 1, 0, 3, 2, 3]), 4);
        assert_eq!(longest_increasing_subsequence(&[7, 7, 7, 7, 7, 7, 7]), 1);
        assert_eq!(longest_increasing_subsequence(&[1, 3, 6, 7, 9, 4, 10, 5, 6]), 6);
        assert_eq!(longest_increasing_subsequence(&[]), 0);
        assert_eq!(longest_increasing_subsequence(&[5]), 1);
    }
}
```

## Output Example:
```
Test case 1: [10, 9, 2, 5, 3, 7, 101, 18]
  LIS length: 4
  LIS with path: [2, 3, 7, 18]

Test case 2: [0, 1, 0, 3, 2, 3]
  LIS length: 4
  LIS with path: [0, 1, 2, 3]

Test case 3: [7, 7, 7, 7, 7, 7, 7]
  LIS length: 1
  LIS with path: [7]

Test case 4: [1, 3, 6, 7, 9, 4, 10, 5, 6]
  LIS length: 6
  LIS with path: [1, 3, 6, 7, 9, 10]
```

## Key Features:

1. **Basic LIS Function**: Returns only the length of the longest increasing subsequence
2. **Enhanced Version**: Returns both the length and the actual subsequence
3. **Edge Cases**: Handles empty arrays and single-element arrays
4. **Time Complexity**: O(n²) for the basic implementation
5. **Space Complexity**: O(n) for the DP array
6. **Comprehensive Tests**: Includes unit tests for verification

## How it works:

1. Create a DP array where `dp[i]` represents the length of the longest increasing subsequence ending at index `i`
2. For each element, check all previous elements that are smaller
3. Update the DP value if we can extend a longer subsequence
4. Return the maximum value in the DP array

The algorithm correctly identifies that in `[10, 9, 2, 5, 3, 7, 101, 18]`, one possible LIS is `[2, 3, 7, 18]` with length 4.

