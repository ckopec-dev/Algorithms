# Levenshtein Distance in Rust

Here's a complete implementation of the Levenshtein distance algorithm in Rust:

```rust
fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let chars1: Vec<char> = s1.chars().collect();
    let chars2: Vec<char> = s2.chars().collect();
    
    let len1 = chars1.len();
    let len2 = chars2.len();
    
    // Create a 2D matrix to store distances
    let mut dp = vec![vec![0; len2 + 1]; len1 + 1];
    
    // Initialize base cases
    for i in 0..=len1 {
        dp[i][0] = i;
    }
    
    for j in 0..=len2 {
        dp[0][j] = j;
    }
    
    // Fill the matrix
    for (i, &char1) in chars1.iter().enumerate() {
        for (j, &char2) in chars2.iter().enumerate() {
            if char1 == char2 {
                dp[i + 1][j + 1] = dp[i][j];
            } else {
                dp[i + 1][j + 1] = dp[i][j].min(dp[i + 1][j].min(dp[i][j + 1])) + 1;
            }
        }
    }
    
    dp[len1][len2]
}

fn main() {
    // Test cases
    println!("Levenshtein Distance Examples:");
    println!("'kitten' vs 'sitting': {}", levenshtein_distance("kitten", "sitting"));
    println!("'saturday' vs 'sunday': {}", levenshtein_distance("saturday", "sunday"));
    println!("'hello' vs 'world': {}", levenshtein_distance("hello", "world"));
    println!("'rust' vs 'rust': {}", levenshtein_distance("rust", "rust"));
    println!("'abc' vs 'def': {}", levenshtein_distance("abc", "def"));
    println!("'' vs 'hello': {}", levenshtein_distance("", "hello"));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_levenshtein() {
        assert_eq!(levenshtein_distance("kitten", "sitting"), 3);
        assert_eq!(levenshtein_distance("saturday", "sunday"), 3);
        assert_eq!(levenshtein_distance("hello", "world"), 4);
        assert_eq!(levenshtein_distance("rust", "rust"), 0);
        assert_eq!(levenshtein_distance("abc", "def"), 3);
        assert_eq!(levenshtein_distance("", "hello"), 5);
        assert_eq!(levenshtein_distance("hello", ""), 5);
    }
}
```

## How it works:

1. **Dynamic Programming Approach**: Uses a 2D matrix where `dp[i][j]` represents the minimum edit distance between the first `i` characters of string 1 and the first `j` characters of string 2.

2. **Base Cases**: 
   - Empty string to any string: distance = length of the non-empty string
   - Empty string to empty string: distance = 0

3. **Recurrence Relation**:
   - If characters match: `dp[i][j] = dp[i-1][j-1]`
   - If characters don't match: `dp[i][j] = min(dp[i-1][j], dp[i][j-1], dp[i-1][j-1]) + 1`

## Example Output:
```
Levenshtein Distance Examples:
'kitten' vs 'sitting': 3
'saturday' vs 'sunday': 3
'hello' vs 'world': 4
'rust' vs 'rust': 0
'abc' vs 'def': 3
'' vs 'hello': 5
```

The algorithm has a time complexity of O(m×n) and space complexity of O(m×n), where m and n are the lengths of the input strings.

