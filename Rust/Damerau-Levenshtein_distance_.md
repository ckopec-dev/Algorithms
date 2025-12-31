# Damerau-Levenshtein Distance in Rust

Here's a complete implementation of the Damerau-Levenshtein distance algorithm in Rust:

```rust
fn damerau_levenshtein_distance(s1: &str, s2: &str) -> usize {
    let chars1: Vec<char> = s1.chars().collect();
    let chars2: Vec<char> = s2.chars().collect();
    
    let len1 = chars1.len();
    let len2 = chars2.len();
    
    // Create a matrix to store the distances
    let mut dp = vec![vec![0; len2 + 1]; len1 + 1];
    
    // Initialize the first row and column
    for i in 0..=len1 {
        dp[i][0] = i;
    }
    
    for j in 0..=len2 {
        dp[0][j] = j;
    }
    
    // Fill the matrix
    for i in 1..=len1 {
        for j in 1..=len2 {
            let cost = if chars1[i - 1] == chars2[j - 1] { 0 } else { 1 };
            
            dp[i][j] = dp[i - 1][j].min(dp[i][j - 1]).min(dp[i - 1][j - 1]) + cost;
            
            // Check for transposition (Damerau-Levenshtein specific)
            if i > 1 && j > 1 && chars1[i - 1] == chars2[j - 2] && chars1[i - 2] == chars2[j - 1] {
                dp[i][j] = dp[i][j].min(dp[i - 2][j - 2] + 1);
            }
        }
    }
    
    dp[len1][len2]
}

fn main() {
    // Test cases
    let test_cases = vec![
        ("kitten", "sitting"),
        ("saturday", "sunday"),
        ("hello", "world"),
        ("rust", "rust"),
        ("", "abc"),
        ("abc", ""),
        ("abc", "abc"),
        ("abc", "acb"),
        ("abc", "ab"),
        ("abc", "abcd"),
    ];
    
    println!("Damerau-Levenshtein Distance Examples:");
    println!("=====================================");
    
    for (s1, s2) in test_cases {
        let distance = damerau_levenshtein_distance(s1, s2);
        println!("'{}' -> '{}' = {}", s1, s2, distance);
    }
    
    // Example with explanation
    println!("\nDetailed Example:");
    println!("'kitten' -> 'sitting':");
    println!("- kitten (6 chars)");
    println!("- sitting (6 chars)");
    println!("- Operations needed: 1 substitution (k->s), 1 substitution (e->i), 1 insertion (g)");
    println!("- Distance: 3");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_cases() {
        assert_eq!(damerau_levenshtein_distance("kitten", "sitting"), 3);
        assert_eq!(damerau_levenshtein_distance("saturday", "sunday"), 3);
        assert_eq!(damerau_levenshtein_distance("hello", "world"), 4);
        assert_eq!(damerau_levenshtein_distance("rust", "rust"), 0);
        assert_eq!(damerau_levenshtein_distance("", ""), 0);
        assert_eq!(damerau_levenshtein_distance("abc", "acb"), 1);
    }

    #[test]
    fn test_edge_cases() {
        assert_eq!(damerau_levenshtein_distance("", "abc"), 3);
        assert_eq!(damerau_levenshtein_distance("abc", ""), 3);
        assert_eq!(damerau_levenshtein_distance("a", "b"), 1);
    }
}
```

## How it works:

1. **Matrix Initialization**: Creates a 2D matrix where `dp[i][j]` represents the distance between the first `i` characters of string 1 and the first `j` characters of string 2.

2. **Base Cases**: 
   - First row: distance from empty string to prefix of second string
   - First column: distance from prefix of first string to empty string

3. **Core Logic**: 
   - For each cell, calculate the minimum of:
     - Deletion (from above)
     - Insertion (from left)
     - Substitution (from diagonal)
   - Add transposition check (Damerau-Levenshtein specific)

4. **Transposition Detection**: If characters at positions `(i-1, j-2)` and `(i-2, j-1)` match, it's a transposition operation.

## Sample Output:
```
Damerau-Levenshtein Distance Examples:
=====================================
'kitten' -> 'sitting' = 3
'saturday' -> 'sunday' = 3
'hello' -> 'world' = 4
'rust' -> 'rust' = 0
'' -> 'abc' = 3
'abc' -> '' = 3
'abc' -> 'abc' = 0
'abc' -> 'acb' = 1
'abc' -> 'ab' = 1
'abc' -> 'abcd' = 1

Detailed Example:
'kitten' -> 'sitting':
- kitten (6 chars)
- sitting (6 chars)
- Operations needed: 1 substitution (k->s), 1 substitution (e->i), 1 insertion (g)
- Distance: 3
```

This implementation handles Unicode characters correctly since it uses Rust's `char` type, which represents Unicode scalar values.

