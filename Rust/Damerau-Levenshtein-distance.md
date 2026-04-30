# Damerau-Levenshtein Distance in Rust

Here's a complete implementation of the Damerau-Levenshtein distance algorithm in Rust:

```rust
fn damerau_levenshtein_distance(s1: &str, s2: &str) -> usize {
    let chars1: Vec<char> = s1.chars().collect();
    let chars2: Vec<char> = s2.chars().collect();
    let len1 = chars1.len();
    let len2 = chars2.len();
    
    // Create a matrix to store distances
    let mut dp = vec![vec![0; len2 + 1]; len1 + 1];
    
    // Initialize base cases
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
            
            dp[i][j] = std::cmp::min(
                std::cmp::min(
                    dp[i - 1][j] + 1,     // deletion
                    dp[i][j - 1] + 1,     // insertion
                ),
                dp[i - 1][j - 1] + cost,  // substitution
            );
            
            // Check for transposition (Damerau-Levenshtein specific)
            if i > 1 && j > 1 && chars1[i - 1] == chars2[j - 2] && chars1[i - 2] == chars2[j - 1] {
                dp[i][j] = std::cmp::min(dp[i][j], dp[i - 2][j - 2] + 1);
            }
        }
    }
    
    dp[len1][len2]
}

fn main() {
    // Test cases
    println!("Damerau-Levenshtein Distance Examples:");
    println!("=====================================");
    
    let test_cases = vec![
        ("kitten", "sitting"),
        ("saturday", "sunday"),
        ("hello", "world"),
        ("rust", "rust"),
        ("abc", "acb"),
        ("", "abc"),
        ("abc", ""),
        ("algorithm", "logarithm"),
    ];
    
    for (s1, s2) in test_cases {
        let distance = damerau_levenshtein_distance(s1, s2);
        println!("'{}' -> '{}' = {}", s1, s2, distance);
    }
    
    // Detailed example with explanation
    println!("\nDetailed Example:");
    println!("'kitten' vs 'sitting':");
    println!("- kitten -> sitten (substitution of 'k' with 's') = 1");
    println!("- sitten -> sittin (substitution of 'e' with 'i') = 1");
    println!("- sittin -> sitting (insertion of 'g') = 1");
    println!("- Total: 3");
    
    let distance = damerau_levenshtein_distance("kitten", "sitting");
    println!("Actual distance: {}", distance);
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
    }

    #[test]
    fn test_transposition() {
        // Transposition should be counted as 1 operation
        assert_eq!(damerau_levenshtein_distance("abc", "acb"), 1);
        assert_eq!(damerau_levenshtein_distance("abc", "bca"), 1);
    }

    #[test]
    fn test_edge_cases() {
        assert_eq!(damerau_levenshtein_distance("", "abc"), 3);
        assert_eq!(damerau_levenshtein_distance("abc", ""), 3);
    }
}
```

## Output
```
Damerau-Levenshtein Distance Examples:
=====================================
'kitten' -> 'sitting' = 3
'saturday' -> 'sunday' = 3
'hello' -> 'world' = 4
'rust' -> 'rust' = 0
'abc' -> 'acb' = 1
'' -> 'abc' = 3
'abc' -> '' = 3
'algorithm' -> 'logarithm' = 3

Detailed Example:
'kitten' vs 'sitting':
- kitten -> sitten (substitution of 'k' with 's') = 1
- sitten -> sittin (substitution of 'e' with 'i') = 1
- sittin -> sitting (insertion of 'g') = 1
- Total: 3
Actual distance: 3
```

## Key Features

1. **Transposition Support**: Unlike standard Levenshtein distance, this implementation includes transposition operations (swapping adjacent characters)
2. **Unicode Support**: Works with any Unicode characters
3. **Efficient Implementation**: Uses dynamic programming with O(m×n) time complexity
4. **Comprehensive Testing**: Includes unit tests for various scenarios

## How It Works

The algorithm uses dynamic programming to build a matrix where each cell `[i][j]` represents the minimum edit distance between the first `i` characters of string 1 and the first `j` characters of string 2. The key addition for Damerau-Levenshtein is the transposition check that considers swapping adjacent characters as a single operation.

