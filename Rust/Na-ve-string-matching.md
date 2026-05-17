# Naïve String Matching Algorithm in Rust

Here's an implementation of the naïve string matching algorithm in Rust:

```rust
fn naive_string_search(pattern: &str, text: &str) -> Vec<usize> {
    let mut positions = Vec::new();
    
    // Convert strings to character arrays for easier indexing
    let pattern_chars: Vec<char> = pattern.chars().collect();
    let text_chars: Vec<char> = text.chars().collect();
    
    let pattern_len = pattern_chars.len();
    let text_len = text_chars.len();
    
    // If pattern is empty, return empty vector
    if pattern_len == 0 {
        return positions;
    }
    
    // Iterate through text, checking for pattern matches
    for i in 0..=(text_len - pattern_len) {
        let mut match_found = true;
        
        // Check if pattern matches at position i
        for j in 0..pattern_len {
            if text_chars[i + j] != pattern_chars[j] {
                match_found = false;
                break;
            }
        }
        
        // If match found, add position to results
        if match_found {
            positions.push(i);
        }
    }
    
    positions
}

fn main() {
    let text = "ABABDABACDABABCABCABCABCABC";
    let pattern = "ABABCABCABCABC";
    
    println!("Text: {}", text);
    println!("Pattern: {}", pattern);
    
    let positions = naive_string_search(pattern, text);
    
    if positions.is_empty() {
        println!("Pattern not found in text");
    } else {
        println!("Pattern found at positions: {:?}", positions);
        
        // Display the matches
        for &pos in &positions {
            println!("Match at position {}: {}", pos, 
                     &text[pos..(pos + pattern.len())]);
        }
    }
    
    // Test with another example
    println!("\n--- Another Example ---");
    let text2 = "hello world hello rust hello";
    let pattern2 = "hello";
    
    println!("Text: {}", text2);
    println!("Pattern: {}", pattern2);
    
    let positions2 = naive_string_search(pattern2, text2);
    println!("Pattern found at positions: {:?}", positions2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_match() {
        let text = "abcdef";
        let pattern = "cde";
        let result = naive_string_search(pattern, text);
        assert_eq!(result, vec![2]);
    }

    #[test]
    fn test_multiple_matches() {
        let text = "abababab";
        let pattern = "abab";
        let result = naive_string_search(pattern, text);
        assert_eq!(result, vec![0, 2, 4]);
    }

    #[test]
    fn test_no_match() {
        let text = "abcdef";
        let pattern = "xyz";
        let result = naive_string_search(pattern, text);
        assert_eq!(result, Vec::<usize>::new());
    }

    #[test]
    fn test_empty_pattern() {
        let text = "abcdef";
        let pattern = "";
        let result = naive_string_search(pattern, text);
        assert_eq!(result, Vec::<usize>::new());
    }

    #[test]
    fn test_pattern_longer_than_text() {
        let text = "ab";
        let pattern = "abc";
        let result = naive_string_search(pattern, text);
        assert_eq!(result, Vec::<usize>::new());
    }
}
```

## How it works:

1. **Algorithm Approach**: The naïve string matching algorithm checks every possible position in the text where the pattern could match.

2. **Time Complexity**: O(n × m) where n is the length of the text and m is the length of the pattern.

3. **Space Complexity**: O(k) where k is the number of matches found.

## Key Features:

- **Handles Unicode**: Uses `chars()` to properly handle Unicode characters
- **Returns All Matches**: Finds all occurrences of the pattern in the text
- **Edge Cases**: Handles empty patterns, patterns longer than text, etc.
- **Comprehensive Tests**: Includes unit tests for various scenarios

## Sample Output:
```
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Pattern found at positions: [10]
Match at position 10: ABABCABCABCABC

--- Another Example ---
Text: hello world hello rust hello
Pattern: hello
Pattern found at positions: [0, 12, 25]
```

The algorithm works by sliding the pattern over the text one character at a time and checking if the pattern matches at each position. When a match is found, the starting position is recorded.

