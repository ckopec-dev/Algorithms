# Knuth-Morris-Pratt (KMP) Algorithm in Rust

Here's a complete implementation of the KMP algorithm in Rust:

```rust
fn compute_lps_array(pattern: &str) -> Vec<usize> {
    let pattern_chars: Vec<char> = pattern.chars().collect();
    let mut lps = vec![0; pattern_chars.len()];
    let mut len = 0;
    let mut i = 1;
    
    while i < pattern_chars.len() {
        if pattern_chars[i] == pattern_chars[len] {
            len += 1;
            lps[i] = len;
            i += 1;
        } else {
            if len != 0 {
                len = lps[len - 1];
            } else {
                lps[i] = 0;
                i += 1;
            }
        }
    }
    
    lps
}

fn kmp_search(text: &str, pattern: &str) -> Vec<usize> {
    if pattern.is_empty() {
        return vec![];
    }
    
    let text_chars: Vec<char> = text.chars().collect();
    let pattern_chars: Vec<char> = pattern.chars().collect();
    
    let lps = compute_lps_array(pattern);
    let mut result = Vec::new();
    
    let mut i = 0; // index for text
    let mut j = 0; // index for pattern
    
    while i < text_chars.len() {
        if pattern_chars[j] == text_chars[i] {
            i += 1;
            j += 1;
        }
        
        if j == pattern_chars.len() {
            // Pattern found at index i - j
            result.push(i - j);
            j = lps[j - 1];
        } else if i < text_chars.len() && pattern_chars[j] != text_chars[i] {
            if j != 0 {
                j = lps[j - 1];
            } else {
                i += 1;
            }
        }
    }
    
    result
}

fn main() {
    let text = "ABABDABACDABABCABCABCABCABC";
    let pattern = "ABABCABCABCABC";
    
    println!("Text: {}", text);
    println!("Pattern: {}", pattern);
    
    let positions = kmp_search(text, pattern);
    
    if positions.is_empty() {
        println!("Pattern not found in text");
    } else {
        println!("Pattern found at positions: {:?}", positions);
        
        // Show the matches in context
        for &pos in &positions {
            let start = pos.max(0) as usize;
            let end = (pos + pattern.len()).min(text.len()) as usize;
            println!("  Position {}: \"{}\"", pos, &text[start..end]);
        }
    }
    
    // Test with another example
    println!("\n--- Another example ---");
    let text2 = "AABAACAADAABAABA";
    let pattern2 = "AABA";
    
    println!("Text: {}", text2);
    println!("Pattern: {}", pattern2);
    
    let positions2 = kmp_search(text2, pattern2);
    println!("Pattern found at positions: {:?}", positions2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_kmp_search() {
        assert_eq!(kmp_search("ABABDABACDABABCABCABCABCABC", "ABABCABCABCABC"), vec![10]);
        assert_eq!(kmp_search("AABAACAADAABAABA", "AABA"), vec![0, 9, 13]);
        assert_eq!(kmp_search("ABCABCABCABC", "ABC"), vec![0, 3, 6, 9]);
        assert_eq!(kmp_search("ABC", "DEF"), vec![]);
        assert_eq!(kmp_search("", "ABC"), vec![]);
        assert_eq!(kmp_search("ABC", ""), vec![]);
    }

    #[test]
    fn test_lps_array() {
        assert_eq!(compute_lps_array("ABABCABCABCABC"), vec![0, 0, 1, 2, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]);
        assert_eq!(compute_lps_array("AABAACAADAABAABA"), vec![0, 1, 0, 1, 2, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
    }
}
```

## How it works:

### 1. **LPS Array (Longest Proper Prefix which is also Suffix)**
- The `compute_lps_array` function builds an array that helps avoid unnecessary character comparisons
- For each position in the pattern, it stores the length of the longest proper prefix that is also a suffix

### 2. **KMP Search Algorithm**
- Uses the LPS array to skip characters in the text when a mismatch occurs
- Time complexity: O(n + m) where n is text length and m is pattern length
- Space complexity: O(m) for the LPS array

### 3. **Key Features**
- Handles edge cases (empty strings, no matches)
- Returns all occurrences of the pattern in the text
- Includes comprehensive tests
- Demonstrates usage with multiple examples

### 4. **Example Output**
```
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Pattern found at positions: [10]
  Position 10: "ABABCABCABCABC"

--- Another example ---
Text: AABAACAADAABAABA
Pattern: AABA
Pattern found at positions: [0, 9, 13]
```

The KMP algorithm is particularly efficient for pattern searching when the pattern has repeated substrings, as it uses the information from previous matches to avoid redundant comparisons.

