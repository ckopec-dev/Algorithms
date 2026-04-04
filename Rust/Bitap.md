# Bitap Algorithm Implementation in Rust

The Bitap algorithm is a string searching algorithm that uses bitwise operations to find occurrences of a pattern in a text. Here's a complete implementation:

```rust
pub struct Bitap {
    pattern: String,
    pattern_len: usize,
    mask: Vec<u32>,
}

impl Bitap {
    pub fn new(pattern: &str) -> Self {
        let pattern = pattern.to_string();
        let pattern_len = pattern.len();
        let mut mask = vec![u32::MAX; 256]; // ASCII table size
        
        // Initialize the mask for each character
        for (i, byte) in pattern.bytes().enumerate() {
            let bit_pos = pattern_len - 1 - i;
            mask[byte as usize] &= !(1 << bit_pos);
        }
        
        Bitap {
            pattern,
            pattern_len,
            mask,
        }
    }
    
    pub fn search(&self, text: &str) -> Vec<usize> {
        let mut results = Vec::new();
        let mut state = u32::MAX;
        let pattern_len = self.pattern_len;
        
        // Initialize state for the first character
        for i in 0..pattern_len {
            state &= !(1 << i);
        }
        
        // Process each character in the text
        for (i, byte) in text.bytes().enumerate() {
            // Update state using bitwise operations
            state = (state >> 1) | self.mask[byte as usize];
            
            // Check if we found a match
            if (state & (1 << (pattern_len - 1))) == 0 {
                results.push(i - pattern_len + 1);
            }
        }
        
        results
    }
    
    pub fn search_with_distance(&self, text: &str, max_distance: usize) -> Vec<(usize, usize)> {
        let mut results = Vec::new();
        let pattern_len = self.pattern_len;
        let mut state = vec![u32::MAX; pattern_len + 1];
        
        // Initialize the first state
        for i in 0..=pattern_len {
            state[i] = (1 << i) - 1;
        }
        
        for (i, byte) in text.bytes().enumerate() {
            // Update all states
            let mut new_state = vec![0; pattern_len + 1];
            new_state[0] = state[0] >> 1;
            
            for j in 1..=pattern_len {
                new_state[j] = (state[j] >> 1) | self.mask[byte as usize];
                if j > 0 {
                    new_state[j] |= (state[j - 1] & 1) << (pattern_len - 1);
                }
            }
            
            state = new_state;
            
            // Check if we found a match within the allowed distance
            if (state[pattern_len] & (1 << (pattern_len - 1))) == 0 {
                let distance = self.calculate_distance(&state, pattern_len);
                if distance <= max_distance {
                    results.push((i - pattern_len + 1, distance));
                }
            }
        }
        
        results
    }
    
    fn calculate_distance(&self, state: &[u32], pattern_len: usize) -> usize {
        let mut distance = 0;
        for i in 0..pattern_len {
            if (state[pattern_len] & (1 << i)) != 0 {
                distance += 1;
            }
        }
        distance
    }
}

// Example usage
fn main() {
    // Basic search example
    let bitap = Bitap::new("abc");
    let text = "abcdef abcdefg abchijk";
    let matches = bitap.search(text);
    
    println!("Pattern: 'abc'");
    println!("Text: '{}'", text);
    println!("Matches at positions: {:?}", matches);
    
    // Example with multiple matches
    let bitap2 = Bitap::new("test");
    let text2 = "this is a test string for testing purposes test";
    let matches2 = bitap2.search(text2);
    
    println!("\nPattern: 'test'");
    println!("Text: '{}'", text2);
    println!("Matches at positions: {:?}", matches2);
    
    // Example with distance search
    let bitap3 = Bitap::new("abc");
    let text3 = "abdc abcc abec";
    let matches3 = bitap3.search_with_distance(text3, 1);
    
    println!("\nPattern: 'abc' with max distance 1");
    println!("Text: '{}'", text3);
    println!("Matches with distances: {:?}", matches3);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_search() {
        let bitap = Bitap::new("abc");
        let text = "abcdef";
        let matches = bitap.search(text);
        assert_eq!(matches, vec![0]);
    }

    #[test]
    fn test_multiple_matches() {
        let bitap = Bitap::new("ab");
        let text = "ababab";
        let matches = bitap.search(text);
        assert_eq!(matches, vec![0, 2, 4]);
    }

    #[test]
    fn test_no_matches() {
        let bitap = Bitap::new("xyz");
        let text = "abcdef";
        let matches = bitap.search(text);
        assert_eq!(matches, Vec::<usize>::new());
    }

    #[test]
    fn test_empty_pattern() {
        let bitap = Bitap::new("");
        let text = "abcdef";
        let matches = bitap.search(text);
        // Empty pattern should match at every position
        assert_eq!(matches, vec![0, 1, 2, 3, 4, 5]);
    }
}
```

## Key Features of this Implementation:

1. **Bitwise Operations**: Uses bit manipulation for efficient pattern matching
2. **Preprocessing**: Builds a mask table for each character in the pattern
3. **Multiple Search Modes**: 
   - Basic exact matching
   - Approximate matching with edit distance
4. **Memory Efficient**: Uses only O(1) extra space for the mask table
5. **Time Complexity**: O(nm) where n is text length and m is pattern length

## Usage Examples:

```rust
// Basic usage
let bitap = Bitap::new("hello");
let matches = bitap.search("say hello world hello");

// With distance constraint
let matches_with_distance = bitap.search_with_distance("say hello world hello", 1);
```

This implementation demonstrates the power of bitwise operations in string matching algorithms, making it particularly efficient for searching in large texts.

