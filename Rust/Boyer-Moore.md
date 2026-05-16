# Boyer-Moore Algorithm in Rust

Here's a complete implementation of the Boyer-Moore string searching algorithm in Rust:

```rust
fn boyer_moore_search(text: &str, pattern: &str) -> Vec<usize> {
    if pattern.is_empty() || text.is_empty() {
        return vec![];
    }
    
    let text_bytes = text.as_bytes();
    let pattern_bytes = pattern.as_bytes();
    let text_len = text_bytes.len();
    let pattern_len = pattern_bytes.len();
    
    // Preprocessing: Create bad character table
    let mut bad_char_table = [pattern_len; 256];
    for i in 0..pattern_len {
        bad_char_table[pattern_bytes[i] as usize] = pattern_len - 1 - i;
    }
    
    let mut matches = Vec::new();
    let mut text_index = 0;
    
    while text_index <= text_len - pattern_len {
        let mut pattern_index = pattern_len - 1;
        
        // Compare from right to left
        while pattern_index >= 0 && text_bytes[text_index + pattern_index] == pattern_bytes[pattern_index] {
            pattern_index -= 1;
        }
        
        // If pattern is found
        if pattern_index < 0 {
            matches.push(text_index);
            text_index += 1;
        } else {
            // Shift based on bad character rule
            let bad_char_shift = bad_char_table[text_bytes[text_index + pattern_index] as usize];
            text_index += bad_char_shift.max(1);
        }
    }
    
    matches
}

fn main() {
    let text = "ABAAABCDABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABC

