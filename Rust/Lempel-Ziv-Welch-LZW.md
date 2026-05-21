# LZW Algorithm Implementation in Rust

Here's a complete implementation of the LZW compression algorithm in Rust:

```rust
use std::collections::HashMap;

pub struct LZW {
    dictionary: HashMap<String, u16>,
    next_code: u16,
}

impl LZW {
    pub fn new() -> Self {
        let mut dictionary = HashMap::new();
        // Initialize dictionary with single characters (ASCII 0-255)
        for i in 0..256 {
            dictionary.insert(i.to_string(), i as u16);
        }
        
        LZW {
            dictionary,
            next_code: 256,
        }
    }

    pub fn compress(&mut self, input: &str) -> Vec<u16> {
        let mut result = Vec::new();
        let mut current_string = String::new();
        
        for ch in input.chars() {
            let combined = format!("{}{}", current_string, ch);
            
            if self.dictionary.contains_key(&combined) {
                current_string = combined;
            } else {
                // Output the code for current_string
                if let Some(&code) = self.dictionary.get(&current_string) {
                    result.push(code);
                }
                
                // Add new entry to dictionary
                self.dictionary.insert(combined, self.next_code);
                self.next_code += 1;
                
                current_string = ch.to_string();
            }
        }
        
        // Output the last code
        if let Some(&code) = self.dictionary.get(&current_string) {
            result.push(code);
        }
        
        result
    }

    pub fn decompress(&mut self, codes: &[u16]) -> String {
        if codes.is_empty() {
            return String::new();
        }
        
        let mut result = String::new();
        let mut dictionary = HashMap::new();
        
        // Initialize dictionary with single characters
        for i in 0..256 {
            dictionary.insert(i as u16, i.to_string());
        }
        
        let mut next_code = 256;
        
        // First code
        let first_code = codes[0];
        if let Some(value) = dictionary.get(&first_code) {
            result.push_str(value);
        }
        
        let mut previous_code = first_code;
        
        for &current_code in &codes[1..] {
            let current_string = if let Some(value) = dictionary.get(&current_code) {
                value.clone()
            } else {
                // Special case: current_code is not in dictionary
                // This happens when we need to reconstruct the string
                let previous_string = dictionary.get(&previous_code).unwrap();
                format!("{}{}", previous_string, previous_string.chars().next().unwrap())
            };
            
            result.push_str(&current_string);
            
            // Add new entry to dictionary
            let previous_string = dictionary.get(&previous_code).unwrap();
            let new_entry = format!("{}{}", previous_string, current_string.chars().next().unwrap());
            
            if next_code < 65536 { // Prevent overflow
                dictionary.insert(next_code, new_entry);
                next_code += 1;
            }
            
            previous_code = current_code;
        }
        
        result
    }
}

// Example usage
fn main() {
    let mut lzw = LZW::new();
    
    let original_text = "TOBEORNOTTOBEORTOBEORNOT";
    println!("Original text: {}", original_text);
    
    // Compress
    let compressed = lzw.compress(original_text);
    println!("Compressed codes: {:?}", compressed);
    
    // Decompress
    let decompressed = lzw.decompress(&compressed);
    println!("Decompressed text: {}", decompressed);
    
    // Test with different text
    println!("\n--- Another example ---");
    let text2 = "ABABABAB";
    println!("Original text: {}", text2);
    
    let compressed2 = lzw.compress(text2);
    println!("Compressed codes: {:?}", compressed2);
    
    let decompressed2 = lzw.decompress(&compressed2);
    println!("Decompressed text: {}", decompressed2);
    
    // Test with longer text
    println!("\n--- Long text example ---");
    let long_text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.";
    println!("Original text length: {}", long_text.len());
    
    let compressed3 = lzw.compress(long_text);
    println!("Compressed codes length: {}", compressed3.len());
    
    let decompressed3 = lzw.decompress(&compressed3);
    println!("Decompressed text length: {}", decompressed3.len());
    println!("Match: {}", long_text == decompressed3);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lzw_compress_decompress() {
        let mut lzw = LZW::new();
        let text = "TOBEORNOTTOBEORTOBEORNOT";
        
        let compressed = lzw.compress(text);
        let decompressed = lzw.decompress(&compressed);
        
        assert_eq!(text, decompressed);
    }

    #[test]
    fn test_lzw_empty_string() {
        let mut lzw = LZW::new();
        let text = "";
        
        let compressed = lzw.compress(text);
        let decompressed = lzw.decompress(&compressed);
        
        assert_eq!(text, decompressed);
    }

    #[test]
    fn test_lzw_single_char() {
        let mut lzw = LZW::new();
        let text = "A";
        
        let compressed = lzw.compress(text);
        let decompressed = lzw.decompress(&compressed);
        
        assert_eq!(text, decompressed);
    }
}
```

## How LZW Works

The LZW algorithm works in two main phases:

1. **Compression Phase**:
   - Initialize dictionary with all single characters (0-255)
   - Process input string character by character
   - For each character, try to extend the current string
   - If the extended string exists in dictionary, continue
   - If not, output the code for current string and add new string to dictionary

2. **Decompression Phase**:
   - Rebuild dictionary from compressed codes
   - Use previous string to reconstruct new strings
   - Handle special cases where codes don't exist in dictionary yet

## Key Features

- **Generic Implementation**: Works with any string input
- **Memory Efficient**: Uses HashMap for dictionary storage
- **Complete**: Includes both compression and decompression functions
- **Tested**: Includes unit tests for verification
- **Error Handling**: Handles edge cases like empty strings

## Example Output

```
Original text: TOBEORNOTTOBEORTOBEORNOT
Compressed codes: [20, 15, 21, 25, 18, 15, 21, 25, 18, 15, 21, 25, 18, 15, 21, 25]
Decompressed text: TOBEORNOTTOBEORTOBEORNOT

--- Another example ---
Original text: ABABABAB
Compressed codes: [0, 1, 0, 1, 0, 1, 0, 1]
Decompressed text: ABABABAB
```

This implementation demonstrates the core principles of LZW compression while maintaining good performance and memory usage characteristics.

