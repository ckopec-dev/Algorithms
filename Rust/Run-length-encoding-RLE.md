# Run-Length Encoding (RLE) in Rust

Here's a complete implementation of the Run-Length Encoding algorithm in Rust:

```rust
fn run_length_encode(input: &str) -> String {
    if input.is_empty() {
        return String::new();
    }
    
    let mut result = String::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    
    while i < chars.len() {
        let current_char = chars[i];
        let mut count = 1;
        
        // Count consecutive occurrences
        while i + count < chars.len() && chars[i + count] == current_char {
            count += 1;
        }
        
        // Append count and character to result
        if count > 1 {
            result.push_str(&count.to_string());
        }
        result.push(current_char);
        
        i += count;
    }
    
    result
}

fn run_length_decode(encoded: &str) -> String {
    let mut result = String::new();
    let chars: Vec<char> = encoded.chars().collect();
    let mut i = 0;
    
    while i < chars.len() {
        // Parse the count (could be multiple digits)
        let mut count = 0;
        while i < chars.len() && chars[i].is_ascii_digit() {
            count = count * 10 + (chars[i] as u8 - b'0') as usize;
            i += 1;
        }
        
        // If no count was found, it means count is 1
        if count == 0 {
            count = 1;
        }
        
        // Get the character to repeat
        if i < chars.len() {
            let character = chars[i];
            for _ in 0..count {
                result.push(character);
            }
            i += 1;
        }
    }
    
    result
}

fn main() {
    // Test cases
    let test_strings = vec![
        "aaaabbbbcccc",
        "aabbcc",
        "aaaaaa",
        "abcdef",
        "aabbbaa",
        "",
    ];
    
    println!("Run-Length Encoding Examples:");
    println!("============================");
    
    for test in test_strings {
        let encoded = run_length_encode(test);
        let decoded = run_length_decode(&encoded);
        
        println!("Original:    \"{}\"", test);
        println!("Encoded:     \"{}\"", encoded);
        println!("Decoded:     \"{}\"", decoded);
        println!("Match:       {}\n", test == decoded);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encode() {
        assert_eq!(run_length_encode("aaaabbbbcccc"), "4a4b4c");
        assert_eq!(run_length_encode("aabbcc"), "2a2b2c");
        assert_eq!(run_length_encode("aaaaaa"), "6a");
        assert_eq!(run_length_encode("abcdef"), "abcdef");
        assert_eq!(run_length_encode(""), "");
    }

    #[test]
    fn test_decode() {
        assert_eq!(run_length_decode("4a4b4c"), "aaaabbbbcccc");
        assert_eq!(run_length_decode("2a2b2c"), "aabbcc");
        assert_eq!(run_length_decode("6a"), "aaaaaa");
        assert_eq!(run_length_decode("abcdef"), "abcdef");
        assert_eq!(run_length_decode(""), "");
    }

    #[test]
    fn test_encode_decode() {
        let test_cases = vec![
            "aaaabbbbcccc",
            "aabbbaa",
            "hello world",
            "a",
            "",
        ];
        
        for test in test_cases {
            let encoded = run_length_encode(test);
            let decoded = run_length_decode(&encoded);
            assert_eq!(test, decoded);
        }
    }
}
```

## Example Output:
```
Run-Length Encoding Examples:
============================
Original:    "aaaabbbbcccc"
Encoded:     "4a4b4c"
Decoded:     "aaaabbbbcccc"
Match:       true

Original:    "aabbcc"
Encoded:     "2a2b2c"
Decoded:     "aabbcc"
Match:       true

Original:    "aaaaaa"
Encoded:     "6a"
Decoded:     "aaaaaa"
Match:       true

Original:    "abcdef"
Encoded:     "abcdef"
Decoded:     "abcdef"
Match:       true

Original:    "aabbbaa"
Encoded:     "2a3b2a"
Decoded:     "aabbbaa"
Match:       true

Original:    ""
Encoded:     ""
Decoded:     ""
Match:       true
```

## Key Features:

1. **Encoding Function**: `run_length_encode()` - Compresses consecutive characters
2. **Decoding Function**: `run_length_decode()` - Reverses the encoding process
3. **Handles Edge Cases**: Empty strings, single characters, no consecutive characters
4. **Multiple Digit Counts**: Properly handles counts > 9 (e.g., "12a" for 12 consecutive 'a's)
5. **Comprehensive Tests**: Unit tests for both encoding and decoding functions

## How It Works:

- **Encoding**: Groups consecutive identical characters and prefixes them with their count
- **Decoding**: Reads numbers and repeats subsequent characters that many times
- **Special Case**: Single characters are not prefixed with a count number

The algorithm is efficient with O(n) time complexity for both encoding and decoding operations.

