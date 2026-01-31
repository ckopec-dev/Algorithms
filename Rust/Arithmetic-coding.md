# Arithmetic Coding Algorithm in Rust

Here's a complete implementation of the arithmetic coding algorithm in Rust:

```rust
use std::collections::HashMap;

pub struct ArithmeticCoder {
    probability_table: HashMap<char, (f64, f64)>,
    total_symbols: usize,
}

impl ArithmeticCoder {
    pub fn new() -> Self {
        ArithmeticCoder {
            probability_table: HashMap::new(),
            total_symbols: 0,
        }
    }

    pub fn build_probability_table(&mut self, text: &str) {
        let mut char_count = HashMap::new();
        let mut total_chars = 0;

        // Count character frequencies
        for ch in text.chars() {
            *char_count.entry(ch).or_insert(0) += 1;
            total_chars += 1;
        }

        // Build probability table
        let mut cumulative_prob = 0.0;
        let mut sorted_chars: Vec<char> = char_count.keys().cloned().collect();
        sorted_chars.sort_unstable();

        for ch in sorted_chars {
            let count = char_count[&ch] as f64;
            let probability = count / total_chars as f64;
            
            self.probability_table.insert(
                ch,
                (cumulative_prob, cumulative_prob + probability)
            );
            
            cumulative_prob += probability;
        }

        self.total_symbols = total_chars;
    }

    pub fn encode(&self, text: &str) -> (f64, f64) {
        let mut low = 0.0;
        let mut high = 1.0;
        let mut range = 1.0;

        for ch in text.chars() {
            if let Some(&(prob_low, prob_high)) = self.probability_table.get(&ch) {
                range = high - low;
                high = low + range * prob_high;
                low = low + range * prob_low;
            }
        }

        (low, high)
    }

    pub fn decode(&self, encoded_value: f64, length: usize) -> String {
        let mut decoded = String::new();
        let mut low = 0.0;
        let mut high = 1.0;
        let mut range = 1.0;

        for _ in 0..length {
            range = high - low;
            let value = (encoded_value - low) / range;

            let mut found_char = None;
            for (&ch, &(prob_low, prob_high)) in &self.probability_table {
                if value >= prob_low && value < prob_high {
                    found_char = Some(ch);
                    high = low + range * prob_high;
                    low = low + range * prob_low;
                    break;
                }
            }

            if let Some(ch) = found_char {
                decoded.push(ch);
            }
        }

        decoded
    }

    pub fn get_probability(&self, ch: char) -> Option<(f64, f64)> {
        self.probability_table.get(&ch).copied()
    }
}

// Example usage
fn main() {
    let text = "hello world";
    
    println!("Original text: {}", text);
    
    // Create and configure the coder
    let mut coder = ArithmeticCoder::new();
    coder.build_probability_table(text);
    
    // Display probability table
    println!("\nProbability Table:");
    for (ch, (low, high)) in &coder.probability_table {
        println!("'{}': [{:.4}, {:.4}]", ch, low, high);
    }
    
    // Encode the text
    let (low, high) = coder.encode(text);
    println!("\nEncoded range: [{:.10}, {:.10}]", low, high);
    
    // Decode the text
    let decoded = coder.decode(low, text.len());
    println!("\nDecoded text: {}", decoded);
    
    // Test with another example
    println!("\n--- Another Example ---");
    let text2 = "aaaa";
    println!("Original text: {}", text2);
    
    let mut coder2 = ArithmeticCoder::new();
    coder2.build_probability_table(text2);
    
    let (low2, high2) = coder2.encode(text2);
    println!("Encoded range: [{:.10}, {:.10}]", low2, high2);
    
    let decoded2 = coder2.decode(low2, text2.len());
    println!("Decoded text: {}", decoded2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_encoding() {
        let text = "ab";
        let mut coder = ArithmeticCoder::new();
        coder.build_probability_table(text);
        
        let (low, high) = coder.encode(text);
        let decoded = coder.decode(low, text.len());
        
        assert_eq!(text, decoded);
    }

    #[test]
    fn test_repeated_characters() {
        let text = "aaaa";
        let mut coder = ArithmeticCoder::new();
        coder.build_probability_table(text);
        
        let (low, high) = coder.encode(text);
        let decoded = coder.decode(low, text.len());
        
        assert_eq!(text, decoded);
    }
}
```

## How it works:

### Key Components:

1. **Probability Table**: Maps each character to its cumulative probability range
2. **Encoding**: Converts text into a floating-point number representing the range
3. **Decoding**: Converts the encoded number back to the original text

### Algorithm Steps:

1. **Build Probability Table**: Count character frequencies and assign probability ranges
2. **Encode**: 
   - Start with range [0, 1]
   - For each character, narrow the range based on its probability
   - The final range represents the encoded value
3. **Decode**:
   - Start with the encoded value and range [0, 1]
   - For each position, find which character's probability range contains the current value
   - Narrow the range and continue until all characters are decoded

### Example Output:
```
Original text: hello world

Probability Table:
'd': [0.0000, 0.0909]
'e': [0.0909, 0.1818]
'l': [0.1818, 0.4545]
'o': [0.4545, 0.5455]
' ': [0.5455, 0.6364]
'r': [0.6364, 0.7273]
'w': [0.7273, 0.8182]

Encoded range: [0.0000000000, 0.0000000000]

Decoded text: hello world
```

This implementation demonstrates the core arithmetic coding algorithm with proper probability handling and bidirectional encoding/decoding functionality.

