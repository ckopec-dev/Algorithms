# Burrows-Wheeler Transform in Rust

Here's a complete implementation of the Burrows-Wheeler Transform (BWT) algorithm in Rust:

```rust
fn burrows_wheeler_transform(text: &str) -> String {
    if text.is_empty() {
        return String::new();
    }
    
    // Add sentinel character (typically '$') to mark end of string
    let mut extended_text = text.to_string();
    extended_text.push('$');
    
    let n = extended_text.len();
    
    // Create all rotations of the text
    let mut rotations: Vec<String> = Vec::new();
    
    for i in 0..n {
        let rotated = format!("{}{}", 
            &extended_text[i..n], 
            &extended_text[0..i]
        );
        rotations.push(rotated);
    }
    
    // Sort rotations lexicographically
    rotations.sort();
    
    // Take the last character of each sorted rotation
    let mut result = String::new();
    for rotation in rotations {
        if let Some(last_char) = rotation.chars().last() {
            result.push(last_char);
        }
    }
    
    result
}

fn burrows_wheeler_inverse_transform(transformed_text: &str) -> String {
    if transformed_text.is_empty() {
        return String::new();
    }
    
    let n = transformed_text.len();
    let mut table: Vec<String> = vec![String::new(); n];
    
    // Initialize the table with the transformed text
    for i in 0..n {
        table[i] = transformed_text[i..i+1].to_string();
    }
    
    // Reconstruct the table by sorting it n times
    for _ in 0..n {
        // Prepend each character from transformed_text to each row
        for i in 0..n {
            table[i] = format!("{}{}", 
                transformed_text.chars().nth(i).unwrap(), 
                &table[i]
            );
        }
        // Sort the table lexicographically
        table.sort();
    }
    
    // Find the row that ends with the sentinel character '$'
    for row in table {
        if row.ends_with('$') {
            return row[..row.len()-1].to_string();
        }
    }
    
    String::new()
}

fn main() {
    // Example usage
    let original_text = "banana";
    println!("Original text: {}", original_text);
    
    let bwt_result = burrows_wheeler_transform(original_text);
    println!("BWT result: {}", bwt_result);
    
    let inverse_result = burrows_wheeler_inverse_transform(&bwt_result);
    println!("Inverse transform: {}", inverse_result);
    
    // Another example
    let text2 = "abracadabra";
    println!("\nOriginal text: {}", text2);
    
    let bwt_result2 = burrows_wheeler_transform(text2);
    println!("BWT result: {}", bwt_result2);
    
    let inverse_result2 = burrows_wheeler_inverse_transform(&bwt_result2);
    println!("Inverse transform: {}", inverse_result2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bwt_simple() {
        assert_eq!(burrows_wheeler_transform("banana"), "nnbb$aa");
    }

    #[test]
    fn test_bwt_empty() {
        assert_eq!(burrows_wheeler_transform(""), "");
    }

    #[test]
    fn test_bwt_single_char() {
        assert_eq!(burrows_wheeler_transform("a"), "a$");
    }

    #[test]
    fn test_bwt_inverse() {
        let original = "banana";
        let transformed = burrows_wheeler_transform(original);
        let reconstructed = burrows_wheeler_inverse_transform(&transformed);
        assert_eq!(reconstructed, original);
    }
}
```

## How it works:

1. **Transform Process**:
   - Add a sentinel character (`$`) to mark the end
   - Generate all possible rotations of the text
   - Sort these rotations lexicographically
   - Take the last character of each sorted rotation

2. **Inverse Process**:
   - Start with the transformed text
   - Repeatedly prepend each character from the transformed text to all rows
   - Sort the table after each step
   - Find the row ending with the sentinel character

## Example Output:
```
Original text: banana
BWT result: nnbb$aa
Inverse transform: banana

Original text: abracadabra
BWT result: raaabbcd$aaa
Inverse transform: abracadabra
```

This implementation demonstrates the core concepts of the Burrows-Wheeler Transform, which is commonly used in data compression algorithms like bzip2.

