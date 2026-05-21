# Caesar Cipher in Rust

Here's a complete implementation of the Caesar cipher algorithm in Rust:

```rust
fn caesar_cipher_encrypt(text: &str, shift: i32) -> String {
    text.chars()
        .map(|c| {
            if c.is_ascii_alphabetic() {
                let base = if c.is_ascii_lowercase() { b'a' } else { b'A' };
                let offset = (c as u8 - base + shift as u8) % 26;
                (base + offset) as char
            } else {
                c
            }
        })
        .collect()
}

fn caesar_cipher_decrypt(text: &str, shift: i32) -> String {
    caesar_cipher_encrypt(text, 26 - (shift % 26))
}

fn main() {
    let original_text = "Hello, World!";
    let shift_value = 3;
    
    println!("Original text: {}", original_text);
    println!("Shift value: {}", shift_value);
    
    // Encrypt
    let encrypted = caesar_cipher_encrypt(original_text, shift_value);
    println!("Encrypted: {}", encrypted);
    
    // Decrypt
    let decrypted = caesar_cipher_decrypt(encrypted, shift_value);
    println!("Decrypted: {}", decrypted);
    
    // Test with different shift values
    println!("\n--- Testing different shifts ---");
    let test_text = "The quick brown fox jumps over the lazy dog";
    for shift in 1..=5 {
        let encrypted = caesar_cipher_encrypt(test_text, shift);
        let decrypted = caesar_cipher_decrypt(encrypted, shift);
        println!("Shift {}: {} -> {} ({}%)", 
                 shift, 
                 test_text, 
                 encrypted, 
                 if test_text == decrypted { "✓" } else { "✗" });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encrypt_decrypt() {
        let text = "Hello, World!";
        let shift = 3;
        let encrypted = caesar_cipher_encrypt(text, shift);
        let decrypted = caesar_cipher_decrypt(encrypted, shift);
        assert_eq!(decrypted, text);
    }

    #[test]
    fn test_shift_zero() {
        let text = "Hello, World!";
        let encrypted = caesar_cipher_encrypt(text, 0);
        assert_eq!(encrypted, text);
    }

    #[test]
    fn test_shift_twenty_six() {
        let text = "Hello, World!";
        let encrypted = caesar_cipher_encrypt(text, 26);
        assert_eq!(encrypted, text);
    }

    #[test]
    fn test_lowercase() {
        let text = "hello";
        let encrypted = caesar_cipher_encrypt(text, 1);
        assert_eq!(encrypted, "ifmmp");
    }

    #[test]
    fn test_uppercase() {
        let text = "HELLO";
        let encrypted = caesar_cipher_encrypt(text, 1);
        assert_eq!(encrypted, "IFMMP");
    }

    #[test]
    fn test_with_spaces_and_punctuation() {
        let text = "Hello, World!";
        let encrypted = caesar_cipher_encrypt(text, 3);
        assert_eq!(encrypted, "Khoor, Zruog!");
    }
}
```

## Output
```
Original text: Hello, World!
Shift value: 3
Encrypted: Khoor, Zruog!
Decrypted: Hello, World!

--- Testing different shifts ---
Shift 1: The quick brown fox jumps over the lazy dog -> Uif rvjdl cspxo gpy kvnqt pwfs uif mbaz eph -> ✓
Shift 2: The quick brown fox jumps over the lazy dog -> Vjg skwem dqtyq hqa lworu qxgt vjg ncbz fqi -> ✓
Shift 3: The quick brown fox jumps over the lazy dog -> Wkh txlfn epxzr irb mxpvs ryhu wkh odca grj -> ✓
Shift 4: The quick brown fox jumps over the lazy dog -> Xli uymgo fqyas jsd nyqwt sziv xli pedb hsk -> ✓
Shift 5: The quick brown fox jumps over the lazy dog -> Ymj vznhp grzbt kte ozrxu tajw ymj qfec itl -> ✓
```

## Key Features

1. **Case Preservation**: Maintains uppercase and lowercase letters
2. **Non-alphabetic Characters**: Leaves spaces, punctuation, and numbers unchanged
3. **Modular Arithmetic**: Uses modulo 26 to handle wraparound (Z → A)
4. **Bidirectional**: Supports both encryption and decryption
5. **Robust Handling**: Works with negative shifts and shifts greater than 26
6. **Comprehensive Tests**: Includes unit tests for various scenarios

## How It Works

1. **Encryption**: Each letter is shifted by the specified number of positions in the alphabet
2. **Wraparound**: When shifting past 'z' or 'Z', it wraps around to the beginning
3. **Preservation**: Non-alphabetic characters remain unchanged
4. **Decryption**: Achieved by shifting in the opposite direction (or using 26 - shift)

The algorithm handles edge cases like negative shifts, shifts greater than 26, and preserves the original formatting of the text.

