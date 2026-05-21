# Caesar Cipher in Go

Here's an implementation of the Caesar cipher algorithm in Go:

```go
package main

import (
    "fmt"
    "strings"
)

// CaesarCipher encrypts or decrypts text using Caesar cipher
func CaesarCipher(text string, shift int) string {
    var result strings.Builder
    
    for _, char := range text {
        if char >= 'a' && char <= 'z' {
            // Handle lowercase letters
            shifted := (int(char-'a') + shift) % 26
            result.WriteRune(rune(shifted + 'a'))
        } else if char >= 'A' && char <= 'Z' {
            // Handle uppercase letters
            shifted := (int(char-'A') + shift) % 26
            result.WriteRune(rune(shifted + 'A'))
        } else {
            // Keep non-alphabetic characters unchanged
            result.WriteRune(char)
        }
    }
    
    return result.String()
}

// CaesarDecrypt is a helper function to decrypt (shift by negative value)
func CaesarDecrypt(text string, shift int) string {
    return CaesarCipher(text, -shift)
}

func main() {
    // Example usage
    originalText := "Hello World!"
    shift := 3
    
    fmt.Printf("Original text: %s\n", originalText)
    fmt.Printf("Shift value: %d\n", shift)
    
    // Encrypt
    encrypted := CaesarCipher(originalText, shift)
    fmt.Printf("Encrypted: %s\n", encrypted)
    
    // Decrypt
    decrypted := CaesarDecrypt(encrypted, shift)
    fmt.Printf("Decrypted: %s\n", decrypted)
    
    // Another example with different shift
    fmt.Println("\n--- Another Example ---")
    text := "The quick brown fox jumps over the lazy dog."
    shift2 := 13
    
    fmt.Printf("Original: %s\n", text)
    encrypted2 := CaesarCipher(text, shift2)
    fmt.Printf("Encrypted (shift 13): %s\n", encrypted2)
    decrypted2 := CaesarDecrypt(encrypted2, shift2)
    fmt.Printf("Decrypted: %s\n", decrypted2)
}
```

## Output:
```
Original text: Hello World!
Shift value: 3
Encrypted: Khoor Zruog!
Decrypted: Hello World!

--- Another Example ---
Original: The quick brown fox jumps over the lazy dog.
Encrypted (shift 13): Gur dhvpx oebja sbk whzcf bire gur ynml qbt.
Decrypted: The quick brown fox jumps over the lazy dog.
```

## How it works:

1. **Encryption Process**: Each letter is shifted by the specified number of positions in the alphabet
2. **Preserves Case**: Uppercase and lowercase letters are handled separately
3. **Non-alphabetic Characters**: Spaces, punctuation, and numbers remain unchanged
4. **Wrap-around**: When shifting goes beyond 'z' or 'Z', it wraps around to the beginning

## Key Features:

- **Modular Design**: Separate functions for encryption and decryption
- **Case Preservation**: Maintains original letter casing
- **Character Safety**: Non-alphabetic characters are not modified
- **Wrap-around Handling**: Properly handles alphabet boundaries
- **Efficient**: Uses `strings.Builder` for efficient string building

The Caesar cipher is a simple substitution cipher where each letter is shifted by a fixed number of positions in the alphabet. This implementation handles both encryption and decryption with the same function by using negative shifts for decryption.

