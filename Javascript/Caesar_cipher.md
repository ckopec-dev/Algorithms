# Caesar Cipher in JavaScript

Here's a complete implementation of the Caesar cipher algorithm in JavaScript:

```javascript
function caesarCipher(text, shift) {
    let result = '';
    
    for (let i = 0; i < text.length; i++) {
        let char = text[i];
        
        // Check if character is uppercase
        if (char >= 'A' && char <= 'Z') {
            // Apply shift and wrap around using modulo
            let shifted = ((char.charCodeAt(0) - 65 + shift) % 26) + 65;
            result += String.fromCharCode(shifted);
        }
        // Check if character is lowercase
        else if (char >= 'a' && char <= 'z') {
            // Apply shift and wrap around using modulo
            let shifted = ((char.charCodeAt(0) - 97 + shift) % 26) + 97;
            result += String.fromCharCode(shifted);
        }
        // Non-alphabetic characters remain unchanged
        else {
            result += char;
        }
    }
    
    return result;
}

// Example usage:
console.log("Original text: Hello World!");
console.log("Encrypted (shift 3):", caesarCipher("Hello World!", 3));
console.log("Decrypted (shift -3):", caesarCipher("Khoor Zruog!", -3));

// More examples:
console.log("\nMore examples:");
console.log("ABC with shift 1:", caesarCipher("ABC", 1));        // BCD
console.log("XYZ with shift 3:", caesarCipher("XYZ", 3));        // ABC
console.log("Hello123 with shift 5:", caesarCipher("Hello123", 5)); // Mjqqt123
```

## How it works:

1. **Character Processing**: The function processes each character in the input text
2. **Case Handling**: Separately handles uppercase and lowercase letters
3. **Shift Calculation**: Uses modulo arithmetic to wrap around the alphabet
4. **Non-alphabetic Characters**: Leaves spaces, numbers, and punctuation unchanged

## Output:
```
Original text: Hello World!
Encrypted (shift 3): Khoor Zruog!
Decrypted (shift -3): Hello World!

More examples:
ABC with shift 1: BCD
XYZ with shift 3: ABC
Hello123 with shift 5: Mjqqt123
```

The Caesar cipher shifts each letter by a fixed number of positions in the alphabet, wrapping around from Z back to A when necessary.

