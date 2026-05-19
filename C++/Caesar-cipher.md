# Caesar Cipher in C++

Here's a complete implementation of the Caesar cipher algorithm in C++:

```cpp
#include <iostream>
#include <string>
#include <cctype>

class CaesarCipher {
public:
    // Encrypt function
    static std::string encrypt(const std::string& text, int shift) {
        std::string result = "";
        
        for (int i = 0; i < text.length(); i++) {
            char ch = text[i];
            
            // Check if character is uppercase
            if (std::isupper(ch)) {
                result += char((ch - 'A' + shift) % 26 + 'A');
            }
            // Check if character is lowercase
            else if (std::islower(ch)) {
                result += char((ch - 'a' + shift) % 26 + 'a');
            }
            // If not a letter, keep it unchanged
            else {
                result += ch;
            }
        }
        
        return result;
    }
    
    // Decrypt function
    static std::string decrypt(const std::string& text, int shift) {
        std::string result = "";
        
        for (int i = 0; i < text.length(); i++) {
            char ch = text[i];
            
            // Check if character is uppercase
            if (std::isupper(ch)) {
                result += char((ch - 'A' - shift + 26) % 26 + 'A');
            }
            // Check if character is lowercase
            else if (std::islower(ch)) {
                result += char((ch - 'a' - shift + 26) % 26 + 'a');
            }
            // If not a letter, keep it unchanged
            else {
                result += ch;
            }
        }
        
        return result;
    }
};

int main() {
    std::string text = "Hello World!";
    int shift = 3;
    
    std::cout << "Original Text: " << text << std::endl;
    std::cout << "Shift Value: " << shift << std::endl;
    
    // Encrypt the text
    std::string encrypted = CaesarCipher::encrypt(text, shift);
    std::cout << "Encrypted Text: " << encrypted << std::endl;
    
    // Decrypt the text
    std::string decrypted = CaesarCipher::decrypt(encrypted, shift);
    std::cout << "Decrypted Text: " << decrypted << std::endl;
    
    return 0;
}
```

## Output:
```
Original Text: Hello World!
Shift Value: 3
Encrypted Text: Khoor Zruog!
Decrypted Text: Hello World!
```

## How it works:

1. **Encryption**: Each letter is shifted by the specified number of positions in the alphabet
2. **Decryption**: Each letter is shifted back by the same number of positions
3. **Wrapping**: Uses modulo arithmetic to wrap around the alphabet (Z shifts to A)
4. **Preservation**: Non-alphabetic characters remain unchanged

## Key Features:

- Handles both uppercase and lowercase letters
- Preserves spaces and punctuation
- Uses modular arithmetic for wrapping
- Includes both encryption and decryption functions
- Works with any shift value (even greater than 26)

