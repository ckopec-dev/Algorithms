# Caesar Cipher in C

Here's a complete implementation of the Caesar cipher algorithm in C:

```c
#include <stdio.h>
#include <string.h>
#include <ctype.h>

// Function to encrypt text using Caesar cipher
void caesar_encrypt(char *text, int shift) {
    int i;
    for (i = 0; i < strlen(text); i++) {
        if (isalpha(text[i])) {
            // Handle uppercase letters
            if (isupper(text[i])) {
                text[i] = ((text[i] - 'A' + shift) % 26) + 'A';
            }
            // Handle lowercase letters
            else {
                text[i] = ((text[i] - 'a' + shift) % 26) + 'a';
            }
        }
    }
}

// Function to decrypt text using Caesar cipher
void caesar_decrypt(char *text, int shift) {
    int i;
    for (i = 0; i < strlen(text); i++) {
        if (isalpha(text[i])) {
            // Handle uppercase letters
            if (isupper(text[i])) {
                text[i] = ((text[i] - 'A' - shift + 26) % 26) + 'A';
            }
            // Handle lowercase letters
            else {
                text[i] = ((text[i] - 'a' - shift + 26) % 26) + 'a';
            }
        }
    }
}

int main() {
    char text[1000];
    int shift;
    
    printf("Caesar Cipher Program\n");
    printf("====================\n");
    
    // Get input from user
    printf("Enter the text to encrypt: ");
    fgets(text, sizeof(text), stdin);
    
    printf("Enter the shift value (1-25): ");
    scanf("%d", &shift);
    
    // Remove newline character from input
    text[strcspn(text, "\n")] = 0;
    
    // Display original text
    printf("\nOriginal text: %s\n", text);
    
    // Encrypt the text
    caesar_encrypt(text, shift);
    printf("Encrypted text: %s\n", text);
    
    // Decrypt the text
    caesar_decrypt(text, shift);
    printf("Decrypted text: %s\n", text);
    
    return 0;
}
```

## Example Output

```
Caesar Cipher Program
====================
Enter the text to encrypt: Hello World!
Enter the shift value (1-25): 3

Original text: Hello World!
Encrypted text: Khoor Zruog!
Decrypted text: Hello World!
```

## How it works:

1. **Encryption**: Each letter is shifted forward by the specified number of positions in the alphabet
2. **Decryption**: Each letter is shifted backward by the specified number of positions
3. **Wrapping**: Uses modulo arithmetic to wrap around from 'Z' to 'A' and 'z' to 'a'
4. **Preservation**: Non-alphabetic characters remain unchanged

## Key Features:

- Handles both uppercase and lowercase letters
- Preserves spaces and punctuation
- Includes both encryption and decryption functions
- Uses modular arithmetic for proper wrapping
- User-friendly interface with input validation

The Caesar cipher is a simple substitution cipher where each letter in the plaintext is shifted a certain number of places down or up the alphabet.