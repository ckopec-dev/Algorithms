# Caesar Cipher in C

Here's a complete implementation of the Caesar cipher algorithm in C:

```c
#include <stdio.h>
#include <string.h>
#include <ctype.h>

// Function to encrypt text using Caesar cipher
void caesarEncrypt(char *text, int shift) {
    int i;
    for (i = 0; text[i] != '\0'; i++) {
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
void caesarDecrypt(char *text, int shift) {
    int i;
    for (i = 0; text[i] != '\0'; i++) {
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
    
    printf("Enter the shift value (0-25): ");
    scanf("%d", &shift);
    
    // Ensure shift is within valid range
    shift = shift % 26;
    
    // Display original text
    printf("\nOriginal text: %s", text);
    
    // Encrypt the text
    caesarEncrypt(text, shift);
    printf("Encrypted text: %s", text);
    
    // Decrypt the text
    caesarDecrypt(text, shift);
    printf("Decrypted text: %s", text);
    
    return 0;
}
```

## Example Output

```
Caesar Cipher Program
====================
Enter the text to encrypt: Hello World!
Enter the shift value (0-25): 3

Original text: Hello World!

Encrypted text: Khoor Zruog!

Decrypted text: Hello World!
```

## How it works:

1. **Encryption**: Each letter is shifted forward by the specified number of positions in the alphabet
2. **Decryption**: Each letter is shifted backward by the specified number of positions
3. **Wrap-around**: Uses modulo arithmetic to wrap around from 'Z' to 'A' and 'z' to 'a'
4. **Preserves case**: Maintains uppercase and lowercase letters
5. **Non-alphabetic characters**: Spaces, punctuation, and numbers remain unchanged

## Key Features:

- Handles both uppercase and lowercase letters
- Properly wraps around the alphabet
- Preserves non-alphabetic characters
- Includes both encryption and decryption functions
- Input validation for shift values

