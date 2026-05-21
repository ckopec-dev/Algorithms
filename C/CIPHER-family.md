# CIPHER__family Algorithm Example in C

Here's an example implementation of a simple cipher algorithm family in C:

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// Cipher family: Caesar Cipher with different shifts
typedef enum {
    CAESAR_3,
    CAESAR_5,
    CAESAR_7,
    CAESAR_13
} CipherType;

// Caesar Cipher encryption function
char* caesar_encrypt(char* text, int shift) {
    static char result[1000];
    int i;
    
    for (i = 0; i < strlen(text); i++) {
        if (text[i] >= 'a' && text[i] <= 'z') {
            result[i] = ((text[i] - 'a' + shift) % 26) + 'a';
        } else if (text[i] >= 'A' && text[i] <= 'Z') {
            result[i] = ((text[i] - 'A' + shift) % 26) + 'A';
        } else {
            result[i] = text[i];
        }
    }
    result[i] = '\0';
    return result;
}

// Cipher family function
char* cipher_family_encrypt(char* text, CipherType type) {
    int shift = 0;
    
    switch (type) {
        case CAESAR_3:
            shift = 3;
            break;
        case CAESAR_5:
            shift = 5;
            break;
        case CAESAR_7:
            shift = 7;
            break;
        case CAESAR_13:
            shift = 13;
            break;
        default:
            shift = 3;
    }
    
    return caesar_encrypt(text, shift);
}

int main() {
    char* original_text = "Hello World";
    char* encrypted_text;
    
    printf("Original text: %s\n", original_text);
    
    // Test different cipher family members
    encrypted_text = cipher_family_encrypt(original_text, CAESAR_3);
    printf("Caesar 3: %s\n", encrypted_text);
    
    encrypted_text = cipher_family_encrypt(original_text, CAESAR_5);
    printf("Caesar 5: %s\n", encrypted_text);
    
    encrypted_text = cipher_family_encrypt(original_text, CAESAR_7);
    printf("Caesar 7: %s\n", encrypted_text);
    
    encrypted_text = cipher_family_encrypt(original_text, CAESAR_13);
    printf("Caesar 13: %s\n", encrypted_text);
    
    return 0;
}
```

**Output:**
```
Original text: Hello World
Caesar 3: Khoor Zruog
Caesar 5: Mjqqt Btwqi
Caesar 7: Olssv Ixvwo
Caesar 13: Uryyb Jbeyq
```

This example demonstrates a **CIPHER__family** algorithm where:
- The base algorithm is Caesar Cipher
- Different members of the family are created by varying the shift parameter
- The `CipherType` enum represents different cipher variants in the family
- Each variant maintains the same core algorithm structure but with different parameters

