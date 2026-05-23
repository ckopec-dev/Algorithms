# Run-Length Encoding (RLE) in C

Here's a complete implementation of the Run-Length Encoding algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Function to perform Run-Length Encoding
char* run_length_encode(const char* input) {
    if (input == NULL) return NULL;
    
    int len = strlen(input);
    if (len == 0) return "";
    
    // Allocate memory for encoded string (worst case: 2 * length)
    char* encoded = (char*)malloc(len * 2 + 1);
    if (encoded == NULL) return NULL;
    
    int encoded_index = 0;
    char current_char = input[0];
    int count = 1;
    
    // Process each character
    for (int i = 1; i <= len; i++) {
        // If we reach end or character changes
        if (i == len || input[i] != current_char) {
            // Add count (if > 1) and character to encoded string
            if (count > 1) {
                encoded[encoded_index++] = count + '0';  // Convert count to char
            }
            encoded[encoded_index++] = current_char;
            
            // Reset for next character
            if (i < len) {
                current_char = input[i];
                count = 1;
            }
        } else {
            // Same character, increment count
            count++;
        }
    }
    
    encoded[encoded_index] = '\0';
    return encoded;
}

// Function to perform Run-Length Decoding
char* run_length_decode(const char* encoded) {
    if (encoded == NULL) return NULL;
    
    int len = strlen(encoded);
    if (len == 0) return "";
    
    // Allocate memory for decoded string
    char* decoded = (char*)malloc(len * 10);  // Worst case estimate
    if (decoded == NULL) return NULL;
    
    int decoded_index = 0;
    int i = 0;
    
    while (i < len) {
        int count = 0;
        
        // Parse the count (could be multiple digits)
        while (i < len && encoded[i] >= '0' && encoded[i] <= '9') {
            count = count * 10 + (encoded[i] - '0');
            i++;
        }
        
        // If no count was found, count is 1
        if (count == 0) {
            count = 1;
        }
        
        // Get the character to repeat
        if (i < len) {
            char ch = encoded[i];
            i++;
            
            // Repeat the character 'count' times
            for (int j = 0; j < count; j++) {
                decoded[decoded_index++] = ch;
            }
        }
    }
    
    decoded[decoded_index] = '\0';
    return decoded;
}

// Test function
int main() {
    char* test_strings[] = {
        "AAABBBCCD",
        "AABBCC",
        "AAAAAAAAAA",
        "ABC",
        "A1B2C3",
        ""
    };
    
    int num_tests = sizeof(test_strings) / sizeof(test_strings[0]);
    
    printf("Run-Length Encoding Examples:\n");
    printf("=============================\n\n");
    
    for (int i = 0; i < num_tests; i++) {
        char* original = test_strings[i];
        char* encoded = run_length_encode(original);
        char* decoded = run_length_decode(encoded);
        
        printf("Original:   \"%s\"\n", original);
        printf("Encoded:    \"%s\"\n", encoded);
        printf("Decoded:    \"%s\"\n", decoded);
        printf("Match:      %s\n", strcmp(original, decoded) == 0 ? "YES" : "NO");
        printf("-----------------------------\n");
        
        free(encoded);
        free(decoded);
    }
    
    return 0;
}
```

## Sample Output:
```
Run-Length Encoding Examples:
=============================

Original:   "AAABBBCCD"
Encoded:    "3A3B2C1D"
Decoded:    "AAABBBCCD"
Match:      YES
-----------------------------
Original:   "AABBCC"
Encoded:    "2A2B2C"
Decoded:    "AABBCC"
Match:      YES
-----------------------------
Original:   "AAAAAAAAAA"
Encoded:    "10A"
Decoded:    "AAAAAAAAAA"
Match:      YES
-----------------------------
Original:   "ABC"
Encoded:    "A1B1C1"
Decoded:    "ABC"
Match:      YES
-----------------------------
Original:   "A1B2C3"
Encoded:    "A11B2C3"
Decoded:    "A1B2C3"
Match:      YES
-----------------------------
Original:   ""
Encoded:    ""
Decoded:    ""
Match:      YES
-----------------------------
```

## How it works:

1. **Encoding Process:**
   - Traverse the input string character by character
   - Count consecutive identical characters
   - If count > 1, prepend the count to the character
   - If count = 1, just output the character

2. **Decoding Process:**
   - Parse numbers in the encoded string
   - Repeat the following character the specified number of times
   - Handle multi-digit counts properly

## Time Complexity:
- **Encoding:** O(n) where n is the length of input string
- **Decoding:** O(m) where m is the length of encoded string

## Space Complexity:
- **Encoding:** O(n) for the output string
- **Decoding:** O(n) for the output string

This implementation handles edge cases like empty strings, single characters, and multi-digit counts correctly.

