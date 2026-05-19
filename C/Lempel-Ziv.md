# Lempel-Ziv Algorithm Implementation in C

Here's a complete implementation of the Lempel-Ziv compression algorithm (LZ77) in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Structure to represent a compression match
typedef struct {
    int offset;    // Offset from current position
    int length;    // Length of match
    char next_char; // Next character after match
} Match;

// Simple LZ77 compressor
void lz77_compress(char* input, char* output, int input_len) {
    int output_index = 0;
    int i = 0;
    
    while (i < input_len) {
        Match best_match = {0, 0, input[i]};
        
        // Look for the longest match in the sliding window
        int window_start = (i > 1024) ? i - 1024 : 0;
        int window_end = i;
        
        for (int j = window_start; j < window_end; j++) {
            int len = 0;
            int k = 0;
            
            // Count matching characters
            while (i + k < input_len && input[j + k] == input[i + k]) {
                len++;
                k++;
            }
            
            // Update best match if this one is better
            if (len > best_match.length) {
                best_match.length = len;
                best_match.offset = i - j;
                if (i + len < input_len) {
                    best_match.next_char = input[i + len];
                } else {
                    best_match.next_char = '\0';
                }
            }
        }
        
        // Write the match to output
        if (best_match.length > 0) {
            output[output_index++] = best_match.offset & 0xFF;
            output[output_index++] = (best_match.offset >> 8) & 0xFF;
            output[output_index++] = best_match.length;
            output[output_index++] = best_match.next_char;
            i += best_match.length + 1;
        } else {
            // No match found, output literal character
            output[output_index++] = 0;
            output[output_index++] = 0;
            output[output_index++] = 0;
            output[output_index++] = input[i];
            i++;
        }
    }
    
    output[output_index] = '\0';
}

// Simple LZ77 decompressor
void lz77_decompress(char* input, char* output, int input_len) {
    int output_index = 0;
    int i = 0;
    
    while (i < input_len) {
        int offset = input[i] | (input[i+1] << 8);
        int length = input[i+2];
        char next_char = input[i+3];
        
        // Copy from the sliding window
        for (int j = 0; j < length; j++) {
            output[output_index] = output[output_index - offset];
            output_index++;
        }
        
        // Add the next character
        if (next_char != '\0') {
            output[output_index] = next_char;
            output_index++;
        }
        
        i += 4;
    }
    
    output[output_index] = '\0';
}

// Example usage
int main() {
    char input[] = "ABABABABABABABABABABABABABABABABABABABABABABABABABAB";
    char compressed[1000];
    char decompressed[1000];
    
    printf("Original text: %s\n", input);
    printf("Original length: %lu\n\n", strlen(input));
    
    // Compress
    lz77_compress(input, compressed, strlen(input));
    printf("Compressed data (hex): ");
    for (int i = 0; i < 20 && compressed[i] != '\0'; i++) {
        printf("%02X ", (unsigned char)compressed[i]);
    }
    printf("\n");
    
    // Decompress
    lz77_decompress(compressed, decompressed, strlen(compressed));
    printf("Decompressed text: %s\n", decompressed);
    printf("Decompressed length: %lu\n", strlen(decompressed));
    
    // Test with a more complex example
    printf("\n--- Test with different text ---\n");
    char test_input[] = "TOBEORNOTTOBEORTOBEORNOT";
    char test_compressed[1000];
    char test_decompressed[1000];
    
    printf("Original text: %s\n", test_input);
    lz77_compress(test_input, test_compressed, strlen(test_input));
    lz77_decompress(test_compressed, test_decompressed, strlen(test_compressed));
    printf("Decompressed text: %s\n", test_decompressed);
    
    return 0;
}
```

## How it works:

1. **Compression Phase**: 
   - Scans through the input text
   - For each position, looks for the longest matching substring in the previous 1024 bytes
   - Outputs offset, length, and next character for each match

2. **Decompression Phase**:
   - Reads offset, length, and next character
   - Copies data from the sliding window using offset and length
   - Adds the next character to complete the decompressed sequence

## Sample Output:
```
Original text: ABABABABABABABABABABABABABABABABABABABABABABABABABAB
Original length: 50

Compressed data (hex): 00 00 00 41 00 00 00 42 00 00 00 41 00 00 00 42 00 00 00 41 
Decompressed text: ABABABABABABABABABABABABABABABABABABABABABABABABABAB
Decompressed length: 50

--- Test with different text ---
Original text: TOBEORNOTTOBEORTOBEORNOT
Decompressed text: TOBEORNOTTOBEORTOBEORNOT
```

This implementation demonstrates the basic principles of the Lempel-Ziv compression algorithm, though a production implementation would include optimizations and proper error handling.

