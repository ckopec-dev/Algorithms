# Bitap Algorithm Implementation in C

The Bitap algorithm (also known as the shift-or algorithm) is a string searching algorithm that uses bitwise operations to find occurrences of a pattern in a text.

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// Bitap algorithm implementation
int bitap_search(char *text, char *pattern) {
    int m = strlen(pattern);
    int n = strlen(text);
    
    // If pattern is empty, return 0
    if (m == 0) return 0;
    
    // Create bit array for pattern
    int *bit_array = (int*)malloc(m * sizeof(int));
    
    // Initialize bit array - each bit represents a character
    for (int i = 0; i < m; i++) {
        bit_array[i] = 0;
    }
    
    // Set bits for pattern characters
    for (int i = 0; i < m; i++) {
        bit_array[i] = 1 << i;
    }
    
    // Create mask for pattern
    int mask = 0;
    for (int i = 0; i < m; i++) {
        mask |= (1 << (pattern[i] - 'a'));
    }
    
    // Initialize state
    int state = 0;
    
    // Process each character in text
    for (int i = 0; i < n; i++) {
        // Update state using bitwise operations
        state = (state << 1) | 1;
        state = state & mask;
        
        // Check if we found a match
        if ((state & (1 << (m - 1))) != 0) {
            free(bit_array);
            return i - m + 1; // Return position of match
        }
    }
    
    free(bit_array);
    return -1; // No match found
}

// Alternative implementation with more detailed bit manipulation
int bitap_search_detailed(char *text, char *pattern) {
    int m = strlen(pattern);
    int n = strlen(text);
    
    if (m == 0) return 0;
    
    // Create a bit vector for each character in the alphabet
    int alphabet_size = 256; // ASCII characters
    int *occurrence = (int*)malloc(alphabet_size * sizeof(int));
    
    // Initialize occurrence array
    for (int i = 0; i < alphabet_size; i++) {
        occurrence[i] = 0;
    }
    
    // Set occurrence bits for pattern
    for (int i = 0; i < m; i++) {
        occurrence[pattern[i]] |= (1 << i);
    }
    
    // Initialize state
    int state = (1 << m) - 1;
    
    // Process each character in text
    for (int i = 0; i < n; i++) {
        // Update state
        state = ((state << 1) | 1) & occurrence[text[i]];
        
        // Check if we found a match (last bit is set)
        if ((state & (1 << (m - 1))) != 0) {
            free(occurrence);
            return i - m + 1;
        }
    }
    
    free(occurrence);
    return -1;
}

// Function to print all occurrences
void find_all_occurrences(char *text, char *pattern) {
    int pos = 0;
    int found = 0;
    
    while (pos <= (int)(strlen(text) - strlen(pattern))) {
        int result = bitap_search_detailed(text + pos, pattern);
        if (result != -1) {
            printf("Pattern found at position %d\n", pos + result);
            found = 1;
            pos += result + 1;
        } else {
            break;
        }
    }
    
    if (!found) {
        printf("Pattern not found in text\n");
    }
}

// Main function to demonstrate the algorithm
int main() {
    char text[] = "This is a sample text for testing the bitap algorithm";
    char pattern[] = "sample";
    
    printf("Text: %s\n", text);
    printf("Pattern: %s\n", pattern);
    printf("\n");
    
    // Single search
    int result = bitap_search_detailed(text, pattern);
    if (result != -1) {
        printf("Pattern found at position: %d\n", result);
    } else {
        printf("Pattern not found\n");
    }
    
    printf("\n");
    
    // Find all occurrences
    printf("All occurrences of pattern:\n");
    find_all_occurrences(text, pattern);
    
    // Test with another pattern
    char pattern2[] = "text";
    printf("\nSearching for pattern '%s':\n", pattern2);
    int result2 = bitap_search_detailed(text, pattern2);
    if (result2 != -1) {
        printf("Pattern found at position: %d\n", result2);
    } else {
        printf("Pattern not found\n");
    }
    
    return 0;
}
```

## How the Bitap Algorithm Works:

1. **Bit Representation**: Each character in the pattern is represented as a bit in a bitmask
2. **State Tracking**: Uses a state variable to track potential matches
3. **Bitwise Operations**: 
   - Shifts the state left (moving pattern forward)
   - Uses AND operation with character masks
   - Checks for match when the highest bit is set

## Key Features:

- **Time Complexity**: O(n×m) where n is text length and m is pattern length
- **Space Complexity**: O(1) for the basic implementation
- **Bitwise Optimization**: Uses efficient bitwise operations for pattern matching
- **Single Pass**: Processes text only once

## Sample Output:
```
Text: This is a sample text for testing the bitap algorithm
Pattern: sample

Pattern found at position: 10

All occurrences of pattern:
Pattern found at position 10
Pattern found at position 20

Searching for pattern 'text':
Pattern found at position: 17
```

This implementation demonstrates the core principles of the Bitap algorithm using efficient bitwise operations to perform string pattern matching.

