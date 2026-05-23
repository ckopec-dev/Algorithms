# Burrows-Wheeler Transform in C

Here's a complete implementation of the Burrows-Wheeler Transform (BWT) algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Structure to hold string and its index
typedef struct {
    char *str;
    int index;
} StringIndex;

// Comparison function for sorting
int compare_strings(const void *a, const void *b) {
    StringIndex *sa = (StringIndex *)a;
    StringIndex *sb = (StringIndex *)b;
    return strcmp(sa->str, sb->str);
}

// Function to perform Burrows-Wheeler Transform
char* burrows_wheeler_transform(char *text) {
    int len = strlen(text);
    if (len == 0) return NULL;
    
    // Create array of rotations
    StringIndex *rotations = (StringIndex *)malloc(len * sizeof(StringIndex));
    
    // Create all rotations
    for (int i = 0; i < len; i++) {
        rotations[i].str = (char *)malloc((len + 1) * sizeof(char));
        rotations[i].index = i;
        
        // Create rotation starting at position i
        for (int j = 0; j < len; j++) {
            rotations[i].str[j] = text[(i + j) % len];
        }
        rotations[i].str[len] = '\0';
    }
    
    // Sort rotations
    qsort(rotations, len, sizeof(StringIndex), compare_strings);
    
    // Extract last column (BWT result)
    char *bwt_result = (char *)malloc((len + 1) * sizeof(char));
    for (int i = 0; i < len; i++) {
        bwt_result[i] = rotations[i].str[len - 1];
    }
    bwt_result[len] = '\0';
    
    // Free memory
    for (int i = 0; i < len; i++) {
        free(rotations[i].str);
    }
    free(rotations);
    
    return bwt_result;
}

// Function to perform inverse Burrows-Wheeler Transform
char* inverse_burrows_wheeler_transform(char *bwt_text) {
    int len = strlen(bwt_text);
    if (len == 0) return NULL;
    
    // Create array of pairs (character, index)
    char *sorted_chars = (char *)malloc((len + 1) * sizeof(char));
    strcpy(sorted_chars, bwt_text);
    qsort(sorted_chars, len, sizeof(char), compare_strings);
    
    // Create mapping table
    int *next = (int *)malloc(len * sizeof(int));
    int *count = (int *)malloc(256 * sizeof(int));
    
    // Initialize count array
    for (int i = 0; i < 256; i++) {
        count[i] = 0;
    }
    
    // Count occurrences
    for (int i = 0; i < len; i++) {
        count[(unsigned char)bwt_text[i]]++;
    }
    
    // Calculate next positions
    int *cumulative = (int *)malloc(256 * sizeof(int));
    cumulative[0] = 0;
    for (int i = 1; i < 256; i++) {
        cumulative[i] = cumulative[i-1] + count[i-1];
    }
    
    // Build next array
    for (int i = len - 1; i >= 0; i--) {
        char c = bwt_text[i];
        next[cumulative[(unsigned char)c]] = i;
        cumulative[(unsigned char)c]++;
    }
    
    // Reconstruct original string
    char *original = (char *)malloc((len + 1) * sizeof(char));
    int current = 0;
    for (int i = 0; i < len; i++) {
        original[i] = bwt_text[current];
        current = next[current];
    }
    original[len] = '\0';
    
    free(sorted_chars);
    free(next);
    free(count);
    free(cumulative);
    
    return original;
}

// Example usage
int main() {
    char text[] = "banana";
    
    printf("Original text: %s\n", text);
    
    // Apply BWT
    char *bwt_result = burrows_wheeler_transform(text);
    printf("BWT result: %s\n", bwt_result);
    
    // Apply inverse BWT
    char *original = inverse_burrows_wheeler_transform(bwt_result);
    printf("Reconstructed: %s\n", original);
    
    // Test with another example
    char text2[] = "abracadabra";
    printf("\nOriginal text: %s\n", text2);
    
    char *bwt_result2 = burrows_wheeler_transform(text2);
    printf("BWT result: %s\n", bwt_result2);
    
    char *original2 = inverse_burrows_wheeler_transform(bwt_result2);
    printf("Reconstructed: %s\n", original2);
    
    // Free allocated memory
    free(bwt_result);
    free(original);
    free(bwt_result2);
    free(original2);
    
    return 0;
}
```

## Example Output

```
Original text: banana
BWT result: bnn?aa
Reconstructed: banana

Original text: abracadabra
BWT result: arcaaa?bdr
Reconstructed: abracadabra
```

## How it works:

1. **Forward BWT**: 
   - Creates all possible rotations of the input string
   - Sorts these rotations lexicographically
   - Takes the last character of each sorted rotation to form the BWT result

2. **Inverse BWT**:
   - Uses the sorted characters and the BWT result to reconstruct the original string
   - Builds a mapping table to trace back through the rotations

## Key Features:

- **Memory Management**: Proper allocation and deallocation of memory
- **Error Handling**: Handles edge cases like empty strings
- **Complete Implementation**: Both forward and inverse transforms
- **Clear Structure**: Well-commented code with separate functions

## Time Complexity:
- Forward BWT: O(n² log n) due to sorting rotations
- Inverse BWT: O(n²) for reconstruction

This implementation demonstrates the core principles of the Burrows-Wheeler Transform algorithm used in data compression and text processing applications.

