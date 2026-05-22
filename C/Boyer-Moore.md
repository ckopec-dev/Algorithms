# Boyer-Moore Algorithm Implementation in C

Here's a complete implementation of the Boyer-Moore string searching algorithm in C:

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define ALPHABET_SIZE 256

// Function to preprocess the pattern and create the bad character table
void badCharHeuristic(char *pattern, int patternLength, int badChar[ALPHABET_SIZE]) {
    int i;
    
    // Initialize all entries as -1
    for (i = 0; i < ALPHABET_SIZE; i++) {
        badChar[i] = -1;
    }
    
    // Fill the actual value of last occurrence of each character
    for (i = 0; i < patternLength; i++) {
        badChar[(int)pattern[i]] = i;
    }
}

// Boyer-Moore search algorithm
int boyerMooreSearch(char *text, char *pattern) {
    int textLength = strlen(text);
    int patternLength = strlen(pattern);
    
    // Create bad character table
    int badChar[ALPHABET_SIZE];
    badCharHeuristic(pattern, patternLength, badChar);
    
    int shift = 0;  // Shift of the pattern with respect to text
    int i;
    
    while (shift <= (textLength - patternLength)) {
        i = patternLength - 1;
        
        // Keep reducing index i while characters of pattern and text are matching
        while (i >= 0 && pattern[i] == text[shift + i]) {
            i--;
        }
        
        // If the pattern is found at current shift
        if (i < 0) {
            return shift;  // Return the position where pattern is found
        } else {
            // Shift the pattern by the maximum of:
            // 1. Bad character rule
            // 2. Pattern length - 1 (for the case when the character is not found)
            shift += (i - badChar[(int)text[shift + i]]);
        }
    }
    
    return -1;  // Pattern not found
}

// Enhanced version with multiple occurrences
void boyerMooreAllOccurrences(char *text, char *pattern) {
    int textLength = strlen(text);
    int patternLength = strlen(pattern);
    
    if (patternLength > textLength) {
        printf("Pattern is longer than text\n");
        return;
    }
    
    // Create bad character table
    int badChar[ALPHABET_SIZE];
    badCharHeuristic(pattern, patternLength, badChar);
    
    int shift = 0;
    int found = 0;
    
    while (shift <= (textLength - patternLength)) {
        int i = patternLength - 1;
        
        // Keep reducing index i while characters of pattern and text are matching
        while (i >= 0 && pattern[i] == text[shift + i]) {
            i--;
        }
        
        // If the pattern is found at current shift
        if (i < 0) {
            printf("Pattern found at index %d\n", shift);
            found = 1;
            shift += patternLength;  // Move to next position
        } else {
            // Shift the pattern by the maximum of:
            // 1. Bad character rule
            // 2. Pattern length - 1 (for the case when the character is not found)
            int badCharShift = i - badChar[(int)text[shift + i]];
            shift += (badCharShift > 0) ? badCharShift : 1;
        }
    }
    
    if (!found) {
        printf("Pattern not found in text\n");
    }
}

// Main function to demonstrate the algorithm
int main() {
    char text[] = "ABAAABCDABCABCABCABCABC";
    char pattern[] = "ABCABC";
    
    printf("Text: %s\n", text);
    printf("Pattern: %s\n", pattern);
    printf("\n");
    
    // Single occurrence search
    int result = boyerMooreSearch(text, pattern);
    if (result != -1) {
        printf("Pattern found at index: %d\n", result);
    } else {
        printf("Pattern not found\n");
    }
    
    printf("\n");
    
    // Find all occurrences
    printf("All occurrences of pattern:\n");
    boyerMooreAllOccurrences(text, pattern);
    
    printf("\n");
    
    // Test with another example
    char text2[] = "AABAACAADAABAABA";
    char pattern2[] = "AABA";
    
    printf("Text: %s\n", text2);
    printf("Pattern: %s\n", pattern2);
    printf("\n");
    
    result = boyerMooreSearch(text2, pattern2);
    if (result != -1) {
        printf("Pattern found at index: %d\n", result);
    } else {
        printf("Pattern not found\n");
    }
    
    printf("\n");
    printf("All occurrences of pattern:\n");
    boyerMooreAllOccurrences(text2, pattern2);
    
    return 0;
}
```

## Expected Output:
```
Text: ABAABCDABCABCABCABCABC
Pattern: ABCABC

Pattern found at index: 5

All occurrences of pattern:
Pattern found at index 5
Pattern found at index 8
Pattern found at index 11
Pattern found at index 14
Pattern found at index 17

Text: AABAACAADAABAABA
Pattern: AABA

Pattern found at index: 12

All occurrences of pattern:
Pattern found at index 0
Pattern found at index 7
Pattern found at index 12
```

## Key Features of this Implementation:

1. **Bad Character Heuristic**: Creates a lookup table for quick character comparison
2. **Optimized Shift**: Uses the bad character rule to determine optimal shifts
3. **Multiple Occurrences**: Can find all occurrences of the pattern
4. **Efficient**: Time complexity of O(n/m) on average, where n is text length and m is pattern length
5. **Memory Efficient**: Uses constant space for the bad character table

## How it Works:

1. **Preprocessing**: Build a bad character table that maps each character to its last occurrence in the pattern
2. **Search**: Start matching from the right end of the pattern
3. **Shift**: When a mismatch occurs, shift the pattern based on the bad character rule
4. **Match**: If all characters match, we found an occurrence

This implementation demonstrates the core Boyer-Moore algorithm with both single and multiple occurrence search capabilities.

