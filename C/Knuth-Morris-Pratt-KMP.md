# Knuth-Morris-Pratt (KMP) Algorithm Implementation in C

## Overview
The KMP algorithm is a string searching algorithm that efficiently finds occurrences of a pattern within a text by using previously matched characters to avoid unnecessary comparisons.

## C Implementation

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// Function to compute the LPS (Longest Proper Prefix which is also Suffix) array
void computeLPSArray(char *pattern, int patternLength, int *lps) {
    int len = 0;  // length of previous longest prefix suffix
    int i = 1;
    
    lps[0] = 0;  // First element is always 0
    
    // Loop through the pattern to fill the LPS array
    while (i < patternLength) {
        if (pattern[i] == pattern[len]) {
            len++;
            lps[i] = len;
            i++;
        } else {
            if (len != 0) {
                // This is tricky - consider the previous lps value
                len = lps[len - 1];
            } else {
                // If len is 0, set lps[i] to 0 and move to next character
                lps[i] = 0;
                i++;
            }
        }
    }
}

// KMP search function
void KMPSearch(char *pattern, char *text) {
    int patternLength = strlen(pattern);
    int textLength = strlen(text);
    
    // Create LPS array for pattern
    int *lps = (int*)malloc(patternLength * sizeof(int));
    
    // Preprocess the pattern (compute LPS array)
    computeLPSArray(pattern, patternLength, lps);
    
    int i = 0;  // index for text
    int j = 0;  // index for pattern
    
    printf("Pattern found at positions: ");
    
    while (i < textLength) {
        if (pattern[j] == text[i]) {
            i++;
            j++;
        }
        
        if (j == patternLength) {
            // Pattern found at index i-j
            printf("%d ", i - j);
            j = lps[j - 1];  // Continue searching for more occurrences
        } else if (i < textLength && pattern[j] != text[i]) {
            // Mismatch after some matches
            if (j != 0) {
                j = lps[j - 1];  // Use previously computed LPS value
            } else {
                i++;  // Move to next character in text
            }
        }
    }
    
    printf("\n");
    
    free(lps);  // Free allocated memory
}

// Example usage
int main() {
    char text[] = "ABABDABACDABABCABCABCABCABC";
    char pattern[] = "ABABCABCABCABC";
    
    printf("Text: %s\n", text);
    printf("Pattern: %s\n", pattern);
    
    KMPSearch(pattern, text);
    
    // Another example
    printf("\n--- Another Example ---\n");
    char text2[] = "AABAACAADAABAABA";
    char pattern2[] = "AABA";
    
    printf("Text: %s\n", text2);
    printf("Pattern: %s\n", pattern2);
    
    KMPSearch(pattern2, text2);
    
    return 0;
}
```

## Output
```
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Pattern found at positions: 10 

--- Another Example ---
Text: AABAACAADAABAABA
Pattern: AABA
Pattern found at positions: 0 9 12 
```

## How it Works

### 1. LPS Array Construction
The LPS (Longest Proper Prefix which is also Suffix) array helps us determine how much to shift the pattern when a mismatch occurs. For each position in the pattern, it stores the length of the longest proper prefix that is also a suffix.

### 2. Search Process
- Compare characters of pattern and text
- When characters match, advance both pointers
- When characters don't match:
  - If we're not at the beginning of pattern, use LPS array to determine how much to shift
  - Otherwise, move to next character in text
- When complete pattern match is found, record position and continue searching

## Time Complexity
- **Preprocessing (LPS array)**: O(m) where m is pattern length
- **Searching**: O(n) where n is text length
- **Overall**: O(n + m)

## Space Complexity
- O(m) for the LPS array

This implementation efficiently handles overlapping patterns and avoids unnecessary character comparisons, making it more efficient than naive string matching algorithms.

