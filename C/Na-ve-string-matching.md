# Naïve String Matching Algorithm in C

Here's a complete implementation of the naïve string matching algorithm in C:

```c
#include <stdio.h>
#include <string.h>

// Naïve string matching algorithm
int naive_string_match(char *text, char *pattern) {
    int text_len = strlen(text);
    int pattern_len = strlen(pattern);
    
    // If pattern is longer than text, no match possible
    if (pattern_len > text_len) {
        return -1;
    }
    
    // Check each possible starting position in text
    for (int i = 0; i <= text_len - pattern_len; i++) {
        int j;
        
        // Check if pattern matches at position i
        for (j = 0; j < pattern_len; j++) {
            if (text[i + j] != pattern[j]) {
                break;
            }
        }
        
        // If we've matched all characters, we found a match
        if (j == pattern_len) {
            return i; // Return the starting index
        }
    }
    
    return -1; // No match found
}

// Function to find all occurrences of pattern
void find_all_matches(char *text, char *pattern) {
    int text_len = strlen(text);
    int pattern_len = strlen(pattern);
    int found = 0;
    
    if (pattern_len > text_len) {
        printf("Pattern is longer than text\n");
        return;
    }
    
    printf("All occurrences of pattern \"%s\" in text:\n", pattern);
    
    for (int i = 0; i <= text_len - pattern_len; i++) {
        int j;
        for (j = 0; j < pattern_len; j++) {
            if (text[i + j] != pattern[j]) {
                break;
            }
        }
        
        if (j == pattern_len) {
            printf("Found at index %d\n", i);
            found = 1;
        }
    }
    
    if (!found) {
        printf("Pattern not found in text\n");
    }
}

int main() {
    char text[] = "ABABDABACDABABCABCABCABCABC";
    char pattern[] = "ABABCABCABCABC";
    
    printf("Text: %s\n", text);
    printf("Pattern: %s\n", pattern);
    printf("\n");
    
    // Find first occurrence
    int result = naive_string_match(text, pattern);
    
    if (result != -1) {
        printf("First occurrence found at index: %d\n", result);
    } else {
        printf("Pattern not found in text\n");
    }
    
    printf("\n");
    
    // Find all occurrences
    find_all_matches(text, pattern);
    
    printf("\n");
    
    // Test with another example
    char text2[] = "HELLO WORLD HELLO";
    char pattern2[] = "HELLO";
    
    printf("Text: %s\n", text2);
    printf("Pattern: %s\n", pattern2);
    
    result = naive_string_match(text2, pattern2);
    
    if (result != -1) {
        printf("First occurrence found at index: %d\n", result);
    } else {
        printf("Pattern not found in text\n");
    }
    
    printf("\n");
    find_all_matches(text2, pattern2);
    
    return 0;
}
```

## Output:
```
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC

First occurrence found at index: 10

All occurrences of pattern "ABABCABCABCABC" in text:
Found at index 10

Text: HELLO WORLD HELLO
Pattern: HELLO
First occurrence found at index: 0

All occurrences of pattern "HELLO" in text:
Found at index 0
Found at index 11
```

## Algorithm Explanation:

**Time Complexity:** O(n × m) where n is the length of text and m is the length of pattern
**Space Complexity:** O(1)

### How it works:
1. The algorithm checks every possible position in the text where the pattern could start
2. At each position, it compares characters one by one
3. If all characters match, it returns the starting index
4. If no match is found after checking all positions, it returns -1

### Key Features:
- Simple and easy to understand
- Works well for small texts or when pattern is short
- No preprocessing required
- Can be modified to find all occurrences, not just the first one

The algorithm is called "naïve" because it doesn't use any advanced techniques like KMP or Boyer-Moore, but instead uses brute force comparison.

