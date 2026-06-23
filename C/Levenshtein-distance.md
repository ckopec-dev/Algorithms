# Levenshtein Distance Algorithm in C

Here's a complete implementation of the Levenshtein distance algorithm in C:

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// Function to find minimum of three numbers
int min3(int a, int b, int c) {
    int min = a;
    if (b < min) min = b;
    if (c < min) min = c;
    return min;
}

// Function to calculate Levenshtein distance
int levenshtein_distance(char *str1, char *str2) {
    int len1 = strlen(str1);
    int len2 = strlen(str2);
    
    // Create a 2D array to store distances
    int **dp = (int**)malloc((len1 + 1) * sizeof(int*));
    for (int i = 0; i <= len1; i++) {
        dp[i] = (int*)malloc((len2 + 1) * sizeof(int));
    }
    
    // Initialize base cases
    for (int i = 0; i <= len1; i++) {
        dp[i][0] = i;
    }
    
    for (int j = 0; j <= len2; j++) {
        dp[0][j] = j;
    }
    
    // Fill the distance matrix
    for (int i = 1; i <= len1; i++) {
        for (int j = 1; j <= len2; j++) {
            if (str1[i-1] == str2[j-1]) {
                dp[i][j] = dp[i-1][j-1];  // No operation needed
            } else {
                dp[i][j] = 1 + min3(
                    dp[i-1][j],     // deletion
                    dp[i][j-1],     // insertion
                    dp[i-1][j-1]    // substitution
                );
            }
        }
    }
    
    int result = dp[len1][len2];
    
    // Free allocated memory
    for (int i = 0; i <= len1; i++) {
        free(dp[i]);
    }
    free(dp);
    
    return result;
}

// Alternative implementation using O(min(m,n)) space
int levenshtein_distance_optimized(char *str1, char *str2) {
    int len1 = strlen(str1);
    int len2 = strlen(str2);
    
    // Make str1 the shorter string for optimization
    if (len1 > len2) {
        char *temp = str1;
        str1 = str2;
        str2 = temp;
        int temp_len = len1;
        len1 = len2;
        len2 = temp_len;
    }
    
    // Use only two rows instead of full matrix
    int *prev = (int*)malloc((len1 + 1) * sizeof(int));
    int *curr = (int*)malloc((len1 + 1) * sizeof(int));
    
    // Initialize previous row
    for (int i = 0; i <= len1; i++) {
        prev[i] = i;
    }
    
    // Fill the matrix row by row
    for (int j = 1; j <= len2; j++) {
        curr[0] = j;
        
        for (int i = 1; i <= len1; i++) {
            if (str1[i-1] == str2[j-1]) {
                curr[i] = prev[i-1];
            } else {
                curr[i] = 1 + min3(
                    prev[i],      // deletion
                    curr[i-1],    // insertion
                    prev[i-1]     // substitution
                );
            }
        }
        
        // Swap arrays
        int *temp = prev;
        prev = curr;
        curr = temp;
    }
    
    int result = prev[len1];
    
    free(prev);
    free(curr);
    
    return result;
}

int main() {
    char str1[100], str2[100];
    
    printf("Levenshtein Distance Calculator\n");
    printf("==============================\n");
    
    // Example 1
    strcpy(str1, "kitten");
    strcpy(str2, "sitting");
    printf("String 1: %s\n", str1);
    printf("String 2: %s\n", str2);
    printf("Levenshtein distance: %d\n\n", levenshtein_distance(str1, str2));
    
    // Example 2
    strcpy(str1, "saturday");
    strcpy(str2, "sunday");
    printf("String 1: %s\n", str1);
    printf("String 2: %s\n", str2);
    printf("Levenshtein distance: %d\n\n", levenshtein_distance(str1, str2));
    
    // Example 3
    strcpy(str1, "hello");
    strcpy(str2, "world");
    printf("String 1: %s\n", str1);
    printf("String 2: %s\n", str2);
    printf("Levenshtein distance: %d\n\n", levenshtein_distance(str1, str2));
    
    // Example 4 - Optimized version
    strcpy(str1, "algorithm");
    strcpy(str2, "logarithm");
    printf("String 1: %s\n", str1);
    printf("String 2: %s\n", str2);
    printf("Levenshtein distance (optimized): %d\n", levenshtein_distance_optimized(str1, str2));
    
    return 0;
}
```

## Output:
```
Levenshtein Distance Calculator
==============================
String 1: kitten
String 2: sitting
Levenshtein distance: 3

String 1: saturday
String 2: sunday
Levenshtein distance: 3

String 1: hello
String 2: world
Levenshtein distance: 4

String 1: algorithm
String 2: logarithm
Levenshtein distance (optimized): 3
```

## Algorithm Explanation:

The Levenshtein distance calculates the minimum number of single-character edits (insertions, deletions, or substitutions) required to change one string into another.

### How it works:
1. **Dynamic Programming Approach**: Creates a matrix where `dp[i][j]` represents the edit distance between the first `i` characters of string1 and the first `j` characters of string2
2. **Base Cases**: 
   - `dp[i][0] = i` (deleting i characters from string1)
   - `dp[0][j] = j` (inserting j characters to get string2)
3. **Recurrence Relation**:
   - If characters match: `dp[i][j] = dp[i-1][j-1]`
   - If characters don't match: `dp[i][j] = 1 + min(dp[i-1][j], dp[i][j-1], dp[i-1][j-1])`

### Time and Space Complexity:
- **Time Complexity**: O(m × n) where m and n are the lengths of the strings
- **Space Complexity**: O(m × n) for basic version, O(min(m,n)) for optimized version

The optimized version uses only two rows instead of a full matrix to reduce space complexity.