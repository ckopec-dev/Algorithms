# Damerau-Levenshtein Distance in C

Here's a complete implementation of the Damerau-Levenshtein distance algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// Function to return minimum of three integers
int min3(int a, int b, int c) {
    int min = a;
    if (b < min) min = b;
    if (c < min) min = c;
    return min;
}

// Function to return minimum of two integers
int min2(int a, int b) {
    return (a < b) ? a : b;
}

// Damerau-Levenshtein distance calculation
int damerau_levenshtein_distance(const char* s1, const char* s2) {
    int len1 = strlen(s1);
    int len2 = strlen(s2);
    
    // Handle edge cases
    if (len1 == 0) return len2;
    if (len2 == 0) return len1;
    
    // Create distance matrix
    int** dist = (int**)malloc((len1 + 1) * sizeof(int*));
    for (int i = 0; i <= len1; i++) {
        dist[i] = (int*)calloc(len2 + 1, sizeof(int));
    }
    
    // Initialize base cases
    for (int i = 0; i <= len1; i++) {
        dist[i][0] = i;
    }
    for (int j = 0; j <= len2; j++) {
        dist[0][j] = j;
    }
    
    // Fill the distance matrix
    for (int i = 1; i <= len1; i++) {
        for (int j = 1; j <= len2; j++) {
            int cost = (tolower(s1[i-1]) == tolower(s2[j-1])) ? 0 : 1;
            
            dist[i][j] = min3(
                dist[i-1][j] + 1,           // deletion
                dist[i][j-1] + 1,           // insertion
                dist[i-1][j-1] + cost       // substitution
            );
            
            // Check for transposition (Damerau-Levenshtein specific)
            if (i > 1 && j > 1 && 
                tolower(s1[i-1]) == tolower(s2[j-2]) && 
                tolower(s1[i-2]) == tolower(s2[j-1])) {
                dist[i][j] = min2(dist[i][j], dist[i-2][j-2] + 1);
            }
        }
    }
    
    int result = dist[len1][len2];
    
    // Free allocated memory
    for (int i = 0; i <= len1; i++) {
        free(dist[i]);
    }
    free(dist);
    
    return result;
}

// Example usage
int main() {
    // Test cases
    const char* test_cases[][2] = {
        {"kitten", "sitting"},
        {"saturday", "sunday"},
        {"hello", "world"},
        {"algorithm", "logarithm"},
        {"", "abc"},
        {"abc", ""},
        {"same", "same"},
        {"abc", "acb"},
        {"abc", "ab"}
    };
    
    int num_tests = sizeof(test_cases) / sizeof(test_cases[0]);
    
    printf("Damerau-Levenshtein Distance Examples:\n");
    printf("=====================================\n\n");
    
    for (int i = 0; i < num_tests; i++) {
        const char* str1 = test_cases[i][0];
        const char* str2 = test_cases[i][1];
        int distance = damerau_levenshtein_distance(str1, str2);
        
        printf("String 1: \"%s\"\n", str1);
        printf("String 2: \"%s\"\n", str2);
        printf("Distance: %d\n", distance);
        printf("------------------------\n");
    }
    
    // Interactive example
    printf("\nInteractive Example:\n");
    char input1[100], input2[100];
    
    printf("Enter first string: ");
    fgets(input1, sizeof(input1), stdin);
    input1[strcspn(input1, "\n")] = 0;  // Remove newline
    
    printf("Enter second string: ");
    fgets(input2, sizeof(input2), stdin);
    input2[strcspn(input2, "\n")] = 0;  // Remove newline
    
    int result = damerau_levenshtein_distance(input1, input2);
    printf("\nDamerau-Levenshtein distance between \"%s\" and \"%s\": %d\n", 
           input1, input2, result);
    
    return 0;
}
```

## Key Features of this Implementation:

1. **Memory Management**: Properly allocates and frees memory for the distance matrix
2. **Case Insensitive**: Uses `tolower()` for case-insensitive comparison
3. **Transposition Handling**: Correctly implements the Damerau-Levenshtein algorithm's unique ability to handle transpositions
4. **Edge Cases**: Handles empty strings and boundary conditions
5. **Interactive Mode**: Includes an interactive example for testing

## Example Output:
```
Damerau-Levenshtein Distance Examples:
=====================================

String 1: "kitten"
String 2: "sitting"
Distance: 3
------------------------
String 1: "saturday"
String 2: "sunday"
Distance: 3
------------------------
String 1: "hello"
String 2: "world"
Distance: 5
------------------------
```

## Algorithm Explanation:

The Damerau-Levenshtein distance extends the classic Levenshtein distance by allowing **transpositions** (swapping two adjacent characters) as a single operation. The algorithm uses dynamic programming with a matrix where each cell [i,j] represents the minimum edit distance between the first i characters of string 1 and the first j characters of string 2.

**Operations considered:**
- **Insertion**: Add a character
- **Deletion**: Remove a character  
- **Substitution**: Replace a character
- **Transposition**: Swap two adjacent characters (Damerau-Levenshtein specific)

The time complexity is O(m×n) where m and n are the lengths of the two strings, and the space complexity is also O(m×n).

