# Rabin-Karp Algorithm Implementation in C

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// d is the number of characters in the input alphabet
#define d 256

// Search for pattern in text using Rabin-Karp algorithm
void search(char* pattern, char* text, int q) {
    int M = strlen(pattern);
    int N = strlen(text);
    int i, j;
    int p = 0;  // hash value for pattern
    int t = 0;  // hash value for text
    int h = 1;

    // The value of h would be "pow(d, M-1) % q"
    for (i = 0; i < M - 1; i++)
        h = (h * d) % q;

    // Calculate the hash value of pattern and first window of text
    for (i = 0; i < M; i++) {
        p = (d * p + pattern[i]) % q;
        t = (d * t + text[i]) % q;
    }

    // Slide the pattern over text one by one
    for (i = 0; i <= N - M; i++) {
        // Check the hash values of current window of text and pattern
        if (p == t) {
            // Check characters one by one
            for (j = 0; j < M; j++) {
                if (text[i + j] != pattern[j])
                    break;
            }
            
            // If p == t and pattern[0...M-1] = text[i, i+1, ...i+M-1]
            if (j == M)
                printf("Pattern found at index %d\n", i);
        }

        // Calculate hash value for next window of text: Remove
        // leading digit, add trailing digit
        if (i < N - M) {
            t = (d * (t - text[i] * h) + text[i + M]) % q;
            
            // We might get negative value of t, converting it to positive
            if (t < 0)
                t = (t + q);
        }
    }
}

int main() {
    char* text = "AABAACAADAABAABA";
    char* pattern = "AABA";
    int q = 101;  // A prime number
    
    printf("Text: %s\n", text);
    printf("Pattern: %s\n", pattern);
    printf("Searching for pattern in text...\n\n");
    
    search(pattern, text, q);
    
    return 0;
}
```

## Output:
```
Text: AABAACAADAABAABA
Pattern: AABA
Searching for pattern in text...

Pattern found at index 0
Pattern found at index 9
Pattern found at index 12
```

## How the Algorithm Works:

1. **Hash Calculation**: Both pattern and text windows are hashed using a rolling hash function
2. **Initial Hash**: Calculate hash values for pattern and first window of text
3. **Sliding Window**: Move through text one character at a time
4. **Hash Comparison**: Compare hash values - if they match, do character-by-character comparison
5. **Rolling Hash**: Update hash value for next window efficiently

## Time Complexity:
- **Best/Average Case**: O(N + M) where N is text length and M is pattern length
- **Worst Case**: O(N × M) when all hash collisions occur

## Space Complexity: O(1)

The algorithm is particularly useful when searching for multiple patterns or when the text is very large, as it avoids unnecessary character comparisons in most cases.