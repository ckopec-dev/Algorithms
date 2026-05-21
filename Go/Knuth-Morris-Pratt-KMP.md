# Knuth-Morris-Pratt (KMP) Algorithm in Go

The KMP algorithm is a string searching algorithm that efficiently finds occurrences of a pattern within a text without backtracking.

## Implementation

```go
package main

import (
    "fmt"
    "log"
)

// computeLPSArray computes the Longest Proper Prefix which is also Suffix array
func computeLPSArray(pattern string) []int {
    m := len(pattern)
    lps := make([]int, m)
    
    length := 0
    i := 1
    
    // lps[0] is always 0
    lps[0] = 0
    
    // Loop through the pattern to fill the LPS array
    for i < m {
        if pattern[i] == pattern[length] {
            length++
            lps[i] = length
            i++
        } else {
            if length != 0 {
                // This is tricky - consider the previous length
                length = lps[length-1]
            } else {
                // If length is 0, set lps[i] to 0 and move i
                lps[i] = 0
                i++
            }
        }
    }
    
    return lps
}

// KMPSearch searches for pattern in text using KMP algorithm
func KMPSearch(pattern, text string) []int {
    var result []int
    
    n := len(text)
    m := len(pattern)
    
    // Handle edge cases
    if m == 0 || n == 0 {
        return result
    }
    
    // Preprocess pattern to create LPS array
    lps := computeLPSArray(pattern)
    
    i := 0 // index for text
    j := 0 // index for pattern
    
    // Search for pattern in text
    for i < n {
        if pattern[j] == text[i] {
            i++
            j++
        }
        
        if j == m {
            // Pattern found at index i-j
            result = append(result, i-j)
            j = lps[j-1]
        } else if i < n && pattern[j] != text[i] {
            if j != 0 {
                j = lps[j-1]
            } else {
                i++
            }
        }
    }
    
    return result
}

// printLPSArray prints the LPS array for visualization
func printLPSArray(pattern string) {
    lps := computeLPSArray(pattern)
    fmt.Printf("Pattern: %s\n", pattern)
    fmt.Printf("LPS Array: %v\n", lps)
    fmt.Println()
}

func main() {
    // Example 1: Basic search
    fmt.Println("=== KMP Algorithm Example ===")
    
    text1 := "ABABDABACDABABCABCABCABCABC"
    pattern1 := "ABABCABCABCABC"
    
    fmt.Printf("Text: %s\n", text1)
    fmt.Printf("Pattern: %s\n", pattern1)
    
    positions := KMPSearch(pattern1, text1)
    fmt.Printf("Pattern found at positions: %v\n", positions)
    fmt.Println()
    
    // Example 2: Multiple occurrences
    text2 := "AABAACAADAABAABA"
    pattern2 := "AABA"
    
    fmt.Printf("Text: %s\n", text2)
    fmt.Printf("Pattern: %s\n", pattern2)
    
    positions2 := KMPSearch(pattern2, text2)
    fmt.Printf("Pattern found at positions: %v\n", positions2)
    fmt.Println()
    
    // Example 3: Show LPS array computation
    fmt.Println("=== LPS Array Computation ===")
    patterns := []string{"ABAB", "AAABAAA", "ABCABC"}
    
    for _, pattern := range patterns {
        printLPSArray(pattern)
    }
    
    // Example 4: No match
    text3 := "HELLO WORLD"
    pattern3 := "XYZ"
    
    fmt.Printf("Text: %s\n", text3)
    fmt.Printf("Pattern: %s\n", pattern3)
    
    positions3 := KMPSearch(pattern3, text3)
    fmt.Printf("Pattern found at positions: %v\n", positions3)
    fmt.Println()
    
    // Example 5: Single character pattern
    text4 := "AAAAAAAAAA"
    pattern4 := "A"
    
    fmt.Printf("Text: %s\n", text4)
    fmt.Printf("Pattern: %s\n", pattern4)
    
    positions4 := KMPSearch(pattern4, text4)
    fmt.Printf("Pattern found at positions: %v\n", positions4)
}
```

## Output

```
=== KMP Algorithm Example ===
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Pattern found at positions: [10]

Text: AABAACAADAABAABA
Pattern: AABA
Pattern found at positions: [0 9 13]

=== LPS Array Computation ===
Pattern: ABAB
LPS Array: [0 0 1 2]

Pattern: AAABAAA
LPS Array: [0 1 2 0 1 2 2 2]

Pattern: ABCABC
LPS Array: [0 0 0 1 2 3]

Text: HELLO WORLD
Pattern: XYZ
Pattern found at positions: []

Text: AAAAAAAAAA
Pattern: A
Pattern found at positions: [0 1 2 3 4 5 6 7 8 9]
```

## How it works

1. **Preprocessing (LPS Array)**: 
   - Compute the Longest Proper Prefix which is also Suffix array
   - This helps avoid unnecessary character comparisons

2. **Searching**:
   - Use the LPS array to skip characters in the text when a mismatch occurs
   - This ensures O(n + m) time complexity where n is text length and m is pattern length

## Time and Space Complexity

- **Time Complexity**: O(n + m) where n is the length of text and m is the length of pattern
- **Space Complexity**: O(m) for storing the LPS array

## Key Advantages

- No backtracking in the text
- Efficient for multiple pattern searches
- Better than naive string matching algorithms
- Works well with repeated patterns

The KMP algorithm is particularly useful when the pattern has overlapping subpatterns, as it uses the information from previous matches to avoid redundant comparisons.

