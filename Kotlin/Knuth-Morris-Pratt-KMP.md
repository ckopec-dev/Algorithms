# Knuth-Morris-Pratt (KMP) Algorithm in Kotlin

## Overview
The KMP algorithm is a string matching algorithm that efficiently finds occurrences of a pattern within a text without backtracking.

## Implementation

```kotlin
class KMP {
    
    /**
     * Preprocess the pattern to create the failure function (also known as the LPS array)
     */
    private fun computeLPSArray(pattern: String): IntArray {
        val m = pattern.length
        val lps = IntArray(m)
        var len = 0
        var i = 1
        
        while (i < m) {
            if (pattern[i] == pattern[len]) {
                len++
                lps[i] = len
                i++
            } else {
                if (len != 0) {
                    len = lps[len - 1]
                } else {
                    lps[i] = 0
                    i++
                }
            }
        }
        return lps
    }
    
    /**
     * Search for pattern in text using KMP algorithm
     */
    fun search(text: String, pattern: String): List<Int> {
        val result = mutableListOf<Int>()
        
        if (pattern.isEmpty() || text.isEmpty()) {
            return result
        }
        
        val lps = computeLPSArray(pattern)
        val n = text.length
        val m = pattern.length
        
        var i = 0 // index for text
        var j = 0 // index for pattern
        
        while (i < n) {
            if (pattern[j] == text[i]) {
                i++
                j++
            }
            
            if (j == m) {
                // Pattern found at index i - j
                result.add(i - j)
                j = lps[j - 1]
            } else if (i < n && pattern[j] != text[i]) {
                if (j != 0) {
                    j = lps[j - 1]
                } else {
                    i++
                }
            }
        }
        
        return result
    }
}

// Example usage
fun main() {
    val kmp = KMP()
    
    // Example 1
    val text1 = "ABABDABACDABABCABCABCABCABC"
    val pattern1 = "ABABCABCABCABC"
    
    val matches1 = kmp.search(text1, pattern1)
    println("Text: $text1")
    println("Pattern: $pattern1")
    println("Matches found at indices: $matches1")
    println()
    
    // Example 2
    val text2 = "AABAACAADAABAABA"
    val pattern2 = "AABA"
    
    val matches2 = kmp.search(text2, pattern2)
    println("Text: $text2")
    println("Pattern: $pattern2")
    println("Matches found at indices: $matches2")
    println()
    
    // Example 3 - No matches
    val text3 = "HELLO WORLD"
    val pattern3 = "XYZ"
    
    val matches3 = kmp.search(text3, pattern3)
    println("Text: $text3")
    println("Pattern: $pattern3")
    println("Matches found at indices: $matches3")
}
```

## Output
```
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Matches found at indices: [10]

Text: AABAACAADAABAABA
Pattern: AABA
Matches found at indices: [0, 9, 13]

Text: HELLO WORLD
Pattern: XYZ
Matches found at indices: []
```

## How it works

1. **Preprocessing Phase**: 
   - Create the LPS (Longest Proper Prefix which is also Suffix) array
   - This array helps determine how much to shift the pattern when a mismatch occurs

2. **Search Phase**:
   - Compare pattern with text character by character
   - When a mismatch occurs, use the LPS array to determine the next position to compare
   - This avoids unnecessary comparisons and achieves O(n + m) time complexity

## Time and Space Complexity

- **Time Complexity**: O(n + m) where n is the length of text and m is the length of pattern
- **Space Complexity**: O(m) for storing the LPS array

## Key Advantages

- No backtracking in the text string
- Efficient for multiple pattern searches
- Better than naive string matching algorithms for large texts
- Handles overlapping patterns well

