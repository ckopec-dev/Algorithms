# Knuth-Morris-Pratt (KMP) Algorithm in Kotlin

## Overview
The KMP algorithm is a string searching algorithm that efficiently finds occurrences of a pattern within a text without backtracking.

## Implementation

```kotlin
class KMP {
    
    /**
     * Preprocess the pattern to create the failure function (also known as LPS - Longest Proper Prefix)
     */
    private fun computeLPSArray(pattern: String): IntArray {
        val lps = IntArray(pattern.length)
        var len = 0
        var i = 1
        
        while (i < pattern.length) {
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
        var i = 0 // index for text
        var j = 0 // index for pattern
        
        while (i < text.length) {
            if (pattern[j] == text[i]) {
                i++
                j++
            }
            
            if (j == pattern.length) {
                // Pattern found at index i - j
                result.add(i - j)
                j = lps[j - 1]
            } else if (i < text.length && pattern[j] != text[i]) {
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
    val result1 = kmp.search(text1, pattern1)
    
    println("Text: $text1")
    println("Pattern: $pattern1")
    println("Found at positions: $result1")
    println()
    
    // Example 2
    val text2 = "AABAACAADAABAABA"
    val pattern2 = "AABA"
    val result2 = kmp.search(text2, pattern2)
    
    println("Text: $text2")
    println("Pattern: $pattern2")
    println("Found at positions: $result2")
    println()
    
    // Example 3 - No match
    val text3 = "HELLO WORLD"
    val pattern3 = "XYZ"
    val result3 = kmp.search(text3, pattern3)
    
    println("Text: $text3")
    println("Pattern: $pattern3")
    println("Found at positions: $result3")
}
```

## Output
```
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Found at positions: [10]

Text: AABAACAADAABAABA
Pattern: AABA
Found at positions: [0, 9, 12]

Text: HELLO WORLD
Pattern: XYZ
Found at positions: []
```

## How it works

1. **LPS Array Construction**: The algorithm first builds a "Longest Proper Prefix which is also Suffix" array for the pattern
2. **Search Phase**: Uses the LPS array to avoid unnecessary character comparisons
3. **Efficient Backtracking**: When a mismatch occurs, instead of going back in the text, it uses the LPS array to determine the next position to compare

## Time Complexity
- **Preprocessing**: O(m) where m is the length of the pattern
- **Search**: O(n) where n is the length of the text
- **Overall**: O(n + m)

## Space Complexity
- O(m) for storing the LPS array

The KMP algorithm is particularly useful when you need to search for multiple occurrences of the same pattern in different texts, as the preprocessing step can be reused.

