# Rabin-Karp Algorithm in Kotlin

The Rabin-Karp algorithm is a string searching algorithm that uses hashing to find patterns in text. It's particularly useful for finding multiple pattern occurrences efficiently.

## Implementation

```kotlin
class RabinKarp {
    private val d = 256  // Number of characters in the input alphabet
    private val q = 101  // A prime number for hashing
    
    fun search(pattern: String, text: String): List<Int> {
        val result = mutableListOf<Int>()
        val m = pattern.length
        val n = text.length
        
        // Edge case: if pattern is longer than text
        if (m > n) return result
        
        // Calculate d^(m-1) % q
        var h = 1
        for (i in 0 until m - 1) {
            h = (h * d) % q
        }
        
        // Calculate hash value of pattern and first window of text
        var p = 0  // Hash value for pattern
        var t = 0  // Hash value for text
        
        for (i in 0 until m) {
            p = (d * p + pattern[i].code) % q
            t = (d * t + text[i].code) % q
        }
        
        // Slide the pattern over text one by one
        for (i in 0..n - m) {
            // Check if hash values match
            if (p == t) {
                // Check character by character
                var j = 0
                while (j < m) {
                    if (text[i + j] != pattern[j]) {
                        break
                    }
                    j++
                }
                
                // If pattern is found
                if (j == m) {
                    result.add(i)
                }
            }
            
            // Calculate hash value for next window of text
            if (i < n - m) {
                t = (d * (t - text[i].code * h) + text[i + m].code) % q
                
                // Handle negative hash values
                if (t < 0) {
                    t += q
                }
            }
        }
        
        return result
    }
}

// Example usage
fun main() {
    val rk = RabinKarp()
    
    val text = "ABABDABACDABABCABCABC"
    val pattern = "ABABCABCABC"
    
    println("Text: $text")
    println("Pattern: $pattern")
    
    val matches = rk.search(pattern, text)
    
    if (matches.isNotEmpty()) {
        println("Pattern found at positions: ${matches.joinToString(", ")}")
        matches.forEach { position ->
            println("Position $position: ${text.substring(position, position + pattern.length)}")
        }
    } else {
        println("Pattern not found in text")
    }
    
    // Another example with multiple occurrences
    println("\n--- Another Example ---")
    val text2 = "AABAACAADAABAABA"
    val pattern2 = "AABA"
    
    println("Text: $text2")
    println("Pattern: $pattern2")
    
    val matches2 = rk.search(pattern2, text2)
    
    if (matches2.isNotEmpty()) {
        println("Pattern found at positions: ${matches2.joinToString(", ")}")
    } else {
        println("Pattern not found in text")
    }
}
```

## Output

```
Text: ABABDABACDABABCABCABC
Pattern: ABABCABCABC
Pattern found at positions: 10
Position 10: ABABCABCABC

--- Another Example ---
Text: AABAACAADAABAABA
Pattern: AABA
Pattern found at positions: 0, 9, 12
```

## How it works:

1. **Hash Calculation**: Calculate hash values for the pattern and the first window of text
2. **Sliding Window**: Move the window one character at a time
3. **Hash Comparison**: Compare hash values first (fast operation)
4. **Character Comparison**: If hash values match, compare actual characters
5. **Rolling Hash**: Use rolling hash technique to efficiently update hash values

## Time Complexity:
- **Best/Average Case**: O(n + m) where n is text length and m is pattern length
- **Worst Case**: O(n × m) when all hash collisions occur

## Space Complexity: O(1)

The algorithm is efficient for multiple pattern searches and when the pattern is much smaller than the text.

