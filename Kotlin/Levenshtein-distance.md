# Levenshtein Distance Algorithm in Kotlin

The Levenshtein distance is a string metric that measures the difference between two sequences by counting the minimum number of single-character edits (insertions, deletions, or substitutions) required to change one string into another.

## Implementation

```kotlin
fun levenshteinDistance(str1: String, str2: String): Int {
    val m = str1.length
    val n = str2.length
    
    // Create a 2D array to store the distances
    val dp = Array(m + 1) { IntArray(n + 1) }
    
    // Initialize base cases
    for (i in 0..m) {
        dp[i][0] = i
    }
    
    for (j in 0..n) {
        dp[0][j] = j
    }
    
    // Fill the dp table
    for (i in 1..m) {
        for (j in 1..n) {
            if (str1[i - 1] == str2[j - 1]) {
                // Characters match, no operation needed
                dp[i][j] = dp[i - 1][j - 1]
            } else {
                // Take minimum of three operations
                dp[i][j] = 1 + minOf(
                    dp[i - 1][j],     // deletion
                    dp[i][j - 1],     // insertion
                    dp[i - 1][j - 1]  // substitution
                )
            }
        }
    }
    
    return dp[m][n]
}

// Alternative implementation with optimized space complexity
fun levenshteinDistanceOptimized(str1: String, str2: String): Int {
    val m = str1.length
    val n = str2.length
    
    // Use only two rows instead of full matrix
    var prev = IntArray(n + 1) { it }
    var curr = IntArray(n + 1)
    
    for (i in 1..m) {
        curr[0] = i
        
        for (j in 1..n) {
            if (str1[i - 1] == str2[j - 1]) {
                curr[j] = prev[j - 1]
            } else {
                curr[j] = 1 + minOf(
                    prev[j],      // deletion
                    curr[j - 1],  // insertion
                    prev[j - 1]   // substitution
                )
            }
        }
        
        // Swap arrays
        val temp = prev
        prev = curr
        curr = temp
    }
    
    return prev[n]
}

// Example usage
fun main() {
    val word1 = "kitten"
    val word2 = "sitting"
    
    val distance = levenshteinDistance(word1, word2)
    println("Levenshtein distance between '$word1' and '$word2': $distance")
    
    // More examples
    val examples = listOf(
        Pair("kitten", "sitting"),
        Pair("saturday", "sunday"),
        Pair("hello", "world"),
        Pair("abc", "abc"),
        Pair("", "abc")
    )
    
    println("\nExamples:")
    examples.forEach { (str1, str2) ->
        val dist = levenshteinDistance(str1, str2)
        println("'$str1' -> '$str2': $dist")
    }
}
```

## Output

```
Levenshtein distance between 'kitten' and 'sitting': 3

Examples:
'kitten' -> 'sitting': 3
'saturday' -> 'sunday': 3
'hello' -> 'world': 4
'abc' -> 'abc': 0
'' -> 'abc': 3
```

## How it works

1. **Initialization**: Create a 2D array where `dp[i][j]` represents the minimum edit distance between the first `i` characters of `str1` and the first `j` characters of `str2`.

2. **Base cases**: 
   - `dp[i][0] = i` (deleting all characters from str1)
   - `dp[0][j] = j` (inserting all characters to get str2)

3. **Recurrence relation**:
   - If characters match: `dp[i][j] = dp[i-1][j-1]`
   - If characters don't match: `dp[i][j] = 1 + min(dp[i-1][j], dp[i][j-1], dp[i-1][j-1])`

4. **Result**: `dp[m][n]` contains the final Levenshtein distance.

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the strings
- **Space Complexity**: 
  - Standard version: O(m × n)
  - Optimized version: O(min(m, n))

The algorithm is widely used in spell checkers, DNA sequence analysis, and fuzzy string matching applications.

