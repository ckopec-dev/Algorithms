# Damerau-Levenshtein Distance in Kotlin

Here's an implementation of the Damerau-Levenshtein distance algorithm in Kotlin:

```kotlin
fun damerauLevenshteinDistance(str1: String, str2: String): Int {
    val m = str1.length
    val n = str2.length
    
    // Create a matrix to store distances
    val dp = Array(m + 1) { IntArray(n + 1) }
    
    // Initialize base cases
    for (i in 0..m) {
        dp[i][0] = i
    }
    
    for (j in 0..n) {
        dp[0][j] = j
    }
    
    // Fill the matrix
    for (i in 1..m) {
        for (j in 1..n) {
            val cost = if (str1[i - 1] == str2[j - 1]) 0 else 1
            
            dp[i][j] = minOf(
                dp[i - 1][j] + 1,        // deletion
                dp[i][j - 1] + 1,        // insertion
                dp[i - 1][j - 1] + cost   // substitution
            )
            
            // Check for transposition (Damerau-Levenshtein specific)
            if (i > 1 && j > 1 && 
                str1[i - 1] == str2[j - 2] && 
                str1[i - 2] == str2[j - 1]) {
                dp[i][j] = minOf(dp[i][j], dp[i - 2][j - 2] + cost)
            }
        }
    }
    
    return dp[m][n]
}

// Example usage
fun main() {
    val word1 = "kitten"
    val word2 = "sitting"
    
    val distance = damerauLevenshteinDistance(word1, word2)
    println("Damerau-Levenshtein distance between '$word1' and '$word2': $distance")
    
    // More examples
    println("Distance between 'hello' and 'hallo': ${damerauLevenshteinDistance("hello", "hallo")}")
    println("Distance between 'cat' and 'dog': ${damerauLevenshteinDistance("cat", "dog")}")
    println("Distance between 'abc' and 'acb': ${damerauLevenshteinDistance("abc", "acb")}")
    println("Distance between 'abc' and 'bac': ${damerauLevenshteinDistance("abc", "bac")}")
}
```

## Output:
```
Damerau-Levenshtein distance between 'kitten' and 'sitting': 3
Distance between 'hello' and 'hallo': 1
Distance between 'cat' and 'dog': 3
Distance between 'abc' and 'acb': 1
Distance between 'abc' and 'bac': 1
```

## How it works:

1. **Base cases**: Initialize the first row and column with incremental values
2. **Dynamic programming**: Fill the matrix using the recurrence relation
3. **Transposition check**: The key addition to Levenshtein distance - check if swapping two adjacent characters reduces the distance
4. **Return**: The value at the bottom-right corner represents the minimum edit distance

## Key differences from Levenshtein distance:
- **Transposition**: Allows swapping of two adjacent characters with cost 1
- **Additional condition**: Checks for `str1[i-1] == str2[j-2] && str1[i-2] == str2[j-1]` to detect transpositions

The algorithm has a time complexity of O(m×n) and space complexity of O(m×n) where m and n are the lengths of the input strings.

