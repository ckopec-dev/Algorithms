# Longest Common Subsequence (LCS) in Scala

Here's an implementation of the LCS algorithm using dynamic programming in Scala:

```scala
object LCS {
  
  /**
   * Computes the length of the longest common subsequence between two strings
   * @param str1 First string
   * @param str2 Second string
   * @return Length of LCS
   */
  def lcsLength(str1: String, str2: String): Int = {
    val m = str1.length
    val n = str2.length
    
    // Create a 2D DP table
    val dp = Array.ofDim[Int](m + 1, n + 1)
    
    // Fill the DP table
    for (i <- 1 to m; j <- 1 to n) {
      if (str1(i - 1) == str2(j - 1)) {
        dp(i)(j) = dp(i - 1)(j - 1) + 1
      } else {
        dp(i)(j) = math.max(dp(i - 1)(j), dp(i)(j - 1))
      }
    }
    
    dp(m)(n)
  }
  
  /**
   * Computes the actual longest common subsequence string
   * @param str1 First string
   * @param str2 Second string
   * @return The LCS string
   */
  def lcsString(str1: String, str2: String): String = {
    val m = str1.length
    val n = str2.length
    
    // Create a 2D DP table
    val dp = Array.ofDim[Int](m + 1, n + 1)
    
    // Fill the DP table
    for (i <- 1 to m; j <- 1 to n) {
      if (str1(i - 1) == str2(j - 1)) {
        dp(i)(j) = dp(i - 1)(j - 1) + 1
      } else {
        dp(i)(j) = math.max(dp(i - 1)(j), dp(i)(j - 1))
      }
    }
    
    // Backtrack to find the actual LCS
    val result = new StringBuilder
    var i = m
    var j = n
    
    while (i > 0 && j > 0) {
      if (str1(i - 1) == str2(j - 1)) {
        result.append(str1(i - 1))
        i -= 1
        j -= 1
      } else if (dp(i - 1)(j) > dp(i)(j - 1)) {
        i -= 1
      } else {
        j -= 1
      }
    }
    
    result.reverse.toString
  }
  
  def main(args: Array[String]): Unit = {
    val str1 = "ABCDGH"
    val str2 = "AEDFHR"
    
    println(s"String 1: $str1")
    println(s"String 2: $str2")
    println(s"LCS Length: ${lcsLength(str1, str2)}")
    println(s"LCS String: ${lcsString(str1, str2)}")
    
    // Another example
    val str3 = "AGGTAB"
    val str4 = "GXTXAYB"
    
    println(s"\nString 1: $str3")
    println(s"String 2: $str4")
    println(s"LCS Length: ${lcsLength(str3, str4)}")
    println(s"LCS String: ${lcsString(str3, str4)}")
  }
}
```

## Output:
```
String 1: ABCDGH
String 2: AEDFHR
LCS Length: 3
LCS String: ADH

String 1: AGGTAB
String 2: GXTXAYB
LCS Length: 4
LCS String: GTAB
```

## How it works:

1. **Dynamic Programming Approach**: Uses a 2D table `dp[i][j]` where each cell represents the length of LCS for substrings `str1[0...i-1]` and `str2[0...j-1]`

2. **Recurrence Relation**:
   - If characters match: `dp[i][j] = dp[i-1][j-1] + 1`
   - If characters don't match: `dp[i][j] = max(dp[i-1][j], dp[i][j-1])`

3. **Time Complexity**: O(m × n) where m and n are the lengths of the input strings
4. **Space Complexity**: O(m × n) for the DP table

The algorithm first computes the length of the LCS, then backtracks through the DP table to reconstruct the actual subsequence string.

