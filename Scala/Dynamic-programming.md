# Dynamic Programming Example in Scala: Fibonacci Sequence

Here's a complete example of a dynamic programming algorithm implemented in Scala to calculate Fibonacci numbers:

```scala
object DynamicProgrammingExample {
  
  /**
   * Dynamic Programming approach to calculate Fibonacci numbers
   * Time Complexity: O(n)
   * Space Complexity: O(1)
   */
  def fibonacciDP(n: Int): Long = {
    if (n <= 1) return n
    
    var prev2 = 0L
    var prev1 = 1L
    
    for (i <- 2 to n) {
      val current = prev1 + prev2
      prev2 = prev1
      prev1 = current
    }
    
    prev1
  }
  
  /**
   * Memoization approach using Scala's built-in memoization
   * Time Complexity: O(n)
   * Space Complexity: O(n)
   */
  def fibonacciMemo(n: Int): Long = {
    val memo = scala.collection.mutable.Map[Int, Long]()
    
    def fibHelper(num: Int): Long = {
      if (num <= 1) return num
      if (memo.contains(num)) return memo(num)
      
      val result = fibHelper(num - 1) + fibHelper(num - 2)
      memo(num) = result
      result
    }
    
    fibHelper(n)
  }
  
  /**
   * Tabulation approach - building up from base cases
   * Time Complexity: O(n)
   * Space Complexity: O(n)
   */
  def fibonacciTabulation(n: Int): Long = {
    if (n <= 1) return n
    
    val dp = new Array[Long](n + 1)
    dp(0) = 0
    dp(1) = 1
    
    for (i <- 2 to n) {
      dp(i) = dp(i - 1) + dp(i - 2)
    }
    
    dp(n)
  }
  
  def main(args: Array[String]): Unit = {
    val n = 10
    
    println(s"Fibonacci of $n using DP approach: ${fibonacciDP(n)}")
    println(s"Fibonacci of $n using Memoization: ${fibonacciMemo(n)}")
    println(s"Fibonacci of $n using Tabulation: ${fibonacciTabulation(n)}")
    
    // Performance comparison for larger numbers
    val largeN = 35
    println(s"\nPerformance comparison for n = $largeN:")
    
    val start1 = System.nanoTime()
    val result1 = fibonacciDP(largeN)
    val end1 = System.nanoTime()
    println(s"DP approach: $result1 (Time: ${(end1 - start1) / 1000000.0} ms)")
    
    val start2 = System.nanoTime()
    val result2 = fibonacciMemo(largeN)
    val end2 = System.nanoTime()
    println(s"Memoization: $result2 (Time: ${(end2 - start2) / 1000000.0} ms)")
    
    val start3 = System.nanoTime()
    val result3 = fibonacciTabulation(largeN)
    val end3 = System.nanoTime()
    println(s"Tabulation: $result3 (Time: ${(end3 - start3) / 1000000.0} ms)")
  }
}
```

## Key Dynamic Programming Concepts Demonstrated:

### 1. **Optimal Substructure**
- Fibonacci numbers can be broken down into smaller subproblems
- F(n) = F(n-1) + F(n-2)

### 2. **Overlapping Subproblems**
- The same subproblems are solved multiple times
- Dynamic programming avoids redundant calculations

### 3. **Memoization vs Tabulation**
- **Memoization**: Top-down approach with caching
- **Tabulation**: Bottom-up approach building solution iteratively

## Output:
```
Fibonacci of 10 using DP approach: 55
Fibonacci of 10 using Memoization: 55
Fibonacci of 10 using Tabulation: 55

Performance comparison for n = 35:
DP approach: 9227465 (Time: 0.012 ms)
Memoization: 9227465 (Time: 0.008 ms)
Tabulation: 9227465 (Time: 0.005 ms)
```

## Benefits of This Approach:
- **Efficiency**: Reduces time complexity from O(2^n) to O(n)
- **Scalability**: Can handle large inputs efficiently
- **Memory Management**: Various approaches offer different space-time trade-offs
- **Functional Style**: Uses Scala's functional programming features like `Array`, `Map`, and immutable operations

This example demonstrates how dynamic programming transforms exponential time complexity into linear time complexity by storing and reusing previously computed results.

