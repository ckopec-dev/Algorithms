# Fermat's Factorization Method in Scala

Fermat's factorization method is based on the representation of an odd integer as the difference of two squares: n = a² - b² = (a-b)(a+b).

```scala
object FermatsFactorization {
  
  /**
   * Implements Fermat's factorization method
   * @param n the odd composite number to factor
   * @return a tuple of factors (a, b) such that a * b = n
   */
  def factor(n: Long): Option[(Long, Long)] = {
    // Handle edge cases
    if (n <= 1) return None
    if (n % 2 == 0) return None // Fermat's method works for odd numbers only
    
    var a = math.ceil(math.sqrt(n)).toLong
    var b2 = a * a - n
    
    // Continue until b² is a perfect square
    while (!isPerfectSquare(b2)) {
      a += 1
      b2 = a * a - n
    }
    
    val b = math.sqrt(b2).toLong
    Some((a - b, a + b))
  }
  
  /**
   * Check if a number is a perfect square
   */
  def isPerfectSquare(n: Long): Boolean = {
    if (n < 0) false
    else {
      val root = math.sqrt(n).toLong
      root * root == n
    }
  }
  
  /**
   * Alternative implementation that returns the actual factors
   */
  def factorize(n: Long): List[Long] = {
    factor(n) match {
      case Some((a, b)) => List(a, b)
      case None => List(n) // Return the number itself if no factors found
    }
  }
  
  def main(args: Array[String]): Unit = {
    // Test cases
    val testNumbers = List(5959L, 13L, 15L, 35L, 91L)
    
    println("Fermat's Factorization Method Results:")
    println("=====================================")
    
    testNumbers.foreach { n =>
      println(s"Factoring $n:")
      factor(n) match {
        case Some((a, b)) =>
          println(s"  Found factors: $a × $b = ${a * b}")
          println(s"  Verification: $a × $b = ${a * b}")
        case None =>
          println(s"  No factors found for $n")
      }
      println()
    }
    
    // Example with a larger number
    println("Example with larger number:")
    val largeNumber = 10007L
    println(s"Factoring $largeNumber:")
    factor(largeNumber) match {
      case Some((a, b)) =>
        println(s"  Factors: $a × $b = ${a * b}")
        println(s"  Verification: $a × $b = ${a * b}")
      case None =>
        println(s"  No factors found")
    }
  }
}

// Usage example
object ExampleUsage extends App {
  import FermatsFactorization._
  
  // Factor some numbers
  val result1 = factor(5959L)
  println(s"5959 = ${result1.map { case (a, b) => s"$a × $b" }.getOrElse("Not factored")}")
  
  val result2 = factor(15L)
  println(s"15 = ${result2.map { case (a, b) => s"$a × $b" }.getOrElse("Not factored")}")
  
  val result3 = factor(13L)
  println(s"13 = ${result3.map { case (a, b) => s"$a × $b" }.getOrElse("Not factored")}")
}
```

## How it works:

1. **Start with the square root**: Find the smallest integer `a` such that `a² ≥ n`
2. **Calculate b²**: Compute `b² = a² - n`
3. **Check if perfect square**: If `b²` is a perfect square, then we found factors
4. **Increment and repeat**: If not, increment `a` and repeat until we find a perfect square

## Example walkthrough for n = 5959:

```
a = ceil(√5959) = 78
b² = 78² - 5959 = 6084 - 5959 = 125 (not perfect square)

a = 79
b² = 79² - 5959 = 6241 - 5959 = 282 (not perfect square)

a = 80
b² = 80² - 5959 = 6400 - 5959 = 441 = 21² (perfect square!)

So: a = 80, b = 21
Factors: (80-21, 80+21) = (59, 101)
Check: 59 × 101 = 5959 ✓
```

## Time Complexity:
- **Best case**: O(1) when n is close to a perfect square
- **Worst case**: O(n) for numbers that are products of two primes that are far apart

## Limitations:
- Only works for odd composite numbers
- Inefficient for numbers where factors are very far apart
- Not suitable for very large numbers due to the search process

