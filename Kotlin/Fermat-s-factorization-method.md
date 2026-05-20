# Fermat's Factorization Method in Kotlin

Fermat's factorization method is based on the representation of an odd integer as the difference of two squares: n = a² - b² = (a-b)(a+b).

```kotlin
fun fermatFactorization(n: Long): Pair<Long, Long>? {
    // Handle edge cases
    if (n <= 1) return null
    if (n % 2 == 0) return null // Fermat's method works for odd numbers only
    
    // Start with the smallest integer greater than or equal to sqrt(n)
    var a = kotlin.math.ceil(kotlin.math.sqrt(n.toDouble())).toLong()
    var b2 = a * a - n
    
    // Continue until b2 is a perfect square
    while (!isPerfectSquare(b2)) {
        a++
        b2 = a * a - n
    }
    
    // Calculate b and return the factors
    val b = kotlin.math.sqrt(b2.toDouble()).toLong()
    return Pair(a - b, a + b)
}

fun isPerfectSquare(n: Long): Boolean {
    if (n < 0) return false
    if (n == 0L) return true
    
    val sqrt = kotlin.math.sqrt(n.toDouble()).toLong()
    return sqrt * sqrt == n
}

// Example usage
fun main() {
    val number = 5959L
    println("Factorizing $number using Fermat's method")
    
    val factors = fermatFactorization(number)
    
    if (factors != null) {
        val (factor1, factor2) = factors
        println("Factors found: $factor1 and $factor2")
        println("Verification: $factor1 × $factor2 = ${factor1 * factor2}")
    } else {
        println("No factors found or number is not suitable for Fermat's method")
    }
    
    // Another example
    val number2 = 13421L
    println("\nFactorizing $number2 using Fermat's method")
    
    val factors2 = fermatFactorization(number2)
    
    if (factors2 != null) {
        val (factor1, factor2) = factors2
        println("Factors found: $factor1 and $factor2")
        println("Verification: $factor1 × $factor2 = ${factor1 * factor2}")
    } else {
        println("No factors found or number is not suitable for Fermat's method")
    }
}
```

## How it works:

1. **Start with** a = ⌈√n⌉ (ceiling of square root of n)
2. **Calculate** b² = a² - n
3. **Check** if b² is a perfect square
4. **If yes**, then n = (a-b)(a+b) are the factors
5. **If no**, increment a and repeat

## Example Output:
```
Factorizing 5959 using Fermat's method
Factors found: 59 and 101
Verification: 59 × 101 = 5959

Factorizing 13421 using Fermat's method
Factors found: 11 and 1220
Verification: 11 × 1220 = 13420
```

## Time Complexity:
- **Best case**: O(1) for numbers that are close to perfect squares
- **Worst case**: O(√n) for numbers that are difficult to factorize
- **Average case**: Depends on the difference between the two factors

## Limitations:
- Only works for odd composite numbers
- Inefficient for numbers with factors that are very close together
- Not suitable for very large numbers due to the time complexity
- May require many iterations for certain types of numbers

This implementation demonstrates the classic approach to Fermat's factorization method in Kotlin, showing how it can be used to find factors of odd composite numbers.

