# Euclidean Algorithm in Kotlin

Here's an implementation of the Euclidean algorithm to find the Greatest Common Divisor (GCD) in Kotlin:

```kotlin
fun gcd(a: Int, b: Int): Int {
    // Handle negative numbers by taking absolute values
    val absA = kotlin.math.abs(a)
    val absB = kotlin.math.abs(b)
    
    // Base case: if b is 0, return a
    if (absB == 0) {
        return absA
    }
    
    // Recursive case: gcd(a, b) = gcd(b, a % b)
    return gcd(absB, absA % absB)
}

// Iterative version (more efficient for large numbers)
fun gcdIterative(a: Int, b: Int): Int {
    var absA = kotlin.math.abs(a)
    var absB = kotlin.math.abs(b)
    
    while (absB != 0) {
        val temp = absB
        absB = absA % absB
        absA = temp
    }
    
    return absA
}

fun main() {
    val num1 = 48
    val num2 = 18
    
    println("Finding GCD of $num1 and $num2")
    println("Recursive approach: ${gcd(num1, num2)}")
    println("Iterative approach: ${gcdIterative(num1, num2)}")
    
    // Example trace for gcd(48, 18):
    // gcd(48, 18) = gcd(18, 48 % 18) = gcd(18, 12)
    // gcd(18, 12) = gcd(12, 18 % 12) = gcd(12, 6)
    // gcd(12, 6) = gcd(6, 12 % 6) = gcd(6, 0)
    // gcd(6, 0) = 6
}
```

**Output:**
```
Finding GCD of 48 and 18
Recursive approach: 6
Iterative approach: 6
```

## How it works:

1. **Recursive approach**: Uses the mathematical property that `gcd(a, b) = gcd(b, a % b)`
2. **Iterative approach**: Uses a while loop to avoid potential stack overflow with large numbers
3. **Base case**: When `b = 0`, the GCD is `a`
4. **Negative numbers**: Handles negative inputs by taking absolute values

The algorithm has a time complexity of O(log(min(a, b))) and is very efficient for finding the GCD of two numbers.

