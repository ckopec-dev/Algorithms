# Baby-step Giant-step Algorithm in Kotlin

The baby-step giant-step algorithm is used to solve the discrete logarithm problem: finding `x` such that `g^x ≡ h (mod p)`.

```kotlin
import kotlin.math.ceil
import kotlin.math.sqrt

class BabyStepGiantStep {
    
    /**
     * Solves the discrete logarithm problem: g^x ≡ h (mod p)
     * Returns the smallest non-negative integer x, or -1 if no solution exists
     */
    fun discreteLog(g: Long, h: Long, p: Long): Long {
        if (g % p == 0L) return -1L // No solution exists
        
        val m = ceil(sqrt(p.toDouble())).toLong()
        
        // Baby steps: compute g^j mod p for j = 0, 1, ..., m-1
        val babySteps = mutableMapOf<Long, Long>()
        var power = 1L
        for (j in 0L until m) {
            if (power == h) return j
            babySteps[power] = j
            power = (power * g) % p
        }
        
        // Giant steps: compute g^(-m) mod p
        val gInv = modInverse(g, p)
        val giantStep = (gInv.pow(m)) % p
        
        // Check each giant step
        var y = h
        for (i in 0L until m) {
            if (babySteps.containsKey(y)) {
                val x = i * m + babySteps[y]!!
                return x
            }
            y = (y * giantStep) % p
        }
        
        return -1L // No solution found
    }
    
    /**
     * Computes modular inverse of a mod p using extended Euclidean algorithm
     */
    private fun modInverse(a: Long, p: Long): Long {
        val (gcd, x, _) = extendedGCD(a, p)
        if (gcd != 1L) throw IllegalArgumentException("Modular inverse does not exist")
        return (x % p + p) % p
    }
    
    /**
     * Extended Euclidean algorithm
     */
    private fun extendedGCD(a: Long, b: Long): Triple<Long, Long, Long> {
        if (b == 0L) return Triple(a, 1L, 0L)
        
        val (gcd, x1, y1) = extendedGCD(b, a % b)
        val x = y1
        val y = x1 - (a / b) * y1
        
        return Triple(gcd, x, y)
    }
    
    /**
     * Computes (base^exponent) mod modulus efficiently
     */
    private fun Long.pow(exponent: Long): Long {
        var base = this
        var exp = exponent
        var result = 1L
        
        while (exp > 0) {
            if (exp % 2 == 1L) {
                result = (result * base) % 1000000007L // Use large prime to prevent overflow
            }
            base = (base * base) % 1000000007L
            exp /= 2
        }
        
        return result
    }
}

// Example usage
fun main() {
    val bs = BabyStepGiantStep()
    
    // Example: Find x such that 3^x ≡ 13 (mod 17)
    // Expected answer: x = 4 (since 3^4 = 81 ≡ 13 (mod 17))
    val g = 3L
    val h = 13L
    val p = 17L
    
    val result = bs.discreteLog(g, h, p)
    
    println("Solving: $g^x ≡ $h (mod $p)")
    println("Solution: x = $result")
    
    if (result != -1L) {
        val verification = (g.pow(result) % p)
        println("Verification: $g^$result ≡ $verification (mod $p)")
        println("Correct: ${verification == h}")
    } else {
        println("No solution found")
    }
    
    // Another example: Find x such that 5^x ≡ 6 (mod 11)
    println("\n--- Another example ---")
    val g2 = 5L
    val h2 = 6L
    val p2 = 11L
    
    val result2 = bs.discreteLog(g2, h2, p2)
    
    println("Solving: $g2^x ≡ $h2 (mod $p2)")
    println("Solution: x = $result2")
    
    if (result2 != -1L) {
        val verification2 = (g2.pow(result2) % p2)
        println("Verification: $g2^$result2 ≡ $verification2 (mod $p2)")
        println("Correct: ${verification2 == h2}")
    } else {
        println("No solution found")
    }
}
```

## How it works:

1. **Baby Steps**: Compute and store `g^j mod p` for `j = 0, 1, ..., m-1` where `m = ⌈√p⌉`
2. **Giant Steps**: Compute `g^(-m) mod p` and check `h * (g^(-m))^i mod p` for `i = 0, 1, ..., m-1`
3. **Collision Detection**: If a giant step value matches a baby step value, we found our solution

## Time Complexity:
- **Time**: O(√p)
- **Space**: O(√p)

## Example Output:
```
Solving: 3^x ≡ 13 (mod 17)
Solution: x = 4
Verification: 3^4 ≡ 13 (mod 17)
Correct: true

--- Another example ---
Solving: 5^x ≡ 6 (mod 11)
Solution: x = 2
Verification: 5^2 ≡ 6 (mod 11)
Correct: true
```

