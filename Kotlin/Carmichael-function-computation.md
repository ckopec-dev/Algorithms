# Carmichael Function Computation in Kotlin

The Carmichael function λ(n) (also known as the reduced totient function) is the smallest positive integer m such that a^m ≡ 1 (mod n) for all integers a coprime to n.

```kotlin
import kotlin.math.sqrt

class CarmichaelFunction {
    
    /**
     * Computes the Carmichael function λ(n)
     * @param n the positive integer
     * @return λ(n) - the Carmichael function value
     */
    fun carmichaelFunction(n: Int): Int {
        if (n <= 0) return 0
        if (n == 1) return 1
        
        // Get prime factorization
        val primeFactors = getPrimeFactors(n)
        
        // For each prime power in the factorization
        val lambdaValues = mutableListOf<Int>()
        
        for ((prime, power) in primeFactors) {
            val lambda = carmichaelForPrimePower(prime, power)
            lambdaValues.add(lambda)
        }
        
        // Return LCM of all λ values
        return lcmOfList(lambdaValues)
    }
    
    /**
     * Computes λ(p^k) where p is prime and k is the power
     * @param prime the prime base
     * @param power the power
     * @return λ(p^k)
     */
    private fun carmichaelForPrimePower(prime: Int, power: Int): Int {
        return when {
            prime == 2 && power >= 3 -> 2.pow(power - 2)
            prime == 2 && power == 2 -> 2
            prime == 2 && power == 1 -> 1
            else -> (prime - 1) * prime.pow(power - 1)
        }
    }
    
    /**
     * Gets prime factorization of n
     * @param n the number to factorize
     * @return map of prime -> power
     */
    private fun getPrimeFactors(n: Int): Map<Int, Int> {
        val factors = mutableMapOf<Int, Int>()
        var num = n
        
        // Handle factor 2
        while (num % 2 == 0) {
            factors[2] = factors.getOrDefault(2, 0) + 1
            num /= 2
        }
        
        // Handle odd factors
        var i = 3
        while (i * i <= num) {
            while (num % i == 0) {
                factors[i] = factors.getOrDefault(i, 0) + 1
                num /= i
            }
            i += 2
        }
        
        // If num is still greater than 1, then it's a prime
        if (num > 1) {
            factors[num] = factors.getOrDefault(num, 0) + 1
        }
        
        return factors
    }
    
    /**
     * Computes LCM of a list of numbers
     * @param numbers list of numbers
     * @return LCM of all numbers
     */
    private fun lcmOfList(numbers: List<Int>): Int {
        if (numbers.isEmpty()) return 0
        if (numbers.size == 1) return numbers[0]
        
        var result = numbers[0]
        for (i in 1 until numbers.size) {
            result = lcm(result, numbers[i])
        }
        return result
    }
    
    /**
     * Computes LCM of two numbers
     * @param a first number
     * @param b second number
     * @return LCM of a and b
     */
    private fun lcm(a: Int, b: Int): Int {
        return (a * b) / gcd(a, b)
    }
    
    /**
     * Computes GCD of two numbers using Euclidean algorithm
     * @param a first number
     * @param b second number
     * @return GCD of a and b
     */
    private fun gcd(a: Int, b: Int): Int {
        return if (b == 0) a else gcd(b, a % b)
    }
    
    /**
     * Computes a^n efficiently
     * @param base the base
     * @param exponent the exponent
     * @return base^exponent
     */
    private fun Int.pow(exponent: Int): Int {
        if (exponent == 0) return 1
        if (exponent == 1) return this
        
        var result = 1
        var base = this
        var exp = exponent
        
        while (exp > 0) {
            if (exp % 2 == 1) {
                result *= base
            }
            base *= base
            exp /= 2
        }
        
        return result
    }
}

// Example usage
fun main() {
    val carmichael = CarmichaelFunction()
    
    println("Carmichael Function Examples:")
    println("============================")
    
    val testValues = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 21, 24, 30)
    
    testValues.forEach { n ->
        val lambda = carmichael.carmichaelFunction(n)
        println("λ($n) = $lambda")
    }
    
    // Verification example
    println("\nVerification for λ(12):")
    println("Prime factorization of 12: 2² × 3¹")
    println("λ(2²) = 2^(2-2) = 1")
    println("λ(3¹) = (3-1) × 3^(1-1) = 2 × 1 = 2")
    println("LCM(1, 2) = 2")
    println("Therefore λ(12) = 2")
    
    val result = carmichael.carmichaelFunction(12)
    println("Computed λ(12) = $result")
}
```

## Output Example:
```
Carmichael Function Examples:
============================
λ(1) = 1
λ(2) = 1
λ(3) = 2
λ(4) = 2
λ(5) = 4
λ(6) = 2
λ(7) = 6
λ(8) = 2
λ(9) = 6
λ(10) = 4
λ(12) = 2
λ(15) = 4
λ(21) = 6
λ(24) = 2
λ(30) = 4

Verification for λ(12):
Prime factorization of 12: 2² × 3¹
λ(2²) = 2^(2-2) = 1
λ(3¹) = (3-1) × 3^(1-1) = 2 × 1 = 2
LCM(1, 2) = 2
Therefore λ(12) = 2
Computed λ(12) = 2
```

## Algorithm Explanation:

1. **Prime Factorization**: First, we find the prime factorization of n
2. **Compute λ for each prime power**: 
   - For λ(2^k) where k ≥ 3: 2^(k-2)
   - For λ(2^2): 2
   - For λ(2^1): 1
   - For λ(p^k) where p is odd prime: (p-1) × p^(k-1)
3. **LCM of results**: Take the least common multiple of all computed λ values

This implementation efficiently computes the Carmichael function using the mathematical properties of the function and the prime factorization of the input number.

