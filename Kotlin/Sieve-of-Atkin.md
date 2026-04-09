# Sieve of Atkin Algorithm in Kotlin

The Sieve of Atkin is a modern algorithm for finding all prime numbers up to a specified integer, which is more efficient than the classic Sieve of Eratosthenes for large numbers.

```kotlin
fun sieveOfAtkin(limit: Int): List<Int> {
    if (limit < 2) return emptyList()
    
    // Create a boolean array and initialize all entries as false
    val isPrime = BooleanArray(limit + 1) { false }
    
    // Mark 2 and 3 as prime
    if (limit >= 2) isPrime[2] = true
    if (limit >= 3) isPrime[3] = true
    
    // Apply the Sieve of Atkin algorithm
    for (x in 1..kotlin.math.sqrt(limit.toDouble()).toInt()) {
        for (y in 1..kotlin.math.sqrt(limit.toDouble()).toInt()) {
            val n = 4 * x * x + y * y
            
            if (n <= limit && (n % 12 == 1 || n % 12 == 5)) {
                isPrime[n] = !isPrime[n]
            }
            
            val m = 3 * x * x + y * y
            if (m <= limit && m % 12 == 7) {
                isPrime[m] = !isPrime[m]
            }
            
            val p = 3 * x * x - y * y
            if (x > y && p <= limit && p % 12 == 11) {
                isPrime[p] = !isPrime[p]
            }
        }
    }
    
    // Mark multiples of squares as non-prime
    for (i in 5..kotlin.math.sqrt(limit.toDouble()).toInt()) {
        if (isPrime[i]) {
            var j = i * i
            while (j <= limit) {
                isPrime[j] = false
                j += i * i
            }
        }
    }
    
    // Collect all prime numbers
    val primes = mutableListOf<Int>()
    for (i in 2..limit) {
        if (isPrime[i]) {
            primes.add(i)
        }
    }
    
    return primes
}

// Example usage
fun main() {
    val limit = 30
    val primes = sieveOfAtkin(limit)
    
    println("Prime numbers up to $limit:")
    println(primes)
    
    // Print in a formatted way
    println("\nFormatted output:")
    primes.forEachIndexed { index, prime ->
        print("$prime ")
        if ((index + 1) % 10 == 0) println()
    }
    println()
}
```

## How it works:

1. **Initialization**: Create a boolean array to track prime numbers, marking 2 and 3 as prime
2. **Main Sieve Process**: 
   - For each pair of numbers (x,y), calculate three formulas
   - Mark numbers that satisfy specific modulo conditions as potentially prime
   - Toggle the prime status for numbers that meet criteria
3. **Square Removal**: Remove multiples of squares of primes
4. **Collection**: Gather all numbers marked as prime

## Example Output:
```
Prime numbers up to 30:
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

Formatted output:
2 3 5 7 11 13 17 19 23 29 
```

## Time Complexity:
- **Time**: O(n / log log n) - faster than Sieve of Eratosthenes for large numbers
- **Space**: O(n) - for the boolean array

This implementation efficiently finds all prime numbers up to the given limit using the optimized Sieve of Atkin algorithm.

