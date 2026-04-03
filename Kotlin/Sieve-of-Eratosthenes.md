# Sieve of Eratosthenes in Kotlin

```kotlin
fun sieveOfEratosthenes(n: Int): List<Int> {
    if (n < 2) return emptyList()
    
    // Create a boolean array and initialize all entries as true
    val isPrime = BooleanArray(n + 1) { true }
    isPrime[0] = isPrime[1] = false // 0 and 1 are not prime numbers
    
    // Start with the first prime number, 2
    var p = 2
    while (p * p <= n) {
        // If isPrime[p] is not changed, then it's a prime
        if (isPrime[p]) {
            // Update all multiples of p
            for (i in p * p..n step p) {
                isPrime[i] = false
            }
        }
        p++
    }
    
    // Collect all prime numbers
    val primes = mutableListOf<Int>()
    for (i in 2..n) {
        if (isPrime[i]) {
            primes.add(i)
        }
    }
    
    return primes
}

// Example usage
fun main() {
    val limit = 30
    val primes = sieveOfEratosthenes(limit)
    println("Prime numbers up to $limit:")
    println(primes)
    
    // Output: Prime numbers up to 30:
    // [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
}
```

## How it works:

1. **Initialize**: Create a boolean array of size n+1, marking all values as `true`
2. **Mark non-primes**: Starting from 2, mark all multiples of each prime number as `false`
3. **Optimization**: Only check up to √n since larger factors would have been found already
4. **Collect results**: Gather all numbers that remain marked as `true`

## Time Complexity: O(n log log n)
## Space Complexity: O(n)

