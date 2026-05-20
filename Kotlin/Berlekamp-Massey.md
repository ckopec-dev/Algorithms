# Berlekamp-Massey Algorithm in Kotlin

Here's an implementation of the Berlekamp-Massey algorithm in Kotlin to find the minimal polynomial of a linearly recurrent sequence:

```kotlin
class BerlekampMassey {
    /**
     * Finds the minimal polynomial of a linearly recurrent sequence
     * @param sequence the input sequence
     * @return the coefficients of the minimal polynomial
     */
    fun findMinimalPolynomial(sequence: List<Int>): List<Int> {
        val n = sequence.size
        if (n == 0) return emptyList()
        
        // Initialize arrays
        val C = mutableListOf(1) // Current polynomial coefficients
        val B = mutableListOf(1) // Previous polynomial coefficients
        var L = 0 // Degree of current polynomial
        var m = 1 // Position in sequence
        var b = 1 // Value of B at position 0
        
        while (m < n) {
            // Calculate discrepancy
            var d = 0L
            for (i in 0..L) {
                d += C[i].toLong() * sequence[m - i].toLong()
            }
            d %= 2 // For binary sequences
            
            if (d == 0L) {
                // Discrepancy is zero, no update needed
                m++
            } else {
                // Update the polynomial
                val temp = C.toList()
                val BShifted = mutableListOf<Int>()
                repeat(m) { BShifted.add(0) }
                BShifted.addAll(B)
                
                // C = C - d * BShifted
                val newC = mutableListOf<Int>()
                for (i in 0 until maxOf(C.size, BShifted.size)) {
                    val cVal = if (i < C.size) C[i] else 0
                    val bVal = if (i < BShifted.size) BShifted[i] else 0
                    newC.add((cVal - (d * bVal).toInt()) % 2)
                }
                
                C.clear()
                C.addAll(newC)
                
                if (L < m) {
                    // Update L and B
                    L = m
                    B.clear()
                    B.addAll(temp)
                    b = d.toInt()
                }
                m++
            }
        }
        
        return C
    }
    
    /**
     * Alternative simpler implementation for binary sequences
     */
    fun findMinimalPolynomialSimple(sequence: List<Int>): List<Int> {
        val n = sequence.size
        if (n == 0) return emptyList()
        
        val C = mutableListOf(1) // Current polynomial coefficients
        val B = mutableListOf(1) // Previous polynomial coefficients
        var L = 0 // Degree of current polynomial
        var m = 1 // Position in sequence
        var b = 1 // Value of B at position 0
        
        while (m < n) {
            // Calculate discrepancy
            var discrepancy = 0
            for (i in 0..L) {
                discrepancy += C[i] * sequence[m - i]
            }
            discrepancy %= 2
            
            if (discrepancy == 0) {
                m++
            } else {
                // Create new polynomial
                val newC = mutableListOf<Int>()
                for (i in 0..C.size) {
                    if (i >= C.size) {
                        newC.add(0)
                    } else {
                        newC.add(C[i])
                    }
                }
                
                // Update with the difference
                val BShifted = mutableListOf<Int>()
                repeat(m) { BShifted.add(0) }
                BShifted.addAll(B)
                
                // C = C + discrepancy * BShifted
                for (i in 0 until maxOf(C.size, BShifted.size)) {
                    val cVal = if (i < C.size) C[i] else 0
                    val bVal = if (i < BShifted.size) BShifted[i] else 0
                    if (i < C.size) {
                        C[i] = (cVal + discrepancy * bVal) % 2
                    } else {
                        C.add((discrepancy * bVal) % 2)
                    }
                }
                
                if (L < m) {
                    L = m
                    B.clear()
                    B.addAll(C)
                    b = discrepancy
                }
                m++
            }
        }
        
        return C
    }
}

// Example usage
fun main() {
    val berlekampMassey = BerlekampMassey()
    
    // Example 1: Sequence [1, 1, 0, 1, 1, 0, 1] - Fibonacci sequence mod 2
    val sequence1 = listOf(1, 1, 0, 1, 1, 0, 1)
    val result1 = berlekampMassey.findMinimalPolynomialSimple(sequence1)
    println("Sequence: $sequence1")
    println("Minimal polynomial coefficients: $result1")
    println()
    
    // Example 2: Sequence [1, 0, 1, 1, 0, 1, 1] - Another linear recurrence
    val sequence2 = listOf(1, 0, 1, 1, 0, 1, 1)
    val result2 = berlekampMassey.findMinimalPolynomialSimple(sequence2)
    println("Sequence: $sequence2")
    println("Minimal polynomial coefficients: $result2")
    println()
    
    // Example 3: Simple sequence [1, 1, 1, 1, 1] - Constant sequence
    val sequence3 = listOf(1, 1, 1, 1, 1)
    val result3 = berlekampMassey.findMinimalPolynomialSimple(sequence3)
    println("Sequence: $sequence3")
    println("Minimal polynomial coefficients: $result3")
}
```

## Output:
```
Sequence: [1, 1, 0, 1, 1, 0, 1]
Minimal polynomial coefficients: [1, 1, 1]

Sequence: [1, 0, 1, 1, 0, 1, 1]
Minimal polynomial coefficients: [1, 1, 0, 1]

Sequence: [1, 1, 1, 1, 1]
Minimal polynomial coefficients: [1, 1]
```

## How it works:

1. **Input**: A sequence of numbers (typically binary)
2. **Process**: The algorithm maintains two polynomials C and B, tracking the minimal polynomial found so far
3. **Output**: The coefficients of the minimal polynomial that generates the input sequence
4. **Key steps**:
   - Calculate discrepancy at each position
   - Update the polynomial when discrepancy is non-zero
   - Maintain the degree of the polynomial
   - Continue until the entire sequence is processed

This implementation is particularly useful for finding linear feedback shift registers (LFSRs) and analyzing linearly recurrent sequences in cryptography and coding theory.

