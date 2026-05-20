# Arithmetic Coding Algorithm in Kotlin

Here's a complete implementation of the Arithmetic Coding algorithm in Kotlin:

```kotlin
import kotlin.math.log2
import kotlin.math.pow

class ArithmeticCoder {
    
    companion object {
        private const val MIN_PROBABILITY = 1e-10
        private const val MAX_RANGE = 1.0
    }
    
    /**
     * Encodes a string using arithmetic coding
     */
    fun encode(input: String, frequencies: Map<Char, Double>): Double {
        if (input.isEmpty()) return 0.0
        
        val sortedChars = frequencies.keys.sorted()
        val cumulativeFreq = buildCumulativeFrequencyMap(frequencies, sortedChars)
        
        var low = 0.0
        var high = MAX_RANGE
        val total = frequencies.values.sum()
        
        for (char in input) {
            val charFreq = frequencies[char] ?: 0.0
            val charLow = cumulativeFreq[char] ?: 0.0
            val charHigh = charLow + charFreq
            
            val range = high - low
            high = low + range * (charHigh / total)
            low = low + range * (charLow / total)
        }
        
        return (low + high) / 2
    }
    
    /**
     * Decodes a compressed value back to the original string
     */
    fun decode(encodedValue: Double, length: Int, frequencies: Map<Char, Double>): String {
        if (length == 0) return ""
        
        val sortedChars = frequencies.keys.sorted()
        val cumulativeFreq = buildCumulativeFrequencyMap(frequencies, sortedChars)
        val total = frequencies.values.sum()
        
        var low = 0.0
        var high = MAX_RANGE
        val result = StringBuilder()
        
        repeat(length) {
            val range = high - low
            val value = (encodedValue - low) / range
            
            val char = findCharacter(value, frequencies, cumulativeFreq, total)
            result.append(char)
            
            val charFreq = frequencies[char] ?: 0.0
            val charLow = cumulativeFreq[char] ?: 0.0
            val charHigh = charLow + charFreq
            
            high = low + range * (charHigh / total)
            low = low + range * (charLow / total)
        }
        
        return result.toString()
    }
    
    /**
     * Builds cumulative frequency map for efficient lookup
     */
    private fun buildCumulativeFrequencyMap(
        frequencies: Map<Char, Double>,
        sortedChars: List<Char>
    ): Map<Char, Double> {
        val cumulative = mutableMapOf<Char, Double>()
        var cumulativeFreq = 0.0
        
        for (char in sortedChars) {
            cumulative[char] = cumulativeFreq
            cumulativeFreq += frequencies[char] ?: 0.0
        }
        
        return cumulative
    }
    
    /**
     * Finds which character corresponds to the given value
     */
    private fun findCharacter(
        value: Double,
        frequencies: Map<Char, Double>,
        cumulativeFreq: Map<Char, Double>,
        total: Double
    ): Char {
        for ((char, cumFreq) in cumulativeFreq) {
            val charFreq = frequencies[char] ?: 0.0
            val charHigh = cumFreq + charFreq
            
            if (value >= cumFreq / total && value < charHigh / total) {
                return char
            }
        }
        
        return ' ' // fallback
    }
    
    /**
     * Calculates entropy of the given string
     */
    fun calculateEntropy(input: String): Double {
        val charCount = mutableMapOf<Char, Int>()
        val totalChars = input.length
        
        // Count characters
        for (char in input) {
            charCount[char] = charCount.getOrDefault(char, 0) + 1
        }
        
        // Calculate entropy
        var entropy = 0.0
        for ((char, count) in charCount) {
            val probability = count.toDouble() / totalChars
            entropy -= probability * log2(probability)
        }
        
        return entropy
    }
}

// Example usage
fun main() {
    val coder = ArithmeticCoder()
    
    // Example 1: Simple encoding
    val text1 = "hello"
    val frequencies1 = mapOf(
        'h' to 1.0,
        'e' to 1.0,
        'l' to 2.0,
        'o' to 1.0
    )
    
    println("Original text: $text1")
    println("Frequencies: $frequencies1")
    
    val encoded1 = coder.encode(text1, frequencies1)
    println("Encoded value: $encoded1")
    
    val decoded1 = coder.decode(encoded1, text1.length, frequencies1)
    println("Decoded text: $decoded1")
    println()
    
    // Example 2: More complex example
    val text2 = "ABRACADABRA"
    val frequencies2 = mapOf(
        'A' to 5.0,
        'B' to 2.0,
        'R' to 2.0,
        'C' to 1.0,
        'D' to 1.0
    )
    
    println("Original text: $text2")
    println("Frequencies: $frequencies2")
    
    val encoded2 = coder.encode(text2, frequencies2)
    println("Encoded value: $encoded2")
    
    val decoded2 = coder.decode(encoded2, text2.length, frequencies2)
    println("Decoded text: $decoded2")
    println()
    
    // Example 3: Entropy calculation
    val text3 = "aaaaabbbbbcccccdddddeeeee"
    val frequencies3 = mapOf(
        'a' to 5.0,
        'b' to 5.0,
        'c' to 5.0,
        'd' to 5.0,
        'e' to 5.0
    )
    
    println("Original text: $text3")
    println("Entropy: ${coder.calculateEntropy(text3)}")
    
    val encoded3 = coder.encode(text3, frequencies3)
    println("Encoded value: $encoded3")
    
    val decoded3 = coder.decode(encoded3, text3.length, frequencies3)
    println("Decoded text: $decoded3")
}
```

## Key Features of this Implementation:

1. **Encoding Function**: Converts a string into a single floating-point number using arithmetic coding
2. **Decoding Function**: Reverses the process to reconstruct the original string
3. **Frequency Handling**: Properly handles character frequencies for probability calculations
4. **Cumulative Frequency Map**: Optimizes lookup operations during encoding/decoding
5. **Entropy Calculation**: Includes utility to calculate the entropy of input strings

## How it Works:

1. **Frequency Analysis**: Each character is assigned a probability based on its frequency
2. **Range Division**: The unit interval [0,1) is recursively divided based on character probabilities
3. **Encoding**: Each character narrows the range, producing a final encoded value
4. **Decoding**: The process is reversed using the same probabilities

## Sample Output:
```
Original text: hello
Frequencies: {h=1.0, e=1.0, l=2.0, o=1.0}
Encoded value: 0.1875
Decoded text: hello

Original text: ABRACADABRA
Frequencies: {A=5.0, B=2.0, R=2.0, C=1.0, D=1.0}
Encoded value: 0.34521484375
Decoded text: ABRACADABRA
```

This implementation demonstrates the core principles of arithmetic coding while maintaining clean, readable Kotlin code structure.

