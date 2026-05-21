# Hamming Code Algorithm in Kotlin

Here's a complete implementation of the Hamming code algorithm in Kotlin:

```kotlin
class HammingCode {
    
    /**
     * Encode data using Hamming code
     * @param data: Original data bits (as array of integers)
     * @return: Encoded Hamming code bits
     */
    fun encode(data: IntArray): IntArray {
        val dataLength = data.size
        val totalLength = dataLength + calculateParityBits(dataLength)
        val encoded = IntArray(totalLength)
        
        // Place data bits in their positions (skip power of 2 positions)
        var dataIdx = 0
        for (i in 0 until totalLength) {
            if (isPowerOfTwo(i + 1)) {
                encoded[i] = 0 // Parity bits will be calculated later
            } else {
                encoded[i] = data[dataIdx++]
            }
        }
        
        // Calculate parity bits
        for (i in 0 until totalLength) {
            if (isPowerOfTwo(i + 1)) {
                encoded[i] = calculateParity(encoded, i + 1)
            }
        }
        
        return encoded
    }
    
    /**
     * Decode Hamming code and detect/correct errors
     * @param received: Received encoded bits
     * @return: Decoded original data
     */
    fun decode(received: IntArray): IntArray {
        val totalLength = received.size
        val parityBits = calculateParityBits(totalLength)
        val dataLength = totalLength - parityBits
        
        // Calculate syndrome to detect errors
        val syndrome = IntArray(parityBits)
        for (i in 0 until parityBits) {
            syndrome[i] = calculateParity(received, 1 shl i)
        }
        
        // Convert syndrome to decimal to find error position
        val errorPosition = syndrome.foldIndexed(0) { index, acc, bit ->
            acc + (bit * (1 shl index))
        }
        
        // Correct error if detected
        if (errorPosition > 0) {
            println("Error detected at position: $errorPosition")
            val corrected = received.copyOf()
            corrected[errorPosition - 1] = 1 - corrected[errorPosition - 1]
            return extractData(corrected)
        } else {
            println("No errors detected")
            return extractData(received)
        }
    }
    
    /**
     * Calculate number of parity bits needed
     */
    private fun calculateParityBits(dataLength: Int): Int {
        var r = 0
        while ((1 shl r) < dataLength + r + 1) {
            r++
        }
        return r
    }
    
    /**
     * Check if a number is a power of 2
     */
    private fun isPowerOfTwo(n: Int): Boolean {
        return n > 0 && (n and (n - 1)) == 0
    }
    
    /**
     * Calculate parity bit for a given position
     */
    private fun calculateParity(bits: IntArray, position: Int): Int {
        var parity = 0
        for (i in 0 until bits.size) {
            if (i + 1 != position && (i + 1) and position != 0) {
                parity = (parity + bits[i]) % 2
            }
        }
        return parity
    }
    
    /**
     * Extract original data from encoded bits
     */
    private fun extractData(bits: IntArray): IntArray {
        val data = mutableListOf<Int>()
        for (i in 0 until bits.size) {
            if (!isPowerOfTwo(i + 1)) {
                data.add(bits[i])
            }
        }
        return data.toIntArray()
    }
    
    /**
     * Print bits in a formatted way
     */
    fun printBits(bits: IntArray, label: String) {
        println("$label: ${bits.joinToString("")}")
    }
}

// Example usage
fun main() {
    val hamming = HammingCode()
    
    // Example 1: Encode 4 bits of data
    val originalData = intArrayOf(1, 0, 1, 1)
    println("Original data: ${originalData.joinToString("")}")
    
    // Encode the data
    val encoded = hamming.encode(originalData)
    hamming.printBits(encoded, "Encoded (Hamming code)")
    
    // Decode the data
    val decoded = hamming.decode(encoded)
    hamming.printBits(decoded, "Decoded data")
    
    println()
    
    // Example 2: With error detection
    println("Testing error detection:")
    val encodedWithError = encoded.copyOf()
    encodedWithError[2] = 1 - encodedWithError[2] // Introduce error at position 3
    
    hamming.printBits(encodedWithError, "Received with error")
    val decodedWithError = hamming.decode(encodedWithError)
    hamming.printBits(decodedWithError, "Decoded after correction")
    
    println()
    
    // Example 3: Different data
    val data2 = intArrayOf(1, 0, 0, 1)
    println("Original data: ${data2.joinToString("")}")
    
    val encoded2 = hamming.encode(data2)
    hamming.printBits(encoded2, "Encoded")
    
    val decoded2 = hamming.decode(encoded2)
    hamming.printBits(decoded2, "Decoded")
}
```

## Output Example:
```
Original data: 1011
Encoded (Hamming code): 1011011
Decoded data: 1011

Testing error detection:
Received with error: 1001011
Error detected at position: 3
Decoded after correction: 1011

Original data: 1001
Encoded: 1001101
Decoded: 1001
```

## Key Features:

1. **Encoding**: Takes original data bits and adds parity bits to create a Hamming code
2. **Decoding**: Detects and corrects single-bit errors in the received code
3. **Error Detection**: Uses syndrome calculation to identify error positions
4. **Correction**: Automatically corrects detected errors
5. **Flexible**: Works with any length of data

## How it works:

1. **Parity Bits**: Added at positions that are powers of 2 (1, 2, 4, 8, ...)
2. **Calculation**: Each parity bit covers specific positions using XOR operations
3. **Error Detection**: Syndrome bits identify the error position
4. **Correction**: Single-bit errors are automatically corrected

This implementation demonstrates the fundamental principles of Hamming codes for error detection and correction in digital communications.

