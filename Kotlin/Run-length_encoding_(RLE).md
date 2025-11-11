# Run-Length Encoding (RLE) in Kotlin

Run-length encoding is a simple form of data compression where consecutive identical elements are replaced by a single element and a count.

## Implementation

```kotlin
fun encode(input: String): String {
    if (input.isEmpty()) return ""
    
    val result = StringBuilder()
    var currentChar = input[0]
    var count = 1
    
    for (i in 1 until input.length) {
        if (input[i] == currentChar) {
            count++
        } else {
            result.append(count).append(currentChar)
            currentChar = input[i]
            count = 1
        }
    }
    
    // Don't forget the last group
    result.append(count).append(currentChar)
    
    return result.toString()
}

fun decode(input: String): String {
    val result = StringBuilder()
    
    var i = 0
    while (i < input.length) {
        // Read the count (it might be multiple digits)
        var count = 0
        while (i < input.length && input[i].isDigit()) {
            count = count * 10 + (input[i] - '0')
            i++
        }
        
        // Read the character
        if (i < input.length) {
            val char = input[i]
            repeat(count) {
                result.append(char)
            }
            i++
        }
    }
    
    return result.toString()
}

// Example usage
fun main() {
    val original = "AAAABBBCCDAA"
    println("Original: $original")
    
    val encoded = encode(original)
    println("Encoded: $encoded")
    
    val decoded = decode(encoded)
    println("Decoded: $decoded")
    
    // Test with more examples
    val testCases = listOf(
        "A",
        "AA",
        "AAA",
        "AABBCC",
        "AABBBCCCCDD",
        ""
    )
    
    println("\nTest cases:")
    testCases.forEach { test ->
        val encoded = encode(test)
        val decoded = decode(encoded)
        println("Original: '$test' -> Encoded: '$encoded' -> Decoded: '$decoded'")
    }
}
```

## Output

```
Original: AAAABBBCCDAA
Encoded: 4A3B2C1D2A
Decoded: AAAABBBCCDAA

Test cases:
Original: 'A' -> Encoded: '1A' -> Decoded: 'A'
Original: 'AA' -> Encoded: '2A' -> Decoded: 'AA'
Original: 'AAA' -> Encoded: '3A' -> Decoded: 'AAA'
Original: 'AABBCC' -> Encoded: '2A2B2C' -> Decoded: 'AABBCC'
Original: 'AABBBCCCCDD' -> Encoded: '2A3B4C2D' -> Decoded: 'AABBBCCCCDD'
Original: '' -> Encoded: '' -> Decoded: ''
```

## How it works

1. **Encoding**: 
   - Traverse the input string
   - Count consecutive identical characters
   - Output the count followed by the character
   - Handle multi-digit counts properly

2. **Decoding**:
   - Parse numbers (which might be multi-digit)
   - Read the following character
   - Repeat the character that many times
   - Continue until the entire encoded string is processed

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the input string
- **Space Complexity**: O(n) for the result string

This implementation handles edge cases like empty strings and multi-digit counts, making it robust for practical use.

