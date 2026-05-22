# Egyptian Multiplication Algorithm in Kotlin

Egyptian multiplication (also known as binary multiplication or Russian peasant multiplication) is an ancient algorithm for multiplying two numbers using repeated doubling and addition.

## Algorithm Explanation

The algorithm works by:
1. Repeatedly halving one number (ignoring remainders)
2. Repeatedly doubling the other number
3. Adding the doubled numbers when the halved number is odd

## Kotlin Implementation

```kotlin
fun egyptianMultiplication(a: Int, b: Int): Int {
    if (a == 0 || b == 0) return 0
    
    var x = a
    var y = b
    var result = 0
    
    while (x > 0) {
        // If x is odd, add y to result
        if (x % 2 == 1) {
            result += y
        }
        
        // Halve x (integer division)
        x = x / 2
        
        // Double y
        y = y * 2
    }
    
    return result
}

// Alternative implementation with bit operations
fun egyptianMultiplicationBitwise(a: Int, b: Int): Int {
    if (a == 0 || b == 0) return 0
    
    var x = a
    var y = b
    var result = 0
    
    while (x > 0) {
        // If x is odd (least significant bit is 1)
        if (x and 1 == 1) {
            result += y
        }
        
        // Right shift x (equivalent to x / 2)
        x = x shr 1
        
        // Left shift y (equivalent to y * 2)
        y = y shl 1
    }
    
    return result
}

// Example usage
fun main() {
    val num1 = 17
    val num2 = 23
    
    val result1 = egyptianMultiplication(num1, num2)
    val result2 = egyptianMultiplicationBitwise(num1, num2)
    
    println("Egyptian multiplication of $num1 and $num2:")
    println("Result (standard): $result1")
    println("Result (bitwise): $result2")
    println("Standard multiplication: ${num1 * num2}")
    
    // Step-by-step trace for 17 × 23
    println("\nStep-by-step trace for 17 × 23:")
    println("17 × 23")
    println("17 is odd → add 23 to result")
    println("8 × 46")
    println("8 is even → skip")
    println("4 × 92")
    println("4 is even → skip")
    println("2 × 184")
    println("2 is even → skip")
    println("1 × 368")
    println("1 is odd → add 368 to result")
    println("Result: 23 + 368 = 391")
}
```

## Example Output

```
Egyptian multiplication of 17 and 23:
Result (standard): 391
Result (bitwise): 391
Standard multiplication: 391

Step-by-step trace for 17 × 23:
17 × 23
17 is odd → add 23 to result
8 × 46
8 is even → skip
4 × 92
4 is even → skip
2 × 184
2 is even → skip
1 × 368
1 is odd → add 368 to result
Result: 23 + 368 = 391
```

## How It Works

For 17 × 23:
- 17 = 16 + 1 = 2⁴ + 2⁰
- 17 × 23 = (16 + 1) × 23 = 16 × 23 + 1 × 23
- 16 × 23 = 23 × 2⁴ = 23 × 16 = 368
- 1 × 23 = 23
- Total = 368 + 23 = 391

This algorithm is essentially performing binary multiplication using only addition, subtraction, and bit shifting operations.

