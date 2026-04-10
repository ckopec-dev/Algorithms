```kotlin
fun fastInverseSquareRoot(number: Float): Float {
    val i = number.toRawBits()
    val y = (0x5f3759df - (i shr 1)).toFloat()
    
    // Newton-Raphson iteration for better accuracy
    val y2 = y * y
    return y * (1.5f - 0.5f * number * y2)
}

// Alternative implementation with more iterations for higher accuracy
fun fastInverseSquareRootAccurate(number: Float): Float {
    val i = number.toRawBits()
    var y = (0x5f3759df - (i shr 1)).toFloat()
    
    // Three Newton-Raphson iterations
    val y2 = y * y
    y = y * (1.5f - 0.5f * number * y2)
    
    val y3 = y * y
    y = y * (1.5f - 0.5f * number * y3)
    
    val y4 = y * y
    y = y * (1.5f - 0.5f * number * y4)
    
    return y
}

// Example usage
fun main() {
    val testNumber = 16.0f
    
    val result1 = fastInverseSquareRoot(testNumber)
    val result2 = fastInverseSquareRootAccurate(testNumber)
    
    println("Number: $testNumber")
    println("Fast inverse square root (1 iteration): $result1")
    println("Fast inverse square root (3 iterations): $result2")
    println("Actual inverse square root: ${1.0f / kotlin.math.sqrt(testNumber)}")
}
```

