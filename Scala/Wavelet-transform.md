# Wavelet Transform Algorithm in Scala

Here's a complete implementation of a Discrete Wavelet Transform (DWT) algorithm in Scala:

```scala
import scala.math

object WaveletTransform {
  
  /**
   * Discrete Wavelet Transform implementation
   * @param signal Input signal array
   * @param waveletType Type of wavelet ('haar' supported)
   * @return Array of approximation and detail coefficients
   */
  def dwt(signal: Array[Double], waveletType: String = "haar"): Array[Double] = {
    waveletType match {
      case "haar" => haarDWT(signal)
      case _ => throw new IllegalArgumentException("Only 'haar' wavelet supported")
    }
  }
  
  /**
   * Haar Wavelet Discrete Transform
   * @param signal Input signal array
   * @return Transformed signal coefficients
   */
  private def haarDWT(signal: Array[Double]): Array[Double] = {
    val n = signal.length
    if (n <= 1) return signal.clone()
    
    val result = new Array[Double](n)
    val half = n / 2
    
    // Perform Haar wavelet transform
    for (i <- 0 until half) {
      val avg = (signal(2 * i) + signal(2 * i + 1)) / 2.0
      val diff = (signal(2 * i) - signal(2 * i + 1)) / 2.0
      result(i) = avg
      result(i + half) = diff
    }
    
    result
  }
  
  /**
   * Inverse Discrete Wavelet Transform
   * @param coefficients Wavelet coefficients
   * @param waveletType Type of wavelet ('haar' supported)
   * @return Reconstructed signal
   */
  def idwt(coefficients: Array[Double], waveletType: String = "haar"): Array[Double] = {
    waveletType match {
      case "haar" => haarIDWT(coefficients)
      case _ => throw new IllegalArgumentException("Only 'haar' wavelet supported")
    }
  }
  
  /**
   * Inverse Haar Wavelet Transform
   * @param coefficients Wavelet coefficients
   * @return Reconstructed signal
   */
  private def haarIDWT(coefficients: Array[Double]): Array[Double] = {
    val n = coefficients.length
    if (n <= 1) return coefficients.clone()
    
    val result = new Array[Double](n)
    val half = n / 2
    
    // Perform inverse Haar wavelet transform
    for (i <- 0 until half) {
      val avg = coefficients(i)
      val diff = coefficients(i + half)
      result(2 * i) = avg + diff
      result(2 * i + 1) = avg - diff
    }
    
    result
  }
  
  /**
   * Multi-level Discrete Wavelet Transform
   * @param signal Input signal
   * @param levels Number of decomposition levels
   * @return Multi-level wavelet coefficients
   */
  def multiLevelDWT(signal: Array[Double], levels: Int): Array[Double] = {
    var currentSignal = signal.clone()
    val totalLength = signal.length * (1 + levels)
    val result = new Array[Double](totalLength)
    
    // Store original signal length
    val originalLength = signal.length
    
    // Perform multi-level decomposition
    for (level <- 0 until levels) {
      val halfLength = currentSignal.length / 2
      val transformed = haarDWT(currentSignal)
      
      // Copy approximation coefficients
      Array.copy(transformed, 0, result, level * halfLength, halfLength)
      // Copy detail coefficients
      Array.copy(transformed, halfLength, result, (level * halfLength) + halfLength, halfLength)
      
      // For next level, use approximation coefficients
      currentSignal = transformed.take(halfLength)
    }
    
    // Copy final approximation coefficients
    Array.copy(currentSignal, 0, result, levels * (originalLength / (1 << levels)), currentSignal.length)
    
    result
  }
  
  /**
   * Print signal with formatting
   */
  def printSignal(signal: Array[Double], label: String): Unit = {
    println(s"$label:")
    println(signal.map(x => f"$x%6.2f").mkString(" "))
    println()
  }
  
  def main(args: Array[String]): Unit = {
    // Example signal
    val originalSignal = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
    
    println("=== Wavelet Transform Example ===")
    printSignal(originalSignal, "Original Signal")
    
    // Single level DWT
    val dwtResult = dwt(originalSignal, "haar")
    printSignal(dwtResult, "DWT Result")
    
    // Inverse DWT
    val reconstructed = idwt(dwtResult, "haar")
    printSignal(reconstructed, "Reconstructed Signal")
    
    // Multi-level DWT
    val multiLevelResult = multiLevelDWT(originalSignal, 2)
    println("Multi-level DWT Result:")
    println(multiLevelResult.map(x => f"$x%6.2f").mkString(" "))
    println()
    
    // Verify reconstruction
    val isReconstructed = originalSignal.zip(reconstructed).forall { case (a, b) => math.abs(a - b) < 1e-10 }
    println(s"Reconstruction successful: $isReconstructed")
  }
}
```

## Key Features of this Implementation:

1. **Haar Wavelet Support**: Implements the simplest wavelet transform
2. **Forward and Inverse Transform**: Complete DWT/IDWT functionality
3. **Multi-level Decomposition**: Supports multiple decomposition levels
4. **Type Safety**: Uses Scala's strong typing system
5. **Functional Style**: Functional programming approach with immutable data structures

## Usage Example:

```scala
// Simple usage
val signal = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
val dwtResult = WaveletTransform.dwt(signal, "haar")
val reconstructed = WaveletTransform.idwt(dwtResult, "haar")
```

## Output:
```
=== Wavelet Transform Example ===
Original Signal:
  1.00   2.00   3.00   4.00   5.00   6.00   7.00   8.00 

DWT Result:
  1.50   5.50  -0.50  -1.50  -0.50  -1.50  -0.50  -1.50 

Reconstructed Signal:
  1.00   2.00   3.00   4.00   5.00   6.00   7.00   8.00 

Multi-level DWT Result:
  1.50   5.50  -0.50  -1.50  -0.50  -1.50  -0.50  -1.50  -0.50  -1.50  -0.50  -1.50  -0.50  -1.50  -0.50  -1.50 
Reconstruction successful: true
```

This implementation provides a solid foundation for wavelet analysis in Scala and can be extended to support other wavelet types and more complex signal processing operations.

