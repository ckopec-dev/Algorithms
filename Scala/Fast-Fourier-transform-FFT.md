# Fast Fourier Transform (FFT) in Scala

Here's an implementation of the Cooley-Tukey FFT algorithm in Scala:

```scala
import scala.math.{Pi, cos, sin}

object FFT {
  
  /**
   * Fast Fourier Transform implementation using Cooley-Tukey algorithm
   * @param x Array of complex numbers (real parts)
   * @param y Array of complex numbers (imaginary parts)
   * @return Array of complex numbers representing the FFT
   */
  def fft(x: Array[Double], y: Array[Double]): Array[(Double, Double)] = {
    val n = x.length
    
    // Check if input size is a power of 2
    if ((n & (n - 1)) != 0) {
      throw new IllegalArgumentException("Input size must be a power of 2")
    }
    
    // Bit-reversal permutation
    val bitReversedX = new Array[Double](n)
    val bitReversedY = new Array[Double](n)
    
    for (i <- 0 until n) {
      val reversedIndex = bitReverse(i, n)
      bitReversedX(reversedIndex) = x(i)
      bitReversedY(reversedIndex) = y(i)
    }
    
    // FFT computation
    val resultX = bitReversedX.clone()
    val resultY = bitReversedY.clone()
    
    var step = 2
    while (step <= n) {
      val angle = -2 * Pi / step
      val wStepReal = cos(angle)
      val wStepImag = sin(angle)
      
      var i = 0
      while (i < n) {
        var wReal = 1.0
        var wImag = 0.0
        
        var j = i
        val halfStep = step / 2
        while (j < i + halfStep) {
          val uReal = resultX(j)
          val uImag = resultY(j)
          
          val vReal = resultX(j + halfStep) * wReal - resultY(j + halfStep) * wImag
          val vImag = resultX(j + halfStep) * wImag + resultY(j + halfStep) * wReal
          
          resultX(j) = uReal + vReal
          resultY(j) = uImag + vImag
          
          resultX(j + halfStep) = uReal - vReal
          resultY(j + halfStep) = uImag - vImag
          
          val tempReal = wReal * wStepReal - wImag * wStepImag
          wImag = wReal * wStepImag + wImag * wStepReal
          wReal = tempReal
          
          j += 1
        }
        i += step
      }
      step *= 2
    }
    
    // Return complex results
    (0 until n).map(i => (resultX(i), resultY(i))).toArray
  }
  
  /**
   * Bit-reversal function
   */
  def bitReverse(x: Int, bits: Int): Int = {
    var result = 0
    var i = 0
    while (i < bits) {
      result = (result << 1) | (x & 1)
      x >>= 1
      i += 1
    }
    result
  }
  
  /**
   * Simple example usage
   */
  def main(args: Array[String]): Unit = {
    // Example: 8-point FFT
    val real = Array(1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    val imag = Array(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    
    println("Input signal:")
    (0 until real.length).foreach(i => 
      println(s"x[$i] = ${real(i)}, y[$i] = ${imag(i)}")
    )
    
    val result = fft(real, imag)
    
    println("\nFFT result:")
    result.zipWithIndex.foreach { case ((re, im), i) =>
      println(s"X[$i] = $re + ${im}i")
    }
    
    // Example with a simple sine wave
    println("\n--- Sine wave example ---")
    val sineReal = Array(0.0, 1.0, 0.0, -1.0, 0.0, 1.0, 0.0, -1.0)
    val sineImag = Array(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    
    val sineResult = fft(sineReal, sineImag)
    
    println("Sine wave FFT result:")
    sineResult.zipWithIndex.foreach { case ((re, im), i) =>
      val magnitude = math.sqrt(re * re + im * im)
      println(s"X[$i] = $re + ${im}i (magnitude: $magnitude)")
    }
  }
}
```

## Key Features of this Implementation:

1. **Cooley-Tukey Algorithm**: Uses the standard FFT algorithm with bit-reversal
2. **Complex Numbers**: Handles both real and imaginary components
3. **Bit-reversal**: Implements bit-reversal permutation for efficient computation
4. **Input Validation**: Checks that input size is a power of 2
5. **Example Usage**: Includes test cases with simple signals

## Usage Example:

```scala
// Simple usage
val real = Array(1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
val imag = Array(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
val result = FFT.fft(real, imag)
```

This implementation provides a working FFT that can be used for signal processing, spectral analysis, and other applications requiring frequency domain analysis.

