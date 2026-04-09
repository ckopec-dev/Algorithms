# Welch's Method Algorithm in Scala

Welch's method is a technique for estimating the power spectral density (PSD) of a signal by averaging modified periodograms. Here's an implementation in Scala:

```scala
import scala.math._
import scala.collection.mutable.ArrayBuffer

object WelchMethod {
  
  /**
   * Welch's Method for Power Spectral Density estimation
   * @param x Input signal
   * @param windowSize Window size for segmenting the signal
   * @param overlap Overlap between consecutive windows (0 to 1)
   * @param windowType Type of window function to use
   * @return Array of frequency bins and corresponding PSD values
   */
  def welchMethod(x: Array[Double], 
                 windowSize: Int = 256, 
                 overlap: Double = 0.5, 
                 windowType: String = "hamming"): (Array[Double], Array[Double]) = {
    
    val n = x.length
    val stepSize = (windowSize * (1 - overlap)).toInt
    val nSegments = (n - windowSize) / stepSize + 1
    
    // Create window function
    val window = createWindow(windowSize, windowType)
    
    // Initialize arrays for storing periodograms
    val periodograms = ArrayBuffer[Array[Double]]()
    
    // Segment the signal and compute periodograms
    for (i <- 0 until nSegments) {
      val start = i * stepSize
      val end = start + windowSize
      
      // Check if we have enough data
      if (end <= n) {
        val segment = x.slice(start, end)
        val windowedSegment = window.zip(segment).map { case (w, s) => w * s }
        val periodogram = computePeriodogram(windowedSegment)
        periodograms += periodogram
      }
    }
    
    // Average all periodograms
    val averagedPeriodogram = averagePeriodograms(periodograms.toArray)
    
    // Generate frequency bins
    val frequencies = (0 until windowSize / 2).map(i => 
      (i.toDouble / windowSize) * 0.5).toArray
    
    (frequencies, averagedPeriodogram)
  }
  
  /**
   * Create window function
   */
  def createWindow(size: Int, windowType: String): Array[Double] = {
    windowType match {
      case "hamming" => 
        val alpha = 0.54
        val beta = 0.46
        (0 until size).map(i => alpha - beta * cos(2 * Pi * i / (size - 1))).toArray
      case "hanning" => 
        (0 until size).map(i => 0.5 * (1 - cos(2 * Pi * i / (size - 1)))).toArray
      case "blackman" => 
        val alpha = 0.16
        val a0 = (1 - alpha) / 2
        val a1 = 0.5
        val a2 = alpha / 2
        (0 until size).map(i => 
          a0 - a1 * cos(2 * Pi * i / (size - 1)) + a2 * cos(4 * Pi * i / (size - 1))
        ).toArray
      case _ => 
        Array.fill(size)(1.0) // Rectangular window
    }
  }
  
  /**
   * Compute periodogram using FFT
   */
  def computePeriodogram(x: Array[Double]): Array[Double] = {
    val n = x.length
    val fftResult = fft(x)
    val magnitudeSquared = fftResult.map(c => c.real * c.real + c.imag * c.imag)
    
    // Return only the first half (positive frequencies)
    magnitudeSquared.take(n / 2)
  }
  
  /**
   * Simple FFT implementation (Cooley-Tukey algorithm)
   */
  def fft(x: Array[Double]): Array[Complex] = {
    val n = x.length
    if (n <= 1) return Array(new Complex(x(0), 0))
    
    val even = fft(x.zipWithIndex.filter(_._2 % 2 == 0).map(_._1))
    val odd = fft(x.zipWithIndex.filter(_._2 % 2 == 1).map(_._1))
    
    val result = new Array[Complex](n)
    for (i <- 0 until n / 2) {
      val angle = -2 * Pi * i / n
      val w = new Complex(cos(angle), sin(angle))
      result(i) = even(i) + w * odd(i)
      result(i + n / 2) = even(i) - w * odd(i)
    }
    result
  }
  
  /**
   * Simple Complex number class
   */
  case class Complex(real: Double, imag: Double) {
    def +(other: Complex): Complex = new Complex(real + other.real, imag + other.imag)
    def -(other: Complex): Complex = new Complex(real - other.real, imag - other.imag)
    def *(other: Complex): Complex = new Complex(
      real * other.real - imag * other.imag,
      real * other.imag + imag * other.real
    )
    def *(scalar: Double): Complex = new Complex(real * scalar, imag * scalar)
  }
  
  /**
   * Average multiple periodograms
   */
  def averagePeriodograms(periodograms: Array[Array[Double]]): Array[Double] = {
    if (periodograms.isEmpty) return Array()
    
    val nPeriodograms = periodograms.length
    val nBins = periodograms(0).length
    val averaged = new Array[Double](nBins)
    
    for (i <- 0 until nBins) {
      var sum = 0.0
      for (j <- 0 until nPeriodograms) {
        sum += periodograms(j)(i)
      }
      averaged(i) = sum / nPeriodograms
    }
    
    averaged
  }
  
  // Example usage
  def main(args: Array[String]): Unit = {
    // Generate test signal (sum of sine waves)
    val n = 1024
    val t = (0 until n).map(_.toDouble / n)
    val signal = t.map(i => 
      0.5 * sin(2 * Pi * 10 * i) + 
      0.3 * sin(2 * Pi * 25 * i) + 
      0.2 * sin(2 * Pi * 50 * i) + 
      0.1 * scala.util.Random.nextGaussian()
    ).toArray
    
    // Apply Welch's method
    val (frequencies, psd) = welchMethod(signal, windowSize = 256, overlap = 0.5)
    
    // Print results
    println("Frequency (Hz)\tPSD")
    println("================")
    for (i <- 0 until math.min(50, frequencies.length)) {
      printf("%.3f\t\t%.6f%n", frequencies(i), psd(i))
    }
  }
}
```

## Key Features of this Implementation:

1. **Signal Segmentation**: Divides input signal into overlapping segments
2. **Windowing**: Applies window functions (Hamming, Hanning, Blackman) to reduce spectral leakage
3. **FFT-based Periodogram**: Computes periodogram for each segment using FFT
4. **Averaging**: Averages all periodograms to reduce variance
5. **Flexible Parameters**: Configurable window size, overlap, and window type

## Usage Example:

```scala
// Create a test signal
val signal = Array.fill(1024)(scala.util.Random.nextGaussian())

// Apply Welch's method
val (frequencies, psd) = WelchMethod.welchMethod(signal, windowSize = 256, overlap = 0.5)

// The result contains frequency bins and corresponding power spectral density values
```

This implementation provides a practical way to estimate power spectral density using Welch's method, which is particularly useful for noisy signals where traditional FFT methods might produce unreliable results.

