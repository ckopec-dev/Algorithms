# Otsu's Method Implementation in Scala

Otsu's method is a popular algorithm for automatic threshold selection in image processing. Here's a complete implementation in Scala:

```scala
object OtsuMethod {
  
  /**
   * Calculates the optimal threshold using Otsu's method
   * @param histogram Array representing the histogram of pixel intensities
   * @return Optimal threshold value
   */
  def otsuThreshold(histogram: Array[Int]): Int = {
    val totalPixels = histogram.sum
    var bestThreshold = 0
    var bestVariance = Double.MinValue
    
    // Calculate probabilities for each intensity level
    val probabilities = histogram.map(_.toDouble / totalPixels)
    
    // Calculate cumulative probabilities and means
    val cumulativeProb = new Array[Double](256)
    val cumulativeMean = new Array[Double](256)
    
    cumulativeProb(0) = probabilities(0)
    cumulativeMean(0) = 0.0
    
    for (i <- 1 until 256) {
      cumulativeProb(i) = cumulativeProb(i - 1) + probabilities(i)
      cumulativeMean(i) = cumulativeMean(i - 1) + i * probabilities(i)
    }
    
    // Find optimal threshold
    for (threshold <- 0 until 255) {
      val prob1 = cumulativeProb(threshold)
      val prob2 = 1.0 - prob1
      
      if (prob1 > 0 && prob2 > 0) {
        val mean1 = cumulativeMean(threshold) / prob1
        val mean2 = (cumulativeMean(255) - cumulativeMean(threshold)) / prob2
        
        // Between-class variance
        val variance = prob1 * prob2 * math.pow(mean1 - mean2, 2)
        
        if (variance > bestVariance) {
          bestVariance = variance
          bestThreshold = threshold
        }
      }
    }
    
    bestThreshold
  }
  
  /**
   * Alternative implementation using a more direct approach
   */
  def otsuThresholdSimple(histogram: Array[Int]): Int = {
    val totalPixels = histogram.sum
    var bestThreshold = 0
    var bestVariance = Double.MinValue
    
    var sumB = 0.0
    var weight1 = 0.0
    var weight2 = 0.0
    var mean1 = 0.0
    var mean2 = 0.0
    
    for (threshold <- 0 until 255) {
      weight1 += histogram(threshold)
      weight2 = totalPixels - weight1
      
      if (weight1 != 0 && weight2 != 0) {
        sumB += threshold * histogram(threshold)
        mean1 = sumB / weight1
        mean2 = (sumB - mean1 * weight1) / weight2
        
        val variance = weight1 * weight2 * math.pow(mean1 - mean2, 2)
        
        if (variance > bestVariance) {
          bestVariance = variance
          bestThreshold = threshold
        }
      }
    }
    
    bestThreshold
  }
  
  /**
   * Example usage with sample data
   */
  def main(args: Array[String]): Unit = {
    // Sample histogram data (256 bins for 8-bit grayscale)
    val sampleHistogram = Array(
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  // Low intensity values
      5, 10, 15, 20, 25, 30, 35, 40, 45, 50,
      100, 150, 200, 250, 300, 350, 400, 450, 500, 550,
      600, 550, 500, 450, 400, 350, 300, 250, 200, 150,
      100, 50, 25, 10, 5, 0, 0, 0, 0, 0,  // High intensity values
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    )
    
    val threshold1 = otsuThreshold(sampleHistogram)
    val threshold2 = otsuThresholdSimple(sampleHistogram)
    
    println(s"Optimal threshold (method 1): $threshold1")
    println(s"Optimal threshold (method 2): $threshold2")
    
    // Example with a simple binary image
    val binaryImage = Array(
      Array(0, 0, 0, 1, 1, 1),
      Array(0, 0, 0, 1, 1, 1),
      Array(0, 0, 0, 1, 1, 1),
      Array(1, 1, 1, 0, 0, 0),
      Array(1, 1, 1, 0, 0, 0),
      Array(1, 1, 1, 0, 0, 0)
    )
    
    // Convert binary image to histogram
    val histogram = Array.fill(2)(0)
    binaryImage.foreach(row => 
      row.foreach(pixel => histogram(pixel) += 1)
    )
    
    println(s"Binary image histogram: ${histogram.mkString(", ")}")
    println(s"Optimal threshold: ${otsuThreshold(histogram)}")
  }
}
```

## Key Features of this Implementation:

1. **Two Methods**: 
   - `otsuThreshold`: Complete implementation with detailed calculations
   - `otsuThresholdSimple`: More concise version

2. **Core Algorithm**:
   - Calculates between-class variance for each possible threshold
   - Selects the threshold that maximizes the variance
   - Works with 8-bit grayscale images (256 intensity levels)

3. **Usage Example**:
   - Demonstrates with sample histogram data
   - Shows how to apply to binary images
   - Provides clear output with optimal threshold values

## How Otsu's Method Works:

1. **Calculate histogram** of pixel intensities
2. **For each possible threshold** value:
   - Split histogram into two classes (background and foreground)
   - Calculate class probabilities and means
   - Compute between-class variance
3. **Select threshold** that maximizes between-class variance

The algorithm automatically determines the optimal threshold without requiring manual parameter tuning, making it very useful for automatic image segmentation tasks.

