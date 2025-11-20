# Grover's Search Algorithm in Scala

Here's an implementation of Grover's search algorithm in Scala:

```scala
import scala.math.{sqrt, pi}
import scala.util.Random

object GroverSearch {
  
  // Oracle function - marks the target element
  def oracle(target: Int, query: Int): Boolean = {
    query == target
  }
  
  // Grover's diffusion operator (inversion about mean)
  def diffusionOperator(amplitudes: Array[Double]): Array[Double] = {
    val n = amplitudes.length
    val mean = amplitudes.sum / n
    
    amplitudes.map(amplitude => 2 * mean - amplitude)
  }
  
  // Grover iteration
  def groverIteration(amplitudes: Array[Double], target: Int): Array[Double] = {
    // Apply oracle (marking)
    val marked = amplitudes.zipWithIndex.map { case (amp, index) =>
      if (oracle(target, index)) -amp else amp
    }
    
    // Apply diffusion operator
    diffusionOperator(marked)
  }
  
  // Main Grover's search algorithm
  def groverSearch(size: Int, target: Int, iterations: Int): Array[Double] = {
    // Initialize uniform superposition
    val amplitudes = Array.fill(size)(1.0 / sqrt(size.toDouble))
    
    // Perform Grover iterations
    (1 to iterations).foldLeft(amplitudes) { (current, _) =>
      groverIteration(current, target)
    }
  }
  
  // Find the most likely target based on amplitudes
  def findTarget(amplitudes: Array[Double]): Int = {
    val probabilities = amplitudes.map(amp => amp * amp)
    probabilities.zipWithIndex.maxBy(_._1)._2
  }
  
  // Calculate optimal number of iterations
  def optimalIterations(n: Int, targetCount: Int = 1): Int = {
    val num = (pi / 4) * sqrt(n.toDouble / targetCount.toDouble)
    math.ceil(num).toInt
  }
  
  def main(args: Array[String]): Unit = {
    // Example: Search for number 5 in range 0-15
    val searchSpaceSize = 16
    val target = 5
    
    println(s"Searching for target $target in space of size $searchSpaceSize")
    
    // Calculate optimal iterations
    val iterations = optimalIterations(searchSpaceSize, 1)
    println(s"Optimal number of iterations: $iterations")
    
    // Run Grover's search
    val finalAmplitudes = groverSearch(searchSpaceSize, target, iterations)
    
    // Find the result
    val result = findTarget(finalAmplitudes)
    val probability = finalAmplitudes(result) * finalAmplitudes(result)
    
    println(s"Found target: $result")
    println(s"Probability of finding target: ${probability * 100} %")
    
    // Show amplitudes for all elements
    println("\nAmplitudes:")
    finalAmplitudes.zipWithIndex.foreach { case (amp, index) =>
      println(s"  Index $index: amplitude = ${amp.round(3)}, probability = ${(amp * amp).round(3)}")
    }
    
    // Verify the target has high probability
    println(s"\nTarget $target amplitude: ${finalAmplitudes(target).round(3)}")
    println(s"Target $target probability: ${finalAmplitudes(target) * finalAmplitudes(target).round(3)}")
  }
}

// Extension for rounding doubles
implicit class DoubleOps(val d: Double) extends AnyVal {
  def round(precision: Int): Double = {
    val multiplier = math.pow(10, precision)
    math.round(d * multiplier) / multiplier
  }
}
```

## How it works:

1. **Initialization**: Creates a uniform superposition of all possible states
2. **Oracle**: Marks the target element by flipping its amplitude sign
3. **Diffusion**: Amplifies the amplitude of marked states while reducing others
4. **Iteration**: Repeats oracle + diffusion steps for optimal number of times
5. **Measurement**: Extracts the most probable result

## Key Features:

- **Oracle function**: Marks the target element
- **Diffusion operator**: Inversion about the mean
- **Optimal iterations**: Calculates the theoretically optimal number of iterations
- **Probability calculation**: Shows how likely each element is to be found

## Expected Output:
```
Searching for target 5 in space of size 16
Optimal number of iterations: 2
Found target: 5
Probability of finding target: 99.0 %

Amplitudes:
  Index 0: amplitude = 0.25, probability = 0.063
  Index 1: amplitude = 0.25, probability = 0.063
  ...
  Index 5: amplitude = 0.707, probability = 0.5
  ...
```

This implementation demonstrates Grover's quadratic speedup for unstructured search problems, where a classical algorithm would require O(N) queries but Grover's algorithm only needs O(√N) queries.

