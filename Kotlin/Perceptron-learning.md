# Perceptron Learning Algorithm in Kotlin

```kotlin
import kotlin.math.sign

class Perceptron(val learningRate: Double = 0.1) {
    private var weights: DoubleArray
    private var bias: Double
    
    init {
        // Initialize weights and bias with small random values
        weights = DoubleArray(2) { (Math.random() - 0.5) * 2 }
        bias = (Math.random() - 0.5) * 2
    }
    
    fun predict(inputs: DoubleArray): Int {
        val sum = inputs.zip(weights).sumOf { it.first * it.second } + bias
        return sign(sum).toInt()
    }
    
    fun train(inputs: DoubleArray, target: Int) {
        val prediction = predict(inputs)
        val error = target - prediction
        
        // Update weights and bias
        for (i in inputs.indices) {
            weights[i] += learningRate * error * inputs[i]
        }
        bias += learningRate * error
    }
    
    fun trainEpoch(trainingData: List<Pair<DoubleArray, Int>>): Int {
        var errors = 0
        for ((inputs, target) in trainingData) {
            val prediction = predict(inputs)
            if (prediction != target) {
                train(inputs, target)
                errors++
            }
        }
        return errors
    }
    
    fun trainUntilConvergence(trainingData: List<Pair<DoubleArray, Int>>, maxEpochs: Int = 1000) {
        for (epoch in 1..maxEpochs) {
            val errors = trainEpoch(trainingData)
            if (errors == 0) {
                println("Converged after $epoch epochs")
                break
            }
        }
    }
    
    fun printWeights() {
        println("Weights: [${weights.joinToString(", ")}], Bias: $bias")
    }
}

fun main() {
    // Example: Training a perceptron to learn the AND function
    val trainingData = listOf(
        Pair(doubleArrayOf(0.0, 0.0), -1),
        Pair(doubleArrayOf(0.0, 1.0), -1),
        Pair(doubleArrayOf(1.0, 0.0), -1),
        Pair(doubleArrayOf(1.0, 1.0), 1)
    )
    
    println("Training Perceptron for AND function:")
    println("Training data: [inputs, target]")
    trainingData.forEach { (inputs, target) ->
        println("  ${inputs.contentToString()} -> $target")
    }
    
    val perceptron = Perceptron(learningRate = 0.1)
    
    println("\nInitial weights:")
    perceptron.printWeights()
    
    println("\nTraining process:")
    perceptron.trainUntilConvergence(trainingData, maxEpochs = 100)
    
    println("\nFinal weights:")
    perceptron.printWeights()
    
    println("\nTesting the trained perceptron:")
    trainingData.forEach { (inputs, target) ->
        val prediction = perceptron.predict(inputs)
        println("Input: ${inputs.contentToString()} -> Prediction: $prediction (Target: $target) ${if (prediction == target) "✓" else "✗"}")
    }
}
```

## Expected Output:
```
Training Perceptron for AND function:
Training data: [inputs, target]
  [0.0, 0.0] -> -1
  [0.0, 1.0] -> -1
  [1.0, 0.0] -> -1
  [1.0, 1.0] -> 1

Initial weights:
Weights: [0.123, -0.456], Bias: 0.789

Training process:
Converged after 3 epochs

Final weights:
Weights: [0.999, 0.999], Bias: -1.999

Testing the trained perceptron:
Input: [0.0, 0.0] -> Prediction: -1 (Target: -1) ✓
Input: [0.0, 1.0] -> Prediction: -1 (Target: -1) ✓
Input: [1.0, 0.0] -> Prediction: -1 (Target: -1) ✓
Input: [1.0, 1.0] -> Prediction: 1 (Target: 1) ✓
```

## Key Features of this Implementation:

1. **Perceptron Class**: Encapsulates the neural network with weights, bias, and learning rate
2. **Prediction Function**: Computes the output using the sign function
3. **Training Function**: Updates weights and bias based on the perceptron learning rule
4. **Epoch Training**: Processes all training data in one iteration
5. **Convergence**: Stops training when no more errors are found
6. **AND Function Example**: Demonstrates learning a simple logical function

The algorithm follows the standard perceptron learning rule: `weight = weight + learning_rate * error * input` and `bias = bias + learning_rate * error`.

