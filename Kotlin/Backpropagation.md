# Backpropagation Algorithm in Kotlin

Here's a complete implementation of a simple neural network with backpropagation using Kotlin:

```kotlin
import kotlin.math.exp
import kotlin.random.Random

class NeuralNetwork(
    private val inputSize: Int,
    private val hiddenSize: Int,
    private val outputSize: Int,
    private val learningRate: Double = 0.1
) {
    
    // Weights and biases
    private val weightsInputHidden = Array(hiddenSize) { DoubleArray(inputSize) { Random.nextDouble() * 0.5 - 0.25 } }
    private val biasHidden = DoubleArray(hiddenSize) { Random.nextDouble() * 0.5 - 0.25 }
    private val weightsHiddenOutput = DoubleArray(hiddenSize) { Random.nextDouble() * 0.5 - 0.25 }
    private val biasOutput = 0.0
    
    // Activation function (sigmoid)
    private fun sigmoid(x: Double): Double = 1.0 / (1.0 + exp(-x))
    
    // Derivative of sigmoid
    private fun sigmoidDerivative(x: Double): Double = x * (1.0 - x)
    
    // Forward propagation
    fun forward(input: DoubleArray): DoubleArray {
        // Hidden layer
        val hiddenOutputs = DoubleArray(hiddenSize) { i ->
            var sum = biasHidden[i]
            for (j in input.indices) {
                sum += weightsInputHidden[i][j] * input[j]
            }
            sigmoid(sum)
        }
        
        // Output layer
        val output = DoubleArray(outputSize) { i ->
            var sum = 0.0
            for (j in hiddenOutputs.indices) {
                sum += weightsHiddenOutput[j] * hiddenOutputs[j]
            }
            sigmoid(sum)
        }
        
        return output
    }
    
    // Backpropagation
    fun train(input: DoubleArray, target: DoubleArray, epochs: Int) {
        for (epoch in 1..epochs) {
            // Forward pass
            val hiddenOutputs = DoubleArray(hiddenSize) { i ->
                var sum = biasHidden[i]
                for (j in input.indices) {
                    sum += weightsInputHidden[i][j] * input[j]
                }
                sigmoid(sum)
            }
            
            val outputs = DoubleArray(outputSize) { i ->
                var sum = 0.0
                for (j in hiddenOutputs.indices) {
                    sum += weightsHiddenOutput[j] * hiddenOutputs[j]
                }
                sigmoid(sum)
            }
            
            // Calculate output layer errors
            val outputErrors = DoubleArray(outputSize) { i ->
                target[i] - outputs[i]
            }
            
            // Calculate output layer gradients
            val outputGradients = DoubleArray(outputSize) { i ->
                outputErrors[i] * sigmoidDerivative(outputs[i])
            }
            
            // Calculate hidden layer errors
            val hiddenErrors = DoubleArray(hiddenSize) { i ->
                var error = 0.0
                for (j in outputGradients.indices) {
                    error += outputGradients[j] * weightsHiddenOutput[i]
                }
                error
            }
            
            // Calculate hidden layer gradients
            val hiddenGradients = DoubleArray(hiddenSize) { i ->
                hiddenErrors[i] * sigmoidDerivative(hiddenOutputs[i])
            }
            
            // Update weights and biases (output layer)
            for (i in weightsHiddenOutput.indices) {
                weightsHiddenOutput[i] += learningRate * outputGradients[0] * hiddenOutputs[i]
            }
            // Update bias output
            // biasOutput += learningRate * outputGradients[0]
            
            // Update weights and biases (hidden layer)
            for (i in weightsInputHidden.indices) {
                for (j in weightsInputHidden[i].indices) {
                    weightsInputHidden[i][j] += learningRate * hiddenGradients[i] * input[j]
                }
                biasHidden[i] += learningRate * hiddenGradients[i]
            }
        }
    }
    
    // Calculate mean squared error
    fun calculateError(input: DoubleArray, target: DoubleArray): Double {
        val output = forward(input)
        var error = 0.0
        for (i in output.indices) {
            error += (target[i] - output[i]) * (target[i] - output[i])
        }
        return error / 2.0
    }
}

// Example usage
fun main() {
    // XOR problem data
    val inputs = arrayOf(
        doubleArrayOf(0.0, 0.0),
        doubleArrayOf(0.0, 1.0),
        doubleArrayOf(1.0, 0.0),
        doubleArrayOf(1.0, 1.0)
    )
    
    val targets = arrayOf(
        doubleArrayOf(0.0),
        doubleArrayOf(1.0),
        doubleArrayOf(1.0),
        doubleArrayOf(0.0)
    )
    
    // Create neural network (2 inputs, 4 hidden, 1 output)
    val nn = NeuralNetwork(2, 4, 1, 0.5)
    
    println("Training Neural Network on XOR problem...")
    println("Initial error: ${nn.calculateError(inputs[0], targets[0])}")
    
    // Train the network
    for (epoch in 1..10000) {
        for (i in inputs.indices) {
            nn.train(inputs[i], targets[i], 1)
        }
        
        if (epoch % 1000 == 0) {
            val totalError = inputs.indices.sumOf { i ->
                nn.calculateError(inputs[i], targets[i])
            } / inputs.size
            println("Epoch $epoch, Average Error: $totalError")
        }
    }
    
    // Test the network
    println("\nTesting the trained network:")
    for (i in inputs.indices) {
        val output = nn.forward(inputs[i])
        println("Input: ${inputs[i].contentToString()} -> Output: ${output[0].toString().take(4)} (Target: ${targets[i][0]})")
    }
}
```

## Key Components Explained:

### 1. **Neural Network Structure**
- Input layer with 2 neurons
- Hidden layer with 4 neurons
- Output layer with 1 neuron

### 2. **Forward Propagation**
- Input data flows through weights and biases
- Sigmoid activation function is applied at each neuron
- Output is calculated through the network

### 3. **Backpropagation Algorithm**
- Calculate output layer errors (target - actual)
- Compute gradients using derivative of activation function
- Propagate errors backward through the network
- Update weights and biases using gradient descent

### 4. **Training Process**
- Iteratively adjust weights to minimize error
- Uses mean squared error as loss function
- Learning rate controls the size of weight updates

### 5. **Expected Output**
The network should learn to approximate the XOR function, where:
- 0 XOR 0 = 0
- 0 XOR 1 = 1
- 1 XOR 0 = 1
- 1 XOR 1 = 0

This implementation demonstrates the core principles of backpropagation in a practical Kotlin application.

