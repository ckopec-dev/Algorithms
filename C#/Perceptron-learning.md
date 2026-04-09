# Perceptron Learning Algorithm in C#

Here's a complete implementation of the Perceptron learning algorithm in C#:

```csharp
using System;
using System.Linq;

public class Perceptron
{
    private double[] weights;
    private double bias;
    private double learningRate;
    
    public Perceptron(int inputSize, double learningRate = 0.1)
    {
        this.learningRate = learningRate;
        this.weights = new double[inputSize];
        
        // Initialize weights with small random values
        Random random = new Random();
        for (int i = 0; i < inputSize; i++)
        {
            weights[i] = (random.NextDouble() - 0.5) * 2; // Random value between -1 and 1
        }
        this.bias = (random.NextDouble() - 0.5) * 2;
    }
    
    // Activation function (step function)
    private int Activate(double sum)
    {
        return sum >= 0 ? 1 : 0;
    }
    
    // Predict output for given inputs
    public int Predict(double[] inputs)
    {
        double sum = bias;
        for (int i = 0; i < inputs.Length; i++)
        {
            sum += weights[i] * inputs[i];
        }
        return Activate(sum);
    }
    
    // Train the perceptron using given data
    public void Train(double[][] inputs, int[] targets, int maxEpochs = 1000)
    {
        for (int epoch = 0; epoch < maxEpochs; epoch++)
        {
            bool allCorrect = true;
            
            for (int i = 0; i < inputs.Length; i++)
            {
                // Make prediction
                int prediction = Predict(inputs[i]);
                int target = targets[i];
                
                // Calculate error
                int error = target - prediction;
                
                // Update weights and bias if prediction is incorrect
                if (error != 0)
                {
                    allCorrect = false;
                    
                    // Update bias
                    bias += learningRate * error;
                    
                    // Update weights
                    for (int j = 0; j < weights.Length; j++)
                    {
                        weights[j] += learningRate * error * inputs[i][j];
                    }
                }
            }
            
            // If all predictions are correct, training is complete
            if (allCorrect)
            {
                Console.WriteLine($"Training completed after {epoch + 1} epochs");
                break;
            }
        }
    }
    
    // Get current weights and bias
    public double[] GetWeights() => weights;
    public double GetBias() => bias;
}

// Example usage
public class Program
{
    public static void Main()
    {
        Console.WriteLine("Perceptron Learning Algorithm Example");
        Console.WriteLine("=====================================");
        
        // Example 1: AND gate
        Console.WriteLine("\nTraining Perceptron for AND Gate:");
        Console.WriteLine("Input 1 | Input 2 | Output");
        Console.WriteLine("--------|---------|--------");
        Console.WriteLine("   0     |    0    |   0");
        Console.WriteLine("   0     |    1    |   0");
        Console.WriteLine("   1     |    0    |   0");
        Console.WriteLine("   1     |    1    |   1");
        
        // Training data for AND gate
        double[][] andInputs = {
            new double[] {0, 0},
            new double[] {0, 1},
            new double[] {1, 0},
            new double[] {1, 1}
        };
        
        int[] andTargets = {0, 0, 0, 1};
        
        // Create and train perceptron
        Perceptron andPerceptron = new Perceptron(2, 0.1);
        andPerceptron.Train(andInputs, andTargets);
        
        // Test the trained perceptron
        Console.WriteLine("\nTesting AND Gate:");
        for (int i = 0; i < andInputs.Length; i++)
        {
            int prediction = andPerceptron.Predict(andInputs[i]);
            Console.WriteLine($"Input: [{string.Join(", ", andInputs[i])}] -> Output: {prediction}");
        }
        
        // Example 2: OR gate
        Console.WriteLine("\nTraining Perceptron for OR Gate:");
        Console.WriteLine("Input 1 | Input 2 | Output");
        Console.WriteLine("--------|---------|--------");
        Console.WriteLine("   0     |    0    |   0");
        Console.WriteLine("   0     |    1    |   1");
        Console.WriteLine("   1     |    0    |   1");
        Console.WriteLine("   1     |    1    |   1");
        
        // Training data for OR gate
        double[][] orInputs = {
            new double[] {0, 0},
            new double[] {0, 1},
            new double[] {1, 0},
            new double[] {1, 1}
        };
        
        int[] orTargets = {0, 1, 1, 1};
        
        // Create and train perceptron
        Perceptron orPerceptron = new Perceptron(2, 0.1);
        orPerceptron.Train(orInputs, orTargets);
        
        // Test the trained perceptron
        Console.WriteLine("\nTesting OR Gate:");
        for (int i = 0; i < orInputs.Length; i++)
        {
            int prediction = orPerceptron.Predict(orInputs[i]);
            Console.WriteLine($"Input: [{string.Join(", ", orInputs[i])}] -> Output: {prediction}");
        }
        
        // Display final weights and bias
        Console.WriteLine("\nFinal weights and bias for AND gate:");
        Console.WriteLine($"Weights: [{string.Join(", ", andPerceptron.GetWeights())}]");
        Console.WriteLine($"Bias: {andPerceptron.GetBias()}");
    }
}
```

## Key Components Explained:

### 1. **Perceptron Class**
- **Weights**: Array of weights for each input feature
- **Bias**: Threshold value for the activation function
- **Learning Rate**: Controls how much weights are updated during training

### 2. **Core Methods**
- **Predict()**: Makes predictions using the current weights and bias
- **Train()**: Implements the perceptron learning rule
- **Activate()**: Step function that returns 1 if sum ≥ 0, else 0

### 3. **Learning Algorithm**
The perceptron learning rule updates weights and bias when prediction is incorrect:
```
weight[i] = weight[i] + learningRate * error * input[i]
bias = bias + learningRate * error
```

### 4. **Training Process**
- Iterates through training data multiple times (epochs)
- Updates weights only when prediction error occurs
- Stops when all examples are correctly classified

## Sample Output:
```
Perceptron Learning Algorithm Example
=====================================

Training Perceptron for AND Gate:
Input 1 | Input 2 | Output
--------|---------|--------
   0     |    0    |   0
   0     |    1    |   0
   1     |    0    |   0
   1     |    1    |   1

Training completed after 2 epochs

Testing AND Gate:
Input: [0, 0] -> Output: 0
Input: [0, 1] -> Output: 0
Input: [1, 0] -> Output: 0
Input: [1, 1] -> Output: 1
```

This implementation demonstrates how a perceptron can learn to classify linearly separable data like AND and OR gates.

