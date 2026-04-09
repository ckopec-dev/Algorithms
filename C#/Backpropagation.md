# Backpropagation Algorithm in C#

Here's a complete implementation of a simple neural network with backpropagation in C#:

```csharp
using System;
using System.Linq;

public class NeuralNetwork
{
    private int[] layers;
    private double[][] weights;
    private double[][] biases;
    private double[][] activations;
    private double[][] errors;
    private double learningRate;

    public NeuralNetwork(int[] layers, double learningRate = 0.01)
    {
        this.layers = layers;
        this.learningRate = learningRate;
        InitializeWeights();
    }

    private void InitializeWeights()
    {
        weights = new double[layers.Length - 1][];
        biases = new double[layers.Length - 1][];
        activations = new double[layers.Length][];
        errors = new double[layers.Length][];

        // Initialize activations and errors
        for (int i = 0; i < layers.Length; i++)
        {
            activations[i] = new double[layers[i]];
            errors[i] = new double[layers[i]];
        }

        // Initialize weights and biases
        Random random = new Random();
        for (int i = 0; i < layers.Length - 1; i++)
        {
            weights[i] = new double[layers[i] * layers[i + 1]];
            biases[i] = new double[layers[i + 1]];

            for (int j = 0; j < weights[i].Length; j++)
            {
                weights[i][j] = (random.NextDouble() - 0.5) * 2; // Random between -1 and 1
            }

            for (int j = 0; j < biases[i].Length; j++)
            {
                biases[i][j] = (random.NextDouble() - 0.5) * 2;
            }
        }
    }

    private double Sigmoid(double x)
    {
        return 1.0 / (1.0 + Math.Exp(-x));
    }

    private double SigmoidDerivative(double x)
    {
        return x * (1.0 - x);
    }

    public double[] Forward(double[] input)
    {
        // Set input layer
        for (int i = 0; i < input.Length; i++)
        {
            activations[0][i] = input[i];
        }

        // Forward propagation
        for (int layer = 0; layer < layers.Length - 1; layer++)
        {
            int currentLayerSize = layers[layer];
            int nextLayerSize = layers[layer + 1];

            for (int i = 0; i < nextLayerSize; i++)
            {
                double sum = biases[layer][i];
                int weightIndex = i * currentLayerSize;

                for (int j = 0; j < currentLayerSize; j++)
                {
                    sum += weights[layer][weightIndex + j] * activations[layer][j];
                }

                activations[layer + 1][i] = Sigmoid(sum);
            }
        }

        return activations[layers.Length - 1];
    }

    public void Backward(double[] target)
    {
        int outputLayer = layers.Length - 1;

        // Calculate output layer error
        for (int i = 0; i < layers[outputLayer]; i++)
        {
            double error = target[i] - activations[outputLayer][i];
            errors[outputLayer][i] = error * SigmoidDerivative(activations[outputLayer][i]);
        }

        // Backpropagate errors
        for (int layer = layers.Length - 2; layer >= 0; layer--)
        {
            int currentLayerSize = layers[layer];
            int nextLayerSize = layers[layer + 1];

            // Calculate errors for current layer
            for (int i = 0; i < currentLayerSize; i++)
            {
                double error = 0.0;
                int weightIndex = i;

                for (int j = 0; j < nextLayerSize; j++)
                {
                    error += weights[layer][weightIndex + j * currentLayerSize] * errors[layer + 1][j];
                }

                errors[layer][i] = error * SigmoidDerivative(activations[layer][i]);
            }
        }

        // Update weights and biases
        for (int layer = 0; layer < layers.Length - 1; layer++)
        {
            int currentLayerSize = layers[layer];
            int nextLayerSize = layers[layer + 1];

            // Update weights
            for (int i = 0; i < nextLayerSize; i++)
            {
                int weightIndex = i * currentLayerSize;

                for (int j = 0; j < currentLayerSize; j++)
                {
                    double delta = learningRate * errors[layer + 1][i] * activations[layer][j];
                    weights[layer][weightIndex + j] += delta;
                }
            }

            // Update biases
            for (int i = 0; i < nextLayerSize; i++)
            {
                biases[layer][i] += learningRate * errors[layer + 1][i];
            }
        }
    }

    public double Train(double[][] inputs, double[][] targets, int epochs)
    {
        double totalError = 0;

        for (int epoch = 0; epoch < epochs; epoch++)
        {
            totalError = 0;

            for (int i = 0; i < inputs.Length; i++)
            {
                double[] output = Forward(inputs[i]);
                Backward(targets[i]);

                // Calculate error for this sample
                for (int j = 0; j < targets[i].Length; j++)
                {
                    totalError += Math.Pow(targets[i][j] - output[j], 2);
                }
            }

            if (epoch % 100 == 0)
            {
                Console.WriteLine($"Epoch {epoch}, Error: {totalError / inputs.Length}");
            }
        }

        return totalError / inputs.Length;
    }

    public double[] Predict(double[] input)
    {
        return Forward(input);
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // XOR problem example
        double[][] inputs = {
            new double[] {0, 0},
            new double[] {0, 1},
            new double[] {1, 0},
            new double[] {1, 1}
        };

        double[][] targets = {
            new double[] {0},
            new double[] {1},
            new double[] {1},
            new double[] {0}
        };

        // Create neural network: 2 inputs, 4 hidden neurons, 1 output
        NeuralNetwork nn = new NeuralNetwork(new int[] {2, 4, 1}, 0.5);

        Console.WriteLine("Training neural network...");
        double error = nn.Train(inputs, targets, 1000);

        Console.WriteLine($"Final error: {error}");

        Console.WriteLine("\nPredictions:");
        foreach (var input in inputs)
        {
            double[] prediction = nn.Predict(input);
            Console.WriteLine($"Input: [{string.Join(", ", input)}] -> Output: [{string.Join(", ", prediction.Select(x => Math.Round(x, 3)))}]");
        }
    }
}
```

## Key Components Explained:

### 1. **Forward Propagation**
- Input data flows through the network
- Each neuron applies sigmoid activation function
- Weights and biases are used to compute weighted sums

### 2. **Backward Propagation**
- Calculates error at output layer
- Propagates error backward through the network
- Uses chain rule to compute gradients

### 3. **Weight Updates**
- Updates weights and biases using gradient descent
- Learning rate controls the step size of updates

### 4. **Training Process**
- Repeats forward and backward passes for multiple epochs
- Minimizes error between predicted and actual outputs

This implementation demonstrates the core principles of backpropagation for training neural networks, suitable for learning and experimentation with different architectures and datasets.

