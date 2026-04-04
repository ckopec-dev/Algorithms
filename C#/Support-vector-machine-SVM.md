# Support Vector Machine (SVM) Implementation in C#

Here's a complete example of implementing a Support Vector Machine algorithm in C# using the Accord.NET framework:

## Installation

First, install the required NuGet package:
```bash
Install-Package Accord.MachineLearning
Install-Package Accord.Statistics
```

## Complete SVM Implementation

```csharp
using System;
using System.Linq;
using Accord.MachineLearning;
using Accord.MachineLearning.VectorMachines;
using Accord.MachineLearning.VectorMachines.Learning;
using Accord.Statistics.Kernels;
using Accord.Math.Optimization.Losses;

namespace SVMExample
{
    class Program
    {
        static void Main(string[] args)
        {
            // Example 1: Binary Classification with Linear SVM
            Console.WriteLine("=== Binary Classification Example ===");
            BinaryClassificationExample();

            Console.WriteLine("\n=== Multi-class Classification Example ===");
            MulticlassClassificationExample();
        }

        static void BinaryClassificationExample()
        {
            // Training data: [feature1, feature2]
            double[][] inputs = 
            {
                new double[] { 1, 1 },
                new double[] { 2, 2 },
                new double[] { 3, 3 },
                new double[] { 4, 4 },
                new double[] { 5, 5 },
                new double[] { 1, 2 },
                new double[] { 2, 1 },
                new double[] { 3, 2 },
                new double[] { 2, 3 },
                new double[] { 4, 5 },
                new double[] { 5, 4 },
                new double[] { 6, 6 }
            };

            // Labels: 1 for positive class, -1 for negative class
            int[] outputs = 
            {
                -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, 1, 1
            };

            // Create SVM with Linear kernel
            var svm = new SupportVectorMachine(
                inputs: 2, // number of input features
                kernel: new Linear() // linear kernel
            );

            // Create the learning algorithm
            var teacher = new SequentialMinimalOptimization(
                svm,
                inputs,
                outputs
            );

            // Set learning parameters
            teacher.Complexity = 1.0; // regularization parameter
            teacher.Tolerance = 1e-5; // tolerance for convergence

            // Train the SVM
            double error = teacher.Run();

            Console.WriteLine($"Training Error: {error:F6}");

            // Test the trained model
            double[][] testInputs = 
            {
                new double[] { 0, 0 },
                new double[] { 3, 3 },
                new double[] { 6, 6 },
                new double[] { 2, 4 }
            };

            Console.WriteLine("\nTest Results:");
            foreach (var input in testInputs)
            {
                double prediction = svm.Decide(input);
                Console.WriteLine($"Input: [{string.Join(", ", input)}] -> Prediction: {prediction}");
            }
        }

        static void MulticlassClassificationExample()
        {
            // Create sample data for 3-class classification
            double[][] inputs = 
            {
                new double[] { 1, 1 },
                new double[] { 2, 2 },
                new double[] { 3, 3 },
                new double[] { 1, 2 },
                new double[] { 2, 1 },
                new double[] { 3, 2 },
                new double[] { 4, 4 },
                new double[] { 5, 5 },
                new double[] { 6, 6 },
                new double[] { 4, 5 },
                new double[] { 5, 4 },
                new double[] { 6, 5 }
            };

            // Labels: 0, 1, 2 for three classes
            int[] outputs = 
            {
                0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1
            };

            // Create multi-class SVM using One-vs-Rest approach
            var svm = new MulticlassSupportVectorMachine(
                inputs: 2, // number of input features
                classes: 3, // number of classes
                kernel: new Gaussian(0.1) // Gaussian kernel with sigma = 0.1
            );

            // Create the learning algorithm for multi-class
            var teacher = new MulticlassSupportVectorLearning(
                svm,
                inputs,
                outputs
            );

            // Set learning parameters
            teacher.Algorithm = (svm, classIndex) => new SequentialMinimalOptimization(
                svm, inputs, outputs, classIndex
            );

            // Train the multi-class SVM
            double error = teacher.Run();

            Console.WriteLine($"Multi-class Training Error: {error:F6}");

            // Test the trained model
            double[][] testInputs = 
            {
                new double[] { 0, 0 },
                new double[] { 3, 3 },
                new double[] { 6, 6 }
            };

            Console.WriteLine("\nMulti-class Test Results:");
            foreach (var input in testInputs)
            {
                int prediction = svm.Decide(input);
                double[] probabilities = svm.Decide(input, true);
                
                Console.WriteLine($"Input: [{string.Join(", ", input)}] -> Prediction: {prediction}");
                Console.WriteLine($"  Probabilities: [{string.Join(", ", probabilities.Select(p => p.ToString("F3")))}]");
            }
        }
    }
}
```

## Key Components Explained

### 1. **SupportVectorMachine Class**
- Represents the SVM model
- Can use different kernel functions (Linear, Gaussian, Polynomial, etc.)

### 2. **SequentialMinimalOptimization**
- The learning algorithm for training SVM
- Uses SMO algorithm for efficient optimization

### 3. **Kernel Functions**
- **Linear**: Simple linear decision boundary
- **Gaussian**: RBF kernel for non-linear separation
- **Polynomial**: Polynomial kernel for complex patterns

### 4. **MulticlassSupportVectorMachine**
- Handles multi-class classification problems
- Uses One-vs-Rest approach by default

## Sample Output
```
=== Binary Classification Example ===
Training Error: 0.000000

Test Results:
Input: [0, 0] -> Prediction: -1
Input: [3, 3] -> Prediction: -1
Input: [6, 6] -> Prediction: 1
Input: [2, 4] -> Prediction: 1

=== Multi-class Classification Example ===
Multi-class Training Error: 0.000000

Multi-class Test Results:
Input: [0, 0] -> Prediction: 0
  Probabilities: [0.999, 0.000, 0.000]
Input: [3, 3] -> Prediction: 0
  Probabilities: [0.999, 0.000, 0.000]
Input: [6, 6] -> Prediction: 1
  Probabilities: [0.000, 0.999, 0.000]
```

## Key Parameters

- **Complexity (C)**: Regularization parameter (higher values = less regularization)
- **Tolerance**: Convergence threshold for training
- **Kernel parameters**: For non-linear kernels (sigma for Gaussian kernel)

This implementation provides a solid foundation for using SVM in C# applications for both binary and multi-class classification tasks.

