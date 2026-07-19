# Gradient Boosting Machines (GBM) Implementation in C#

Below is a simplified implementation of a Gradient Boosting Machine algorithm in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class GradientBoostingMachine
{
    public class DecisionTree
    {
        public double Threshold { get; set; }
        public int FeatureIndex { get; set; }
        public double LeftValue { get; set; }
        public double RightValue { get; set; }
        public bool IsLeaf { get; set; }
        public DecisionTree Left { get; set; }
        public DecisionTree Right { get; set; }

        public double Predict(double[] features)
        {
            if (IsLeaf) return LeftValue;
            
            if (features[FeatureIndex] <= Threshold)
                return Left.Predict(features);
            else
                return Right.Predict(features);
        }
    }

    private List<DecisionTree> trees;
    private double learningRate;
    private int numberOfTrees;
    private double[] predictions;

    public GradientBoostingMachine(int numberOfTrees = 100, double learningRate = 0.1)
    {
        this.numberOfTrees = numberOfTrees;
        this.learningRate = learningRate;
        this.trees = new List<DecisionTree>();
    }

    public void Train(double[][] features, double[] targets)
    {
        // Initialize predictions with mean of targets
        double initialPrediction = targets.Average();
        predictions = Enumerable.Repeat(initialPrediction, targets.Length).ToArray();

        for (int i = 0; i < numberOfTrees; i++)
        {
            // Calculate residuals (negative gradient)
            double[] residuals = new double[targets.Length];
            for (int j = 0; j < targets.Length; j++)
            {
                residuals[j] = targets[j] - predictions[j];
            }

            // Train a decision tree on residuals
            DecisionTree tree = BuildTree(features, residuals);
            
            // Add tree to ensemble with learning rate
            trees.Add(tree);
            
            // Update predictions
            for (int j = 0; j < predictions.Length; j++)
            {
                predictions[j] += learningRate * tree.Predict(features[j]);
            }
        }
    }

    private DecisionTree BuildTree(double[][] features, double[] residuals)
    {
        // Simplified tree building - in practice this would be more complex
        DecisionTree tree = new DecisionTree();
        
        // Simple approach: just return a leaf with average residual
        tree.IsLeaf = true;
        tree.LeftValue = residuals.Average();
        
        return tree;
    }

    public double[] Predict(double[][] features)
    {
        double[] predictions = new double[features.Length];
        
        for (int i = 0; i < features.Length; i++)
        {
            double prediction = 0;
            
            foreach (var tree in trees)
            {
                prediction += learningRate * tree.Predict(features[i]);
            }
            
            predictions[i] = prediction;
        }
        
        return predictions;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Sample data
        double[][] features = new double[][]
        {
            new double[] { 1.0, 2.0 },
            new double[] { 2.0, 3.0 },
            new double[] { 3.0, 4.0 },
            new double[] { 4.0, 5.0 },
            new double[] { 5.0, 6.0 }
        };

        double[] targets = new double[] { 3.0, 5.0, 7.0, 9.0, 11.0 };

        // Create and train GBM
        var gbm = new GradientBoostingMachine(numberOfTrees: 50, learningRate: 0.1);
        gbm.Train(features, targets);

        // Make predictions
        double[][] testFeatures = new double[][]
        {
            new double[] { 2.5, 3.5 },
            new double[] { 4.5, 5.5 }
        };

        double[] predictions = gbm.Predict(testFeatures);
        
        Console.WriteLine("Predictions:");
        foreach (double pred in predictions)
        {
            Console.WriteLine($"Prediction: {pred:F2}");
        }
    }
}
```

## Key Components Explained:

### 1. **DecisionTree Class**
- Represents individual decision trees in the ensemble
- Contains threshold, feature index, and prediction values
- Has a `Predict` method for making predictions

### 2. **GradientBoostingMachine Class**
- **Constructor**: Sets number of trees and learning rate
- **Train Method**: 
  - Initializes predictions with mean target value
  - Iteratively builds decision trees on residuals
  - Updates predictions using learning rate
- **Predict Method**: Makes predictions by summing contributions from all trees

### 3. **Algorithm Flow**
1. Initialize predictions with average target value
2. For each iteration:
   - Calculate residuals (actual - predictions)
   - Train a decision tree on residuals
   - Add tree to ensemble with learning rate
   - Update predictions
3. Make final predictions by summing all tree contributions

## Important Notes:

- This is a simplified implementation for demonstration purposes
- A full GBM would include:
  - More sophisticated tree building (using information gain, etc.)
  - Regularization techniques
  - Cross-validation for hyperparameter tuning
  - Support for different loss functions
  - More complex feature selection and splitting criteria

## Usage:
```csharp
var gbm = new GradientBoostingMachine(100, 0.1);
gbm.Train(trainingFeatures, trainingTargets);
double[] predictions = gbm.Predict(testFeatures);
```

This implementation provides the core concept of gradient boosting while maintaining simplicity for educational purposes.