# CART Decision Tree Algorithm in C#

Here's a complete implementation of the CART (Classification and Regression Trees) decision tree algorithm in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class DecisionTreeNode
{
    public bool IsLeaf { get; set; }
    public int? FeatureIndex { get; set; }
    public double? Threshold { get; set; }
    public int? Prediction { get; set; }
    public DecisionTreeNode Left { get; set; }
    public DecisionTreeNode Right { get; set; }
    public Dictionary<int, int> ClassCounts { get; set; }
    
    public DecisionTreeNode()
    {
        ClassCounts = new Dictionary<int, int>();
    }
}

public class CARTDecisionTree
{
    private DecisionTreeNode root;
    private int maxDepth;
    private int minSamplesSplit;
    
    public CARTDecisionTree(int maxDepth = 10, int minSamplesSplit = 2)
    {
        this.maxDepth = maxDepth;
        this.minSamplesSplit = minSamplesSplit;
    }
    
    public void Fit(double[][] X, int[] y)
    {
        root = BuildTree(X, y, 0);
    }
    
    public int Predict(double[] sample)
    {
        return PredictRecursive(root, sample);
    }
    
    public int[] Predict(double[][] samples)
    {
        return samples.Select(sample => Predict(sample)).ToArray();
    }
    
    private DecisionTreeNode BuildTree(double[][] X, int[] y, int depth)
    {
        var node = new DecisionTreeNode();
        int nSamples = X.Length;
        int nFeatures = X[0].Length;
        
        // Count class occurrences
        foreach (int label in y)
        {
            if (node.ClassCounts.ContainsKey(label))
                node.ClassCounts[label]++;
            else
                node.ClassCounts[label] = 1;
        }
        
        // Check stopping criteria
        if (depth >= maxDepth || nSamples < minSamplesSplit || node.ClassCounts.Count <= 1)
        {
            node.IsLeaf = true;
            node.Prediction = GetMajorityClass(node.ClassCounts);
            return node;
        }
        
        // Find best split
        var bestSplit = FindBestSplit(X, y, nFeatures);
        
        if (bestSplit.Gain <= 0)
        {
            node.IsLeaf = true;
            node.Prediction = GetMajorityClass(node.ClassCounts);
            return node;
        }
        
        // Split the data
        var (leftX, leftY, rightX, rightY) = SplitData(X, y, bestSplit.FeatureIndex, bestSplit.Threshold);
        
        // Create node
        node.FeatureIndex = bestSplit.FeatureIndex;
        node.Threshold = bestSplit.Threshold;
        
        // Recursively build left and right subtrees
        node.Left = BuildTree(leftX, leftY, depth + 1);
        node.Right = BuildTree(rightX, rightY, depth + 1);
        
        return node;
    }
    
    private (int FeatureIndex, double Threshold, double Gain) FindBestSplit(double[][] X, int[] y, int nFeatures)
    {
        double bestGain = -1;
        int bestFeature = -1;
        double bestThreshold = 0;
        
        for (int feature = 0; feature < nFeatures; feature++)
        {
            var thresholds = X.Select(row => row[feature]).Distinct().OrderBy(x => x).ToArray();
            
            for (int i = 0; i < thresholds.Length - 1; i++)
            {
                double threshold = (thresholds[i] + thresholds[i + 1]) / 2.0;
                var (leftY, rightY) = SplitLabels(y, X, feature, threshold);
                
                if (leftY.Length == 0 || rightY.Length == 0) continue;
                
                double gain = CalculateInformationGain(y, leftY, rightY);
                
                if (gain > bestGain)
                {
                    bestGain = gain;
                    bestFeature = feature;
                    bestThreshold = threshold;
                }
            }
        }
        
        return (bestFeature, bestThreshold, bestGain);
    }
    
    private double CalculateInformationGain(int[] y, int[] leftY, int[] rightY)
    {
        double parentEntropy = CalculateEntropy(y);
        double leftWeight = (double)leftY.Length / y.Length;
        double rightWeight = (double)rightY.Length / y.Length;
        
        double leftEntropy = CalculateEntropy(leftY);
        double rightEntropy = CalculateEntropy(rightY);
        
        double weightedEntropy = leftWeight * leftEntropy + rightWeight * rightEntropy;
        return parentEntropy - weightedEntropy;
    }
    
    private double CalculateEntropy(int[] labels)
    {
        if (labels.Length == 0) return 0;
        
        var classCounts = new Dictionary<int, int>();
        foreach (int label in labels)
        {
            if (classCounts.ContainsKey(label))
                classCounts[label]++;
            else
                classCounts[label] = 1;
        }
        
        double entropy = 0;
        int total = labels.Length;
        
        foreach (var count in classCounts.Values)
        {
            double probability = (double)count / total;
            if (probability > 0)
                entropy -= probability * Math.Log(probability, 2);
        }
        
        return entropy;
    }
    
    private (int[] leftY, int[] rightY) SplitLabels(int[] y, double[][] X, int featureIndex, double threshold)
    {
        var leftY = new List<int>();
        var rightY = new List<int>();
        
        for (int i = 0; i < X.Length; i++)
        {
            if (X[i][featureIndex] <= threshold)
                leftY.Add(y[i]);
            else
                rightY.Add(y[i]);
        }
        
        return (leftY.ToArray(), rightY.ToArray());
    }
    
    private (double[][] leftX, int[] leftY, double[][] rightX, int[] rightY) SplitData(double[][] X, int[] y, int featureIndex, double threshold)
    {
        var leftX = new List<double[]>();
        var leftY = new List<int>();
        var rightX = new List<double[]>();
        var rightY = new List<int>();
        
        for (int i = 0; i < X.Length; i++)
        {
            if (X[i][featureIndex] <= threshold)
            {
                leftX.Add(X[i]);
                leftY.Add(y[i]);
            }
            else
            {
                rightX.Add(X[i]);
                rightY.Add(y[i]);
            }
        }
        
        return (leftX.ToArray(), leftY.ToArray(), rightX.ToArray(), rightY.ToArray());
    }
    
    private int GetMajorityClass(Dictionary<int, int> classCounts)
    {
        return classCounts.OrderByDescending(pair => pair.Value).First().Key;
    }
    
    private int PredictRecursive(DecisionTreeNode node, double[] sample)
    {
        if (node.IsLeaf)
            return node.Prediction.Value;
            
        if (sample[node.FeatureIndex.Value] <= node.Threshold.Value)
            return PredictRecursive(node.Left, sample);
        else
            return PredictRecursive(node.Right, sample);
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Sample dataset: [feature1, feature2] -> class
        double[][] X = {
            new double[] {2.0, 3.0},
            new double[] {5.0, 4.0},
            new double[] {1.0, 2.0},
            new double[] {4.0, 1.0},
            new double[] {3.0, 5.0},
            new double[] {6.0, 3.0}
        };
        
        int[] y = {0, 1, 0, 1, 1, 1};
        
        // Create and train the decision tree
        var tree = new CARTDecisionTree(maxDepth: 5, minSamplesSplit: 2);
        tree.Fit(X, y);
        
        // Make predictions
        double[] newSample = {3.5, 3.5};
        int prediction = tree.Predict(newSample);
        
        Console.WriteLine($"Prediction for sample [{string.Join(", ", newSample)}]: Class {prediction}");
        
        // Test with multiple samples
        double[][] testSamples = {
            new double[] {2.5, 2.5},
            new double[] {5.5, 3.5}
        };
        
        int[] predictions = tree.Predict(testSamples);
        Console.WriteLine("Predictions for test samples:");
        for (int i = 0; i < testSamples.Length; i++)
        {
            Console.WriteLine($"Sample [{string.Join(", ", testSamples[i])}]: Class {predictions[i]}");
        }
    }
}
```

## Key Features of this Implementation:

1. **Decision Tree Structure**: Uses a recursive tree node structure with left and right children
2. **Information Gain Calculation**: Implements entropy-based splitting criterion
3. **Stopping Criteria**: 
   - Maximum depth reached
   - Minimum samples required for split
   - Single class in node
4. **Binary Splitting**: Each node splits into two children based on a threshold
5. **Prediction**: Traverses the tree to make predictions for new samples

## Usage Example:

The code demonstrates training a decision tree on a simple 2D dataset and making predictions on new samples. The algorithm automatically determines the best feature and threshold for splitting at each node to maximize information gain.

This implementation provides a solid foundation for classification tasks and can be extended with additional features like pruning, handling of missing values, or different splitting criteria.

