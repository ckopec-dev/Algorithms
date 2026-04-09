# Random Forest Algorithm in C#

Here's a complete implementation of a Random Forest algorithm in C# with a simple example:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

// Decision Tree Node class
public class DecisionTreeNode
{
    public int FeatureIndex { get; set; }
    public double Threshold { get; set; }
    public DecisionTreeNode Left { get; set; }
    public DecisionTreeNode Right { get; set; }
    public int? Prediction { get; set; }
    public bool IsLeaf => Prediction.HasValue;
}

// Decision Tree class
public class DecisionTree
{
    private DecisionTreeNode root;
    private Random random;

    public DecisionTree()
    {
        random = new Random();
    }

    public void Train(double[][] features, int[] labels, int maxDepth = 10, int minSamples = 2)
    {
        root = BuildTree(features, labels, 0, maxDepth, minSamples);
    }

    private DecisionTreeNode BuildTree(double[][] features, int[] labels, int depth, int maxDepth, int minSamples)
    {
        int nSamples = features.Length;
        int nFeatures = features[0].Length;

        // Stopping criteria
        if (depth >= maxDepth || nSamples < minSamples || IsPure(labels))
        {
            return new DecisionTreeNode
            {
                Prediction = GetMajorityClass(labels)
            };
        }

        // Find best split
        var bestSplit = FindBestSplit(features, labels, nFeatures);
        
        if (bestSplit.Gain <= 0)
        {
            return new DecisionTreeNode
            {
                Prediction = GetMajorityClass(labels)
            };
        }

        // Split data
        var (leftFeatures, leftLabels, rightFeatures, rightLabels) = SplitData(features, labels, bestSplit.FeatureIndex, bestSplit.Threshold);

        // Create node
        var node = new DecisionTreeNode
        {
            FeatureIndex = bestSplit.FeatureIndex,
            Threshold = bestSplit.Threshold
        };

        // Recursively build left and right subtrees
        node.Left = BuildTree(leftFeatures, leftLabels, depth + 1, maxDepth, minSamples);
        node.Right = BuildTree(rightFeatures, rightLabels, depth + 1, maxDepth, minSamples);

        return node;
    }

    private (int FeatureIndex, double Threshold, double Gain) FindBestSplit(double[][] features, int[] labels, int nFeatures)
    {
        double bestGain = -1;
        int bestFeature = -1;
        double bestThreshold = 0;

        for (int feature = 0; feature < nFeatures; feature++)
        {
            var featureValues = features.Select(f => f[feature]).OrderBy(x => x).ToArray();
            var thresholds = GetThresholds(featureValues);

            foreach (double threshold in thresholds)
            {
                var (leftLabels, rightLabels) = SplitLabels(labels, features, feature, threshold);
                double gain = CalculateInformationGain(labels, leftLabels, rightLabels);
                
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

    private double[] GetThresholds(double[] values)
    {
        var thresholds = new List<double>();
        for (int i = 0; i < values.Length - 1; i++)
        {
            thresholds.Add((values[i] + values[i + 1]) / 2.0);
        }
        return thresholds.ToArray();
    }

    private (int[] LeftLabels, int[] RightLabels) SplitLabels(int[] labels, double[][] features, int featureIndex, double threshold)
    {
        var leftLabels = new List<int>();
        var rightLabels = new List<int>();

        for (int i = 0; i < features.Length; i++)
        {
            if (features[i][featureIndex] <= threshold)
                leftLabels.Add(labels[i]);
            else
                rightLabels.Add(labels[i]);
        }

        return (leftLabels.ToArray(), rightLabels.ToArray());
    }

    private double CalculateInformationGain(int[] parentLabels, int[] leftLabels, int[] rightLabels)
    {
        if (leftLabels.Length == 0 || rightLabels.Length == 0)
            return 0;

        double parentEntropy = CalculateEntropy(parentLabels);
        double leftEntropy = CalculateEntropy(leftLabels);
        double rightEntropy = CalculateEntropy(rightLabels);

        double leftWeight = (double)leftLabels.Length / parentLabels.Length;
        double rightWeight = (double)rightLabels.Length / parentLabels.Length;

        return parentEntropy - (leftWeight * leftEntropy + rightWeight * rightEntropy);
    }

    private double CalculateEntropy(int[] labels)
    {
        if (labels.Length == 0) return 0;

        var counts = new Dictionary<int, int>();
        foreach (int label in labels)
        {
            if (counts.ContainsKey(label))
                counts[label]++;
            else
                counts[label] = 1;
        }

        double entropy = 0;
        int total = labels.Length;

        foreach (var count in counts.Values)
        {
            double probability = (double)count / total;
            if (probability > 0)
                entropy -= probability * Math.Log(probability, 2);
        }

        return entropy;
    }

    private bool IsPure(int[] labels)
    {
        return labels.Distinct().Count() <= 1;
    }

    private int GetMajorityClass(int[] labels)
    {
        var counts = new Dictionary<int, int>();
        foreach (int label in labels)
        {
            if (counts.ContainsKey(label))
                counts[label]++;
            else
                counts[label] = 1;
        }

        return counts.OrderByDescending(x => x.Value).First().Key;
    }

    private (double[][] LeftFeatures, int[] LeftLabels, double[][] RightFeatures, int[] RightLabels) SplitData(
        double[][] features, int[] labels, int featureIndex, double threshold)
    {
        var leftFeatures = new List<double[]>();
        var leftLabels = new List<int>();
        var rightFeatures = new List<double[]>();
        var rightLabels = new List<int>();

        for (int i = 0; i < features.Length; i++)
        {
            if (features[i][featureIndex] <= threshold)
            {
                leftFeatures.Add(features[i]);
                leftLabels.Add(labels[i]);
            }
            else
            {
                rightFeatures.Add(features[i]);
                rightLabels.Add(labels[i]);
            }
        }

        return (leftFeatures.ToArray(), leftLabels.ToArray(), rightFeatures.ToArray(), rightLabels.ToArray());
    }

    public int Predict(double[] features)
    {
        return PredictRecursive(root, features);
    }

    private int PredictRecursive(DecisionTreeNode node, double[] features)
    {
        if (node.IsLeaf)
            return node.Prediction.Value;

        if (features[node.FeatureIndex] <= node.Threshold)
            return PredictRecursive(node.Left, features);
        else
            return PredictRecursive(node.Right, features);
    }
}

// Random Forest class
public class RandomForest
{
    private List<DecisionTree> trees;
    private int nTrees;
    private Random random;

    public RandomForest(int nTrees = 10)
    {
        this.nTrees = nTrees;
        this.trees = new List<DecisionTree>();
        this.random = new Random();
    }

    public void Train(double[][] features, int[] labels)
    {
        trees.Clear();
        
        for (int i = 0; i < nTrees; i++)
        {
            // Bootstrap sampling
            var (bootstrappedFeatures, bootstrappedLabels) = BootstrapSample(features, labels);
            
            var tree = new DecisionTree();
            tree.Train(bootstrappedFeatures, bootstrappedLabels);
            trees.Add(tree);
        }
    }

    public int Predict(double[] features)
    {
        var predictions = new List<int>();
        
        foreach (var tree in trees)
        {
            predictions.Add(tree.Predict(features));
        }

        // Return majority vote
        return predictions.GroupBy(x => x).OrderByDescending(g => g.Count()).First().Key;
    }

    private (double[][] Features, int[] Labels) BootstrapSample(double[][] features, int[] labels)
    {
        int nSamples = features.Length;
        var sampledFeatures = new List<double[]>();
        var sampledLabels = new List<int>();

        for (int i = 0; i < nSamples; i++)
        {
            int index = random.Next(nSamples);
            sampledFeatures.Add(features[index]);
            sampledLabels.Add(labels[index]);
        }

        return (sampledFeatures.ToArray(), sampledLabels.ToArray());
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Sample dataset: [age, income] -> [class (0 or 1)]
        double[][] features = new double[][]
        {
            new double[] {25, 50000},
            new double[] {35, 80000},
            new double[] {45, 120000},
            new double[] {23, 45000},
            new double[] {33, 70000},
            new double[] {52, 150000},
            new double[] {28, 60000},
            new double[] {41, 100000},
            new double[] {38, 90000},
            new double[] {29, 55000}
        };

        int[] labels = new int[] {0, 1, 1, 0, 1, 1, 0, 1, 1, 0};

        // Create and train Random Forest
        var rf = new RandomForest(5);
        rf.Train(features, labels);

        // Make predictions
        double[] testSample = new double[] {30, 65000};
        int prediction = rf.Predict(testSample);
        
        Console.WriteLine($"Prediction for sample [{string.Join(", ", testSample)}]: Class {prediction}");

        // Test with multiple samples
        double[][] testSamples = new double[][]
        {
            new double[] {25, 50000},
            new double[] {45, 120000},
            new double[] {35, 80000}
        };

        Console.WriteLine("\nPredictions for test samples:");
        foreach (var sample in testSamples)
        {
            int pred = rf.Predict(sample);
            Console.WriteLine($"Sample [{string.Join(", ", sample)}] -> Class {pred}");
        }
    }
}
```

## Key Features of this Implementation:

1. **Decision Tree**: Implements a basic decision tree with entropy-based splitting
2. **Random Forest**: Combines multiple decision trees with bootstrap sampling
3. **Bootstrap Sampling**: Each tree is trained on a random sample of the data
4. **Majority Voting**: Final prediction is made by majority vote of all trees
5. **Information Gain**: Uses entropy to determine the best splits

## How to Use:

1. Create a `RandomForest` instance with desired number of trees
2. Train the model using `Train()` method with features and labels
3. Make predictions using `Predict()` method

## Sample Output:
```
Prediction for sample [30, 65000]: Class 0

Predictions for test samples:
Sample [25, 50000] -> Class 0
Sample [45, 120000] -> Class 1
Sample [35, 80000] -> Class 1
```

This implementation provides a solid foundation for understanding and using Random Forest algorithms in C# applications.

