# ID3 Decision Tree Algorithm in C#

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

// Data structure for a decision tree node
public class TreeNode
{
    public string Feature { get; set; }
    public Dictionary<string, TreeNode> Children { get; set; }
    public string Value { get; set; }
    public bool IsLeaf { get; set; }
    
    public TreeNode()
    {
        Children = new Dictionary<string, TreeNode>();
        IsLeaf = false;
    }
}

// Main ID3 Decision Tree class
public class ID3DecisionTree
{
    private List<string> features;
    private Dictionary<string, List<string>> featureValues;
    
    public ID3DecisionTree(List<string> features)
    {
        this.features = features;
        this.featureValues = new Dictionary<string, List<string>>();
    }
    
    // Main method to build the decision tree
    public TreeNode BuildTree(List<Dictionary<string, string>> data, 
                             List<string> targetFeatures, 
                             string targetAttribute)
    {
        // If all examples have the same target value, return leaf node
        var targetValues = data.Select(x => x[targetAttribute]).ToList();
        if (targetValues.All(x => x == targetValues[0]))
        {
            var node = new TreeNode();
            node.IsLeaf = true;
            node.Value = targetValues[0];
            return node;
        }
        
        // If no features left, return leaf with most common target value
        if (features.Count == 0)
        {
            var mostCommon = targetValues.GroupBy(x => x)
                                       .OrderByDescending(g => g.Count())
                                       .First().Key;
            
            var node = new TreeNode();
            node.IsLeaf = true;
            node.Value = mostCommon;
            return node;
        }
        
        // Choose best feature using information gain
        var bestFeature = ChooseBestFeature(data, targetAttribute);
        
        var node = new TreeNode();
        node.Feature = bestFeature;
        
        // Get all possible values for the best feature
        var featureValues = data.Select(x => x[bestFeature]).Distinct().ToList();
        
        foreach (var value in featureValues)
        {
            // Create subset of data where feature equals value
            var subset = data.Where(x => x[bestFeature] == value).ToList();
            
            if (subset.Count == 0)
            {
                // If no examples, create leaf with most common target value
                var mostCommon = targetValues.GroupBy(x => x)
                                           .OrderByDescending(g => g.Count())
                                           .First().Key;
                
                var childNode = new TreeNode();
                childNode.IsLeaf = true;
                childNode.Value = mostCommon;
                node.Children[value] = childNode;
            }
            else
            {
                // Remove the best feature from remaining features
                var remainingFeatures = new List<string>(features);
                remainingFeatures.Remove(bestFeature);
                
                // Create subtree recursively
                var subTree = BuildTree(subset, remainingFeatures, targetAttribute);
                node.Children[value] = subTree;
            }
        }
        
        return node;
    }
    
    // Choose the best feature based on information gain
    private string ChooseBestFeature(List<Dictionary<string, string>> data, 
                                   string targetAttribute)
    {
        var bestGain = double.MinValue;
        var bestFeature = "";
        
        foreach (var feature in features)
        {
            if (feature == targetAttribute) continue;
            
            var gain = CalculateInformationGain(data, feature, targetAttribute);
            if (gain > bestGain)
            {
                bestGain = gain;
                bestFeature = feature;
            }
        }
        
        return bestFeature;
    }
    
    // Calculate information gain for a feature
    private double CalculateInformationGain(List<Dictionary<string, string>> data,
                                          string feature, 
                                          string targetAttribute)
    {
        var totalEntropy = CalculateEntropy(data, targetAttribute);
        var weightedEntropy = 0.0;
        
        var featureValues = data.Select(x => x[feature]).Distinct().ToList();
        
        foreach (var value in featureValues)
        {
            var subset = data.Where(x => x[feature] == value).ToList();
            if (subset.Count > 0)
            {
                var subsetEntropy = CalculateEntropy(subset, targetAttribute);
                weightedEntropy += (double)subset.Count / data.Count * subsetEntropy;
            }
        }
        
        return totalEntropy - weightedEntropy;
    }
    
    // Calculate entropy of a dataset
    private double CalculateEntropy(List<Dictionary<string, string>> data, 
                                  string targetAttribute)
    {
        var targetValues = data.Select(x => x[targetAttribute]).ToList();
        var total = targetValues.Count;
        
        if (total == 0) return 0;
        
        var entropy = 0.0;
        var uniqueValues = targetValues.Distinct().ToList();
        
        foreach (var value in uniqueValues)
        {
            var count = targetValues.Count(x => x == value);
            var probability = (double)count / total;
            if (probability > 0)
                entropy -= probability * Math.Log(probability, 2);
        }
        
        return entropy;
    }
    
    // Predict class for a new instance
    public string Predict(TreeNode root, Dictionary<string, string> instance)
    {
        if (root.IsLeaf)
            return root.Value;
            
        var featureValue = instance[root.Feature];
        
        if (root.Children.ContainsKey(featureValue))
        {
            return Predict(root.Children[featureValue], instance);
        }
        else
        {
            // If we don't have this feature value, return most common class
            return "Unknown";
        }
    }
    
    // Print the decision tree structure
    public void PrintTree(TreeNode node, string indent = "")
    {
        if (node.IsLeaf)
        {
            Console.WriteLine($"{indent}Predict: {node.Value}");
        }
        else
        {
            Console.WriteLine($"{indent}Feature: {node.Feature}");
            foreach (var child in node.Children)
            {
                Console.WriteLine($"{indent}  {child.Key}:");
                PrintTree(child.Value, indent + "    ");
            }
        }
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Sample dataset: Play Tennis
        var data = new List<Dictionary<string, string>>
        {
            new Dictionary<string, string> { {"Outlook", "Sunny"}, {"Temperature", "Hot"}, {"Humidity", "High"}, {"Wind", "Weak"}, {"PlayTennis", "No"} },
            new Dictionary<string, string> { {"Outlook", "Sunny"}, {"Temperature", "Hot"}, {"Humidity", "High"}, {"Wind", "Strong"}, {"PlayTennis", "No"} },
            new Dictionary<string, string> { {"Outlook", "Overcast"}, {"Temperature", "Hot"}, {"Humidity", "High"}, {"Wind", "Weak"}, {"PlayTennis", "Yes"} },
            new Dictionary<string, string> { {"Outlook", "Rain"}, {"Temperature", "Mild"}, {"Humidity", "High"}, {"Wind", "Weak"}, {"PlayTennis", "Yes"} },
            new Dictionary<string, string> { {"Outlook", "Rain"}, {"Temperature", "Cool"}, {"Humidity", "Normal"}, {"Wind", "Weak"}, {"PlayTennis", "Yes"} },
            new Dictionary<string, string> { {"Outlook", "Rain"}, {"Temperature", "Cool"}, {"Humidity", "Normal"}, {"Wind", "Strong"}, {"PlayTennis", "No"} },
            new Dictionary<string, string> { {"Outlook", "Overcast"}, {"Temperature", "Cool"}, {"Humidity", "Normal"}, {"Wind", "Strong"}, {"PlayTennis", "Yes"} },
            new Dictionary<string, string> { {"Outlook", "Sunny"}, {"Temperature", "Mild"}, {"Humidity", "High"}, {"Wind", "Weak"}, {"PlayTennis", "No"} },
            new Dictionary<string, string> { {"Outlook", "Sunny"}, {"Temperature", "Cool"}, {"Humidity", "Normal"}, {"Wind", "Weak"}, {"PlayTennis", "Yes"} },
            new Dictionary<string, string> { {"Outlook", "Rain"}, {"Temperature", "Mild"}, {"Humidity", "Normal"}, {"Wind", "Weak"}, {"PlayTennis", "Yes"} },
            new Dictionary<string, string> { {"Outlook", "Sunny"}, {"Temperature", "Mild"}, {"Humidity", "Normal"}, {"Wind", "Strong"}, {"PlayTennis", "Yes"} },
            new Dictionary<string, string> { {"Outlook", "Overcast"}, {"Temperature", "Mild"}, {"Humidity", "High"}, {"Wind", "Strong"}, {"PlayTennis", "Yes"} },
            new Dictionary<string, string> { {"Outlook", "Overcast"}, {"Temperature", "Hot"}, {"Humidity", "Normal"}, {"Wind", "Weak"}, {"PlayTennis", "Yes"} },
            new Dictionary<string, string> { {"Outlook", "Rain"}, {"Temperature", "Mild"}, {"Humidity", "High"}, {"Wind", "Strong"}, {"PlayTennis", "No"} }
        };
        
        // Define features
        var features = new List<string> { "Outlook", "Temperature", "Humidity", "Wind" };
        
        // Create and train the decision tree
        var id3Tree = new ID3DecisionTree(features);
        var root = id3Tree.BuildTree(data, features, "PlayTennis");
        
        // Print the tree structure
        Console.WriteLine("Decision Tree:");
        id3Tree.PrintTree(root);
        
        // Make a prediction
        var testInstance = new Dictionary<string, string>
        {
            {"Outlook", "Sunny"},
            {"Temperature", "Cool"},
            {"Humidity", "High"},
            {"Wind", "Strong"}
        };
        
        var prediction = id3Tree.Predict(root, testInstance);
        Console.WriteLine($"\nPrediction for test instance: {prediction}");
    }
}
```

## Key Features of this Implementation:

1. **TreeNode Class**: Represents nodes in the decision tree with features, children, and leaf status
2. **ID3 Algorithm**: Implements the core ID3 algorithm including information gain calculation
3. **Entropy Calculation**: Computes entropy for information gain measurement
4. **Recursive Tree Building**: Builds the tree recursively by selecting best features
5. **Prediction Method**: Traverses the tree to make predictions on new instances
6. **Tree Visualization**: Prints the decision tree structure for debugging

## Algorithm Steps:

1. Calculate entropy of the entire dataset
2. For each feature, calculate information gain
3. Select the feature with maximum information gain
4. Create a node for that feature
5. Recursively build subtrees for each possible value of the feature
6. Handle edge cases (no more features, all same class)

This implementation demonstrates the classic ID3 decision tree algorithm in C# and can be easily extended or modified for different datasets.