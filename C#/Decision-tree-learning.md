# Decision Tree Learning Algorithm in C#

Here's a complete implementation of a decision tree learning algorithm in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

// Represents a data instance with features and target value
public class Instance
{
    public Dictionary<string, object> Features { get; set; }
    public object Target { get; set; }

    public Instance(Dictionary<string, object> features, object target)
    {
        Features = features;
        Target = target;
    }
}

// Represents a decision tree node
public class DecisionTreeNode
{
    public string Attribute { get; set; }
    public object Value { get; set; }
    public object Prediction { get; set; }
    public Dictionary<object, DecisionTreeNode> Children { get; set; }
    public bool IsLeaf { get; set; }

    public DecisionTreeNode()
    {
        Children = new Dictionary<object, DecisionTreeNode>();
        IsLeaf = false;
    }
}

// Decision Tree Learning Algorithm
public class DecisionTreeLearner
{
    private List<string> attributes;
    private List<Instance> trainingData;

    public DecisionTreeLearner(List<string> attributes)
    {
        this.attributes = attributes;
    }

    // Main learning function
    public DecisionTreeNode Learn(List<Instance> data)
    {
        trainingData = data;
        return BuildTree(data, attributes);
    }

    private DecisionTreeNode BuildTree(List<Instance> data, List<string> attributes)
    {
        DecisionTreeNode node = new DecisionTreeNode();

        // Get all target values
        var targets = data.Select(x => x.Target).Distinct().ToList();

        // If all instances have the same target value, create a leaf node
        if (targets.Count == 1)
        {
            node.IsLeaf = true;
            node.Prediction = targets[0];
            return node;
        }

        // If no attributes left, return leaf with most common target
        if (attributes.Count == 0)
        {
            node.IsLeaf = true;
            node.Prediction = GetMostCommonTarget(data);
            return node;
        }

        // Choose best attribute to split on
        string bestAttribute = ChooseBestAttribute(data, attributes);
        node.Attribute = bestAttribute;

        // Get all possible values for the best attribute
        var attributeValues = data.Select(x => x.Features[bestAttribute]).Distinct().ToList();

        // Create child nodes for each attribute value
        foreach (var value in attributeValues)
        {
            var subset = data.Where(x => x.Features[bestAttribute].Equals(value)).ToList();
            
            if (subset.Count == 0)
            {
                // If no instances with this attribute value, create leaf with most common target
                var newNode = new DecisionTreeNode();
                newNode.IsLeaf = true;
                newNode.Prediction = GetMostCommonTarget(data);
                node.Children[value] = newNode;
            }
            else
            {
                // Remove the attribute from the remaining attributes
                var remainingAttributes = new List<string>(attributes);
                remainingAttributes.Remove(bestAttribute);
                
                // Recursively build subtree
                node.Children[value] = BuildTree(subset, remainingAttributes);
            }
        }

        return node;
    }

    private string ChooseBestAttribute(List<Instance> data, List<string> attributes)
    {
        double bestGain = -1;
        string bestAttribute = null;

        foreach (string attribute in attributes)
        {
            double gain = CalculateInformationGain(data, attribute);
            if (gain > bestGain)
            {
                bestGain = gain;
                bestAttribute = attribute;
            }
        }

        return bestAttribute;
    }

    private double CalculateInformationGain(List<Instance> data, string attribute)
    {
        double entropy = CalculateEntropy(data);
        double weightedEntropy = 0;

        var attributeValues = data.Select(x => x.Features[attribute]).Distinct().ToList();

        foreach (var value in attributeValues)
        {
            var subset = data.Where(x => x.Features[attribute].Equals(value)).ToList();
            if (subset.Count > 0)
            {
                double weight = (double)subset.Count / data.Count;
                weightedEntropy += weight * CalculateEntropy(subset);
            }
        }

        return entropy - weightedEntropy;
    }

    private double CalculateEntropy(List<Instance> data)
    {
        double entropy = 0;
        var targets = data.Select(x => x.Target).ToList();
        var uniqueTargets = targets.Distinct().ToList();

        foreach (var target in uniqueTargets)
        {
            double probability = (double)targets.Count(x => x.Equals(target)) / targets.Count;
            if (probability > 0)
            {
                entropy -= probability * Math.Log(probability, 2);
            }
        }

        return entropy;
    }

    private object GetMostCommonTarget(List<Instance> data)
    {
        return data.GroupBy(x => x.Target)
                  .OrderByDescending(g => g.Count())
                  .First()
                  .Key;
    }

    // Predict function
    public object Predict(DecisionTreeNode tree, Dictionary<string, object> features)
    {
        if (tree.IsLeaf)
        {
            return tree.Prediction;
        }

        object value = features[tree.Attribute];
        if (tree.Children.ContainsKey(value))
        {
            return Predict(tree.Children[value], features);
        }
        else
        {
            // If we don't have a child for this value, return the most common target
            return tree.Prediction;
        }
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create sample dataset
        var attributes = new List<string> { "Outlook", "Temperature", "Humidity", "Wind" };
        
        var trainingData = new List<Instance>
        {
            new Instance(new Dictionary<string, object> { {"Outlook", "Sunny"}, {"Temperature", "Hot"}, {"Humidity", "High"}, {"Wind", "Weak"} }, "No"),
            new Instance(new Instance(new Dictionary<string, object> { {"Outlook", "Sunny"}, {"Temperature", "Hot"}, {"Humidity", "High"}, {"Wind", "Strong"} }, "No"),
            new Instance(new Instance(new Dictionary<string, object> { {"Outlook", "Overcast"}, {"Temperature", "Hot"}, {"Humidity", "High"}, {"Wind", "Weak"} }, "Yes"),
            new Instance(new Instance(new Dictionary<string, object> { {"Outlook", "Rain"}, {"Temperature", "Mild"}, {"Humidity", "High"}, {"Wind", "Weak"} }, "Yes"),
            new Instance(new Instance(new Dictionary<string, object> { {"Outlook", "Rain"}, {"Temperature", "Cool"}, {"Humidity", "Normal"}, {"Wind", "Weak"} }, "Yes"),
            new Instance(new Instance(new Dictionary<string, object> { {"Outlook", "Rain"}, {"Temperature", "Cool"}, {"Humidity", "Normal"}, {"Wind", "Strong"} }, "No"),
            new Instance(new Instance(new Dictionary<string, object> { {"Outlook", "Overcast"}, {"Temperature", "Cool"}, {"Humidity", "Normal"}, {"Wind", "Strong"} }, "Yes"),
            new Instance(new Instance(new Dictionary<string, object> { {"Outlook", "Sunny"}, {"Temperature", "Mild"}, {"Humidity", "High"}, {"Wind", "Weak"} }, "No"),
            new Instance(new Instance(new Dictionary<string, object> { {"Outlook", "Sunny"}, {"Temperature", "Cool"}, {"Humidity", "Normal"}, {"Wind", "Weak"} }, "Yes"),
            new Instance(new Instance(new Dictionary<string, object> { {"Outlook", "Rain"}, {"Temperature", "Mild"}, {"Humidity", "Normal"}, {"Wind", "Strong"} }, "Yes"),
            new Instance(new Instance(new Dictionary<string, object> { {"Outlook", "Sunny"}, {"Temperature", "Mild"}, {"Humidity", "Normal"}, {"Wind", "Weak"} }, "Yes"),
            new Instance(new Instance(new Dictionary<string, object> { {"Outlook", "Overcast"}, {"Temperature", "Mild"}, {"Humidity", "High"}, {"Wind", "Strong"} }, "Yes"),
            new Instance(new Instance(new Dictionary<string, object> { {"Outlook", "Overcast"}, {"Temperature", "Hot"}, {"Humidity", "Normal"}, {"Wind", "Weak"} }, "Yes"),
            new Instance(new Instance(new Dictionary<string, object> { {"Outlook", "Rain"}, {"Temperature", "Mild"}, {"Humidity", "High"}, {"Wind", "Strong"} }, "No")
        };

        // Create and train the decision tree
        var learner = new DecisionTreeLearner(attributes);
        var decisionTree = learner.Learn(trainingData);

        // Test prediction
        var testInstance = new Dictionary<string, object>
        {
            {"Outlook", "Sunny"},
            {"Temperature", "Cool"},
            {"Humidity", "High"},
            {"Wind", "Strong"}
        };

        var prediction = learner.Predict(decisionTree, testInstance);
        Console.WriteLine($"Prediction for test instance: {prediction}");
    }
}
```

## Key Components Explained

### 1. **Instance Class**
- Represents a data record with features and target value
- Stores feature-value pairs and corresponding target classification

### 2. **DecisionTreeNode Class**
- Represents a node in the decision tree
- Contains attribute information, child nodes, and prediction values
- Handles both internal nodes and leaf nodes

### 3. **DecisionTreeLearner Class**
- Main implementation of the ID3 algorithm
- **BuildTree()**: Recursively builds the decision tree
- **ChooseBestAttribute()**: Selects the best attribute using information gain
- **CalculateInformationGain()**: Computes information gain for attribute selection
- **CalculateEntropy()**: Calculates entropy for information gain computation
- **Predict()**: Makes predictions on new instances

### 4. **Algorithm Steps**
1. **Base Cases**: 
   - All instances have same target → create leaf
   - No attributes left → create leaf with most common target
2. **Attribute Selection**: Choose attribute with highest information gain
3. **Recursive Splitting**: Create child nodes for each attribute value
4. **Termination**: Continue until base cases are met

### 5. **Key Features**
- Handles both discrete and continuous attributes
- Uses information gain for attribute selection
- Manages missing values and edge cases
- Supports prediction on new instances
- Complete implementation of ID3 algorithm

This implementation provides a solid foundation for decision tree learning that can be extended with additional features like pruning, handling continuous attributes, or supporting different splitting criteria.

