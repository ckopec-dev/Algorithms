# Random Forest Algorithm in Go

Here's a complete implementation of a Random Forest algorithm in Go:

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "sort"
    "time"
)

// Feature represents a single feature in the dataset
type Feature struct {
    Name  string
    Value float64
}

// Sample represents a single data point
type Sample struct {
    Features []Feature
    Label    string
}

// DecisionTree represents a single decision tree
type DecisionTree struct {
    Root *TreeNode
    MaxDepth int
    MinSamplesSplit int
}

// TreeNode represents a node in the decision tree
type TreeNode struct {
    FeatureIndex int
    Threshold    float64
    Left         *TreeNode
    Right        *TreeNode
    IsLeaf       bool
    Prediction   string
    Samples      int
}

// RandomForest represents the random forest
type RandomForest struct {
    Trees       []*DecisionTree
    NumTrees    int
    MaxDepth    int
    MinSamplesSplit int
    NumFeatures int
}

// NewDecisionTree creates a new decision tree
func NewDecisionTree(maxDepth, minSamplesSplit int) *DecisionTree {
    return &DecisionTree{
        MaxDepth:        maxDepth,
        MinSamplesSplit: minSamplesSplit,
    }
}

// NewRandomForest creates a new random forest
func NewRandomForest(numTrees, maxDepth, minSamplesSplit, numFeatures int) *RandomForest {
    return &RandomForest{
        Trees:           make([]*DecisionTree, numTrees),
        NumTrees:        numTrees,
        MaxDepth:        maxDepth,
        MinSamplesSplit: minSamplesSplit,
        NumFeatures:     numFeatures,
    }
}

// gini calculates the Gini impurity
func gini(labels []string) float64 {
    if len(labels) == 0 {
        return 0
    }
    
    counts := make(map[string]int)
    for _, label := range labels {
        counts[label]++
    }
    
    gini := 1.0
    total := float64(len(labels))
    
    for _, count := range counts {
        probability := float64(count) / total
        gini -= probability * probability
    }
    
    return gini
}

// findBestSplit finds the best feature and threshold to split on
func findBestSplit(samples []Sample, featureIndices []int) (int, float64, float64) {
    bestGini := math.MaxFloat64
    bestFeature := -1
    bestThreshold := 0.0
    
    labels := make([]string, len(samples))
    for i, sample := range samples {
        labels[i] = sample.Label
    }
    
    for _, featureIndex := range featureIndices {
        // Get all feature values for this feature
        values := make([]float64, len(samples))
        for i, sample := range samples {
            values[i] = sample.Features[featureIndex].Value
        }
        
        // Sort values to find potential thresholds
        sort.Float64s(values)
        
        // Try different thresholds
        for i := 0; i < len(values)-1; i++ {
            threshold := (values[i] + values[i+1]) / 2
            
            leftLabels := make([]string, 0)
            rightLabels := make([]string, 0)
            
            for _, sample := range samples {
                if sample.Features[featureIndex].Value <= threshold {
                    leftLabels = append(leftLabels, sample.Label)
                } else {
                    rightLabels = append(rightLabels, sample.Label)
                }
            }
            
            // Calculate weighted Gini
            leftWeight := float64(len(leftLabels)) / float64(len(samples))
            rightWeight := float64(len(rightLabels)) / float64(len(samples))
            
            leftGini := gini(leftLabels)
            rightGini := gini(rightLabels)
            
            weightedGini := leftWeight*leftGini + rightWeight*rightGini
            
            if weightedGini < bestGini {
                bestGini = weightedGini
                bestFeature = featureIndex
                bestThreshold = threshold
            }
        }
    }
    
    return bestFeature, bestThreshold, bestGini
}

// buildTree builds a decision tree recursively
func (dt *DecisionTree) buildTree(samples []Sample, depth int, featureIndices []int) *TreeNode {
    if len(samples) == 0 {
        return &TreeNode{IsLeaf: true, Prediction: "unknown"}
    }
    
    // Check stopping criteria
    if depth >= dt.MaxDepth || len(samples) < dt.MinSamplesSplit {
        // Create leaf node with majority class
        labelCounts := make(map[string]int)
        for _, sample := range samples {
            labelCounts[sample.Label]++
        }
        
        maxCount := 0
        majorityLabel := ""
        for label, count := range labelCounts {
            if count > maxCount {
                maxCount = count
                majorityLabel = label
            }
        }
        
        return &TreeNode{
            IsLeaf:     true,
            Prediction: majorityLabel,
            Samples:    len(samples),
        }
    }
    
    // Find best split
    featureIndex, threshold, _ := findBestSplit(samples, featureIndices)
    
    if featureIndex == -1 {
        // No good split found, create leaf
        labelCounts := make(map[string]int)
        for _, sample := range samples {
            labelCounts[sample.Label]++
        }
        
        maxCount := 0
        majorityLabel := ""
        for label, count := range labelCounts {
            if count > maxCount {
                maxCount = count
                majorityLabel = label
            }
        }
        
        return &TreeNode{
            IsLeaf:     true,
            Prediction: majorityLabel,
            Samples:    len(samples),
        }
    }
    
    // Split samples
    leftSamples := make([]Sample, 0)
    rightSamples := make([]Sample, 0)
    
    for _, sample := range samples {
        if sample.Features[featureIndex].Value <= threshold {
            leftSamples = append(leftSamples, sample)
        } else {
            rightSamples = append(rightSamples, sample)
        }
    }
    
    // Create node
    node := &TreeNode{
        FeatureIndex: featureIndex,
        Threshold:    threshold,
        Left:         nil,
        Right:        nil,
        IsLeaf:       false,
        Samples:      len(samples),
    }
    
    // Recursively build left and right subtrees
    node.Left = dt.buildTree(leftSamples, depth+1, featureIndices)
    node.Right = dt.buildTree(rightSamples, depth+1, featureIndices)
    
    return node
}

// buildRandomTree builds a random decision tree for the random forest
func (rf *RandomForest) buildRandomTree(samples []Sample) *DecisionTree {
    tree := NewDecisionTree(rf.MaxDepth, rf.MinSamplesSplit)
    
    // Randomly select features for this tree
    featureCount := len(samples[0].Features)
    featureIndices := make([]int, rf.NumFeatures)
    
    for i := 0; i < rf.NumFeatures; i++ {
        featureIndices[i] = rand.Intn(featureCount)
    }
    
    // Build the tree
    tree.Root = tree.buildTree(samples, 0, featureIndices)
    
    return tree
}

// Train the random forest
func (rf *RandomForest) Train(samples []Sample) {
    rand.Seed(time.Now().UnixNano())
    
    for i := 0; i < rf.NumTrees; i++ {
        // Bootstrap sampling
        bootstrapSamples := make([]Sample, len(samples))
        for j := 0; j < len(samples); j++ {
            bootstrapSamples[j] = samples[rand.Intn(len(samples))]
        }
        
        // Build tree with random features
        tree := rf.buildRandomTree(bootstrapSamples)
        rf.Trees[i] = tree
    }
}

// Predict a single sample using the random forest
func (rf *RandomForest) Predict(sample Sample) string {
    predictions := make(map[string]int)
    
    for _, tree := range rf.Trees {
        prediction := rf.predictSample(tree.Root, sample)
        predictions[prediction]++
    }
    
    // Return the prediction with the most votes
    maxVotes := 0
    finalPrediction := ""
    for prediction, votes := range predictions {
        if votes > maxVotes {
            maxVotes = votes
            finalPrediction = prediction
        }
    }
    
    return finalPrediction
}

// Helper function for prediction
func (rf *RandomForest) predictSample(node *TreeNode, sample Sample) string {
    if node.IsLeaf {
        return node.Prediction
    }
    
    if sample.Features[node.FeatureIndex].Value <= node.Threshold {
        if node.Left != nil {
            return rf.predictSample(node.Left, sample)
        }
    } else {
        if node.Right != nil {
            return rf.predictSample(node.Right, sample)
        }
    }
    
    // Fallback
    return node.Prediction
}

// Example usage
func main() {
    // Create sample dataset
    samples := []Sample{
        {Features: []Feature{{Name: "feature1", Value: 1.0}, {Name: "feature2", Value: 2.0}}, Label: "A"},
        {Features: []Feature{{Name: "feature1", Value: 2.0}, {Name: "feature2", Value: 3.0}}, Label: "A"},
        {Features: []Feature{{Name: "feature1", Value: 3.0}, {Name: "feature2", Value: 1.0}}, Label: "B"},
        {Features: []Feature{{Name: "feature1", Value: 4.0}, {Name: "feature2", Value: 2.0}}, Label: "B"},
        {Features: []Feature{{Name: "feature1", Value: 5.0}, {Name: "feature2", Value: 3.0}}, Label: "B"},
        {Features: []Feature{{Name: "feature1", Value: 6.0}, {Name: "feature2", Value: 4.0}}, Label: "A"},
        {Features: []Feature{{Name: "feature1", Value: 7.0}, {Name: "feature2", Value: 5.0}}, Label: "A"},
        {Features: []Feature{{Name: "feature1", Value: 8.0}, {Name: "feature2", Value: 6.0}}, Label: "B"},
    }
    
    // Create and train random forest
    rf := NewRandomForest(5, 10, 2, 2)
    rf.Train(samples)
    
    // Make predictions
    testSample := Sample{
        Features: []Feature{{Name: "feature1", Value: 2.5}, {Name: "feature2", Value: 2.5}},
        Label:    "unknown",
    }
    
    prediction := rf.Predict(testSample)
    fmt.Printf("Prediction for test sample: %s\n", prediction)
    
    // Test with more samples
    testSamples := []Sample{
        {Features: []Feature{{Name: "feature1", Value: 1.5}, {Name: "feature2", Value: 1.5}}, Label: "unknown"},
        {Features: []Feature{{Name: "feature1", Value: 7.5}, {Name: "feature2", Value: 5.5}}, Label: "unknown"},
    }
    
    for i, sample := range testSamples {
        prediction := rf.Predict(sample)
        fmt.Printf("Test sample %d prediction: %s\n", i+1, prediction)
    }
}
```

## Key Features of this Implementation

### 1. **Core Components**
- `Sample`: Represents individual data points with features and labels
- `DecisionTree`: Single decision tree with tree-building capabilities
- `RandomForest`: Ensemble of multiple decision trees

### 2. **Key Algorithms**
- **Gini Impurity**: Used for measuring node purity
- **Best Split Selection**: Finds optimal feature and threshold for splits
- **Bootstrap Sampling**: Creates diverse training sets for each tree
- **Random Feature Selection**: Uses subset of features for each tree

### 3. **Training Process**
1. Bootstrap sampling of data
2. Random feature selection for each tree
3. Recursive tree building with Gini impurity
4. Ensemble voting for final predictions

### 4. **Usage Example**
The code demonstrates how to:
- Create a dataset
- Train a random forest with 5 trees
- Make predictions on new samples
- Handle both binary and multi-class classification

This implementation provides a solid foundation that can be extended with additional features like cross-validation, hyperparameter tuning, or support for continuous target variables.

