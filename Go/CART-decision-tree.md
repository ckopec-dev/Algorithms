# CART Decision Tree Algorithm in Go

```go
package main

import (
    "fmt"
    "math"
    "sort"
)

// Node represents a node in the decision tree
type Node struct {
    feature     int     // feature index for splitting
    threshold   float64 // threshold value for splitting
    left        *Node   // left child node
    right       *Node   // right child node
    value       int     // prediction value (for leaf nodes)
    isLeaf      bool    // whether this is a leaf node
}

// Dataset represents the training data
type Dataset struct {
    features [][]float64
    labels   []int
}

// CARTDecisionTree implements the Classification and Regression Tree algorithm
type CARTDecisionTree struct {
    root *Node
    maxDepth int
    minSamplesSplit int
}

// NewCARTDecisionTree creates a new CART decision tree
func NewCARTDecisionTree(maxDepth int, minSamplesSplit int) *CARTDecisionTree {
    return &CARTDecisionTree{
        maxDepth: maxDepth,
        minSamplesSplit: minSamplesSplit,
    }
}

// fit trains the decision tree on the given dataset
func (tree *CARTDecisionTree) Fit(dataset *Dataset) {
    tree.root = tree.buildTree(dataset.features, dataset.labels, 0)
}

// buildTree recursively builds the decision tree
func (tree *CARTDecisionTree) buildTree(features [][]float64, labels []int, depth int) *Node {
    nSamples := len(labels)
    
    // Create leaf node if max depth reached or too few samples
    if depth >= tree.maxDepth || nSamples < tree.minSamplesSplit {
        return &Node{
            value:  tree.majorityClass(labels),
            isLeaf: true,
        }
    }
    
    // Find best split
    bestFeature, bestThreshold := tree.findBestSplit(features, labels)
    
    // If no good split found, create leaf node
    if bestFeature == -1 {
        return &Node{
            value:  tree.majorityClass(labels),
            isLeaf: true,
        }
    }
    
    // Split the data
    leftFeatures, leftLabels, rightFeatures, rightLabels := tree.splitData(
        features, labels, bestFeature, bestThreshold)
    
    // Create node and recursively build subtrees
    node := &Node{
        feature:   bestFeature,
        threshold: bestThreshold,
        isLeaf:    false,
    }
    
    node.left = tree.buildTree(leftFeatures, leftLabels, depth+1)
    node.right = tree.buildTree(rightFeatures, rightLabels, depth+1)
    
    return node
}

// findBestSplit finds the best feature and threshold to split on
func (tree *CARTDecisionTree) findBestSplit(features [][]float64, labels []int) (int, float64) {
    nFeatures := len(features[0])
    nSamples := len(labels)
    
    bestGini := math.MaxFloat64
    bestFeature := -1
    bestThreshold := 0.0
    
    // For each feature
    for featureIdx := 0; featureIdx < nFeatures; featureIdx++ {
        // Get unique values for this feature
        values := make([]float64, nSamples)
        for i, row := range features {
            values[i] = row[featureIdx]
        }
        sort.Float64s(values)
        
        // Try different thresholds
        for i := 0; i < len(values)-1; i++ {
            threshold := (values[i] + values[i+1]) / 2.0
            
            leftLabels, rightLabels := tree.splitLabels(labels, features, featureIdx, threshold)
            
            gini := tree.calculateGini(leftLabels, rightLabels)
            
            if gini < bestGini {
                bestGini = gini
                bestFeature = featureIdx
                bestThreshold = threshold
            }
        }
    }
    
    return bestFeature, bestThreshold
}

// splitLabels splits labels based on feature threshold
func (tree *CARTDecisionTree) splitLabels(labels []int, features [][]float64, featureIdx int, threshold float64) ([]int, []int) {
    leftLabels := []int{}
    rightLabels := []int{}
    
    for i, row := range features {
        if row[featureIdx] <= threshold {
            leftLabels = append(leftLabels, labels[i])
        } else {
            rightLabels = append(rightLabels, labels[i])
        }
    }
    
    return leftLabels, rightLabels
}

// calculateGini calculates the Gini impurity for a split
func (tree *CARTDecisionTree) calculateGini(leftLabels, rightLabels []int) float64 {
    if len(leftLabels) == 0 || len(rightLabels) == 0 {
        return 0
    }
    
    total := len(leftLabels) + len(rightLabels)
    leftGini := tree.giniImpurity(leftLabels)
    rightGini := tree.giniImpurity(rightLabels)
    
    return (float64(len(leftLabels))/float64(total))*leftGini + 
           (float64(len(rightLabels))/float64(total))*rightGini
}

// giniImpurity calculates Gini impurity for a set of labels
func (tree *CARTDecisionTree) giniImpurity(labels []int) float64 {
    if len(labels) == 0 {
        return 0
    }
    
    classCounts := make(map[int]int)
    for _, label := range labels {
        classCounts[label]++
    }
    
    gini := 1.0
    total := float64(len(labels))
    
    for _, count := range classCounts {
        proportion := float64(count) / total
        gini -= proportion * proportion
    }
    
    return gini
}

// majorityClass returns the most frequent class in labels
func (tree *CARTDecisionTree) majorityClass(labels []int) int {
    classCounts := make(map[int]int)
    for _, label := range labels {
        classCounts[label]++
    }
    
    maxCount := 0
    majorityClass := 0
    for class, count := range classCounts {
        if count > maxCount {
            maxCount = count
            majorityClass = class
        }
    }
    
    return majorityClass
}

// splitData splits the dataset based on feature and threshold
func (tree *CARTDecisionTree) splitData(features [][]float64, labels []int, featureIdx int, threshold float64) ([][]float64, []int, [][]float64, []int) {
    leftFeatures := [][]float64{}
    leftLabels := []int{}
    rightFeatures := [][]float64{}
    rightLabels := []int{}
    
    for i, row := range features {
        if row[featureIdx] <= threshold {
            leftFeatures = append(leftFeatures, row)
            leftLabels = append(leftLabels, labels[i])
        } else {
            rightFeatures = append(rightFeatures, row)
            rightLabels = append(rightLabels, labels[i])
        }
    }
    
    return leftFeatures, leftLabels, rightFeatures, rightLabels
}

// Predict makes a prediction for a single sample
func (tree *CARTDecisionTree) Predict(sample []float64) int {
    return tree.predictRecursive(tree.root, sample)
}

// predictRecursive recursively traverses the tree to make a prediction
func (tree *CARTDecisionTree) predictRecursive(node *Node, sample []float64) int {
    if node.isLeaf {
        return node.value
    }
    
    if sample[node.feature] <= node.threshold {
        return tree.predictRecursive(node.left, sample)
    } else {
        return tree.predictRecursive(node.right, sample)
    }
}

// PredictAll makes predictions for multiple samples
func (tree *CARTDecisionTree) PredictAll(samples [][]float64) []int {
    predictions := make([]int, len(samples))
    for i, sample := range samples {
        predictions[i] = tree.Predict(sample)
    }
    return predictions
}

// Example usage
func main() {
    // Create sample dataset (features: [sepal_length, sepal_width, petal_length, petal_width])
    features := [][]float64{
        {5.1, 3.5, 1.4, 0.2},
        {4.9, 3.0, 1.4, 0.2},
        {6.2, 3.4, 5.4, 2.3},
        {5.9, 3.0, 5.1, 1.8},
        {6.3, 2.8, 5.1, 1.5},
        {5.5, 2.6, 4.4, 1.2},
        {6.0, 2.2, 4.0, 1.0},
        {5.8, 2.8, 5.1, 2.4},
        {5.0, 2.3, 3.3, 1.0},
        {5.6, 2.5, 3.0, 1.1},
    }
    
    // Labels: 0 = setosa, 1 = versicolor, 2 = virginica
    labels := []int{0, 0, 2, 2, 2, 1, 1, 2, 1, 1}
    
    // Create and train the decision tree
    dataset := &Dataset{
        features: features,
        labels:   labels,
    }
    
    tree := NewCARTDecisionTree(5, 2)
    tree.Fit(dataset)
    
    // Make predictions
    testSamples := [][]float64{
        {5.2, 3.4, 1.4, 0.2},
        {6.1, 2.8, 4.7, 1.2},
        {5.7, 2.8, 4.1, 1.3},
    }
    
    predictions := tree.PredictAll(testSamples)
    
    fmt.Println("Decision Tree Predictions:")
    for i, prediction := range predictions {
        fmt.Printf("Sample %d: Predicted class = %d\n", i+1, prediction)
    }
    
    // Test individual prediction
    sample := []float64{5.5, 2.5, 4.0, 1.3}
    prediction := tree.Predict(sample)
    fmt.Printf("\nIndividual prediction for sample %v: %d\n", sample, prediction)
}
```

## Key Features of this Implementation

1. **Node Structure**: Represents tree nodes with features, thresholds, and child pointers
2. **Gini Impurity**: Uses Gini impurity for measuring the quality of splits
3. **Recursive Building**: Builds the tree recursively by finding optimal splits
4. **Pruning**: Includes depth and sample size constraints to prevent overfitting
5. **Prediction**: Makes predictions by traversing the tree from root to leaf

## How it Works

1. **Training Phase**: 
   - Recursively splits data based on features that minimize Gini impurity
   - Stops splitting when maximum depth is reached or minimum samples required

2. **Prediction Phase**:
   - Traverses the tree from root to leaf based on feature values
   - Returns the majority class of the leaf node

This implementation demonstrates the core CART algorithm for classification tasks with proper handling of tree construction, splitting criteria, and prediction mechanisms.

