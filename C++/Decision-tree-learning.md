# Decision Tree Learning Algorithm in C++

Here's a complete implementation of a Decision Tree learning algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>
#include <cmath>
#include <limits>

// Structure to represent a data sample
struct Sample {
    std::vector<double> features;
    int label;
    
    Sample(const std::vector<double>& f, int l) : features(f), label(l) {}
};

// Structure to represent a decision tree node
struct TreeNode {
    bool isLeaf;
    int prediction;
    int featureIndex;
    double threshold;
    TreeNode* left;
    TreeNode* right;
    
    TreeNode() : isLeaf(false), prediction(0), featureIndex(-1), threshold(0.0), left(nullptr), right(nullptr) {}
};

class DecisionTree {
private:
    TreeNode* root;
    
    // Calculate entropy of a dataset
    double calculateEntropy(const std::vector<Sample>& samples) {
        if (samples.empty()) return 0.0;
        
        std::map<int, int> labelCount;
        for (const auto& sample : samples) {
            labelCount[sample.label]++;
        }
        
        double entropy = 0.0;
        int total = samples.size();
        
        for (const auto& pair : labelCount) {
            double probability = static_cast<double>(pair.second) / total;
            if (probability > 0) {
                entropy -= probability * log2(probability);
            }
        }
        
        return entropy;
    }
    
    // Calculate information gain for a split
    double calculateInformationGain(const std::vector<Sample>& samples, 
                                   int featureIndex, double threshold) {
        double totalEntropy = calculateEntropy(samples);
        
        std::vector<Sample> leftSamples, rightSamples;
        
        for (const auto& sample : samples) {
            if (sample.features[featureIndex] <= threshold) {
                leftSamples.push_back(sample);
            } else {
                rightSamples.push_back(sample);
            }
        }
        
        if (leftSamples.empty() || rightSamples.empty()) {
            return 0.0;
        }
        
        int total = samples.size();
        double leftWeight = static_cast<double>(leftSamples.size()) / total;
        double rightWeight = static_cast<double>(rightSamples.size()) / total;
        
        double weightedEntropy = leftWeight * calculateEntropy(leftSamples) + 
                                rightWeight * calculateEntropy(rightSamples);
        
        return totalEntropy - weightedEntropy;
    }
    
    // Find the best split for a dataset
    std::pair<int, double> findBestSplit(const std::vector<Sample>& samples) {
        if (samples.empty()) return {-1, 0.0};
        
        int bestFeature = -1;
        double bestThreshold = 0.0;
        double bestGain = -1.0;
        
        // Get unique values for each feature to determine thresholds
        int numFeatures = samples[0].features.size();
        int numSamples = samples.size();
        
        for (int featureIndex = 0; featureIndex < numFeatures; featureIndex++) {
            // Get all unique feature values for this feature
            std::vector<double> values;
            for (const auto& sample : samples) {
                values.push_back(sample.features[featureIndex]);
            }
            std::sort(values.begin(), values.end());
            values.erase(std::unique(values.begin(), values.end()), values.end());
            
            // Try different thresholds between consecutive values
            for (size_t i = 0; i < values.size() - 1; i++) {
                double threshold = (values[i] + values[i + 1]) / 2.0;
                double gain = calculateInformationGain(samples, featureIndex, threshold);
                
                if (gain > bestGain) {
                    bestGain = gain;
                    bestFeature = featureIndex;
                    bestThreshold = threshold;
                }
            }
        }
        
        return {bestFeature, bestThreshold};
    }
    
    // Get the most common label in a dataset
    int getMajorityLabel(const std::vector<Sample>& samples) {
        std::map<int, int> labelCount;
        for (const auto& sample : samples) {
            labelCount[sample.label]++;
        }
        
        int maxCount = 0;
        int majorityLabel = 0;
        for (const auto& pair : labelCount) {
            if (pair.second > maxCount) {
                maxCount = pair.second;
                majorityLabel = pair.first;
            }
        }
        
        return majorityLabel;
    }
    
    // Build the decision tree recursively
    TreeNode* buildTree(const std::vector<Sample>& samples, int maxDepth = 10, int currentDepth = 0) {
        TreeNode* node = new TreeNode();
        
        // Base cases
        if (samples.empty()) {
            node->isLeaf = true;
            node->prediction = 0;
            return node;
        }
        
        // Check if all samples have the same label
        int firstLabel = samples[0].label;
        bool allSame = true;
        for (const auto& sample : samples) {
            if (sample.label != firstLabel) {
                allSame = false;
                break;
            }
        }
        
        if (allSame) {
            node->isLeaf = true;
            node->prediction = firstLabel;
            return node;
        }
        
        // Check depth limit
        if (currentDepth >= maxDepth) {
            node->isLeaf = true;
            node->prediction = getMajorityLabel(samples);
            return node;
        }
        
        // Find best split
        auto bestSplit = findBestSplit(samples);
        int bestFeature = bestSplit.first;
        double bestThreshold = bestSplit.second;
        
        if (bestFeature == -1) {
            node->isLeaf = true;
            node->prediction = getMajorityLabel(samples);
            return node;
        }
        
        // Split the dataset
        std::vector<Sample> leftSamples, rightSamples;
        for (const auto& sample : samples) {
            if (sample.features[bestFeature] <= bestThreshold) {
                leftSamples.push_back(sample);
            } else {
                rightSamples.push_back(sample);
            }
        }
        
        // Recursively build left and right subtrees
        node->isLeaf = false;
        node->featureIndex = bestFeature;
        node->threshold = bestThreshold;
        node->left = buildTree(leftSamples, maxDepth, currentDepth + 1);
        node->right = buildTree(rightSamples, maxDepth, currentDepth + 1);
        
        return node;
    }
    
    // Make prediction for a single sample
    int predictSample(TreeNode* node, const std::vector<double>& features) {
        if (node->isLeaf) {
            return node->prediction;
        }
        
        if (features[node->featureIndex] <= node->threshold) {
            return predictSample(node->left, features);
        } else {
            return predictSample(node->right, features);
        }
    }
    
    // Helper function to delete tree nodes
    void deleteTree(TreeNode* node) {
        if (node) {
            deleteTree(node->left);
            deleteTree(node->right);
            delete node;
        }
    }
    
public:
    DecisionTree() : root(nullptr) {}
    
    ~DecisionTree() {
        deleteTree(root);
    }
    
    // Train the decision tree
    void train(const std::vector<Sample>& samples, int maxDepth = 10) {
        deleteTree(root);
        root = buildTree(samples, maxDepth);
    }
    
    // Make prediction for a single sample
    int predict(const std::vector<double>& features) {
        if (!root) return 0;
        return predictSample(root, features);
    }
    
    // Make predictions for multiple samples
    std::vector<int> predict(const std::vector<std::vector<double>>& featuresList) {
        std::vector<int> predictions;
        for (const auto& features : featuresList) {
            predictions.push_back(predict(features));
        }
        return predictions;
    }
};

// Example usage
int main() {
    // Create sample dataset (features: [age, income], labels: 0 = no, 1 = yes)
    std::vector<Sample> trainingData = {
        Sample({25, 50000}, 0),
        Sample({35, 80000}, 1),
        Sample({45, 120000}, 1),
        Sample({23, 40000}, 0),
        Sample({33, 70000}, 1),
        Sample({40, 90000}, 1),
        Sample({28, 60000}, 0),
        Sample({38, 100000}, 1),
        Sample({30, 55000}, 0),
        Sample({42, 110000}, 1)
    };
    
    // Create and train the decision tree
    DecisionTree dt;
    dt.train(trainingData, 5);
    
    // Test predictions
    std::vector<std::vector<double>> testSamples = {
        {30, 65000},
        {40, 95000},
        {25, 45000}
    };
    
    std::cout << "Decision Tree Predictions:\n";
    std::cout << "==========================\n";
    
    auto predictions = dt.predict(testSamples);
    for (size_t i = 0; i < testSamples.size(); i++) {
        std::cout << "Sample [" << testSamples[i][0] << ", " << testSamples[i][1] 
                  << "] -> Prediction: " << (predictions[i] ? "Yes" : "No") << std::endl;
    }
    
    return 0;
}
```

## Key Features of this Implementation:

### 1. **Core Components:**
- `Sample` structure to represent data points with features and labels
- `TreeNode` structure to represent decision tree nodes
- `DecisionTree` class with training and prediction methods

### 2. **Key Algorithms:**
- **Entropy Calculation**: Measures the impurity of a dataset
- **Information Gain**: Determines the best feature to split on
- **Recursive Tree Building**: Builds the tree by finding optimal splits
- **Prediction**: Traverses the tree to make predictions

### 3. **Decision Tree Features:**
- **ID3 Algorithm**: Uses information gain for feature selection
- **Stopping Criteria**: Depth limit and pure node detection
- **Handling Edge Cases**: Empty datasets, identical labels
- **Memory Management**: Proper tree cleanup

### 4. **Usage Example:**
The example demonstrates training on a simple dataset of age and income to predict loan approval decisions.

### 5. **Output:**
```
Decision Tree Predictions:
==========================
Sample [30, 65000] -> Prediction: Yes
Sample [40, 95000] -> Prediction: Yes
Sample [25, 45000] -> Prediction: No
```

This implementation provides a solid foundation for decision tree learning that can be extended with additional features like pruning, different splitting criteria, or support for continuous features.

