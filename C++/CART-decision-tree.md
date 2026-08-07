# CART Decision Tree Algorithm in C++

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <climits>
#include <cmath>

// Structure to represent a data point
struct DataPoint {
    std::vector<double> features;
    int label;
    
    DataPoint(const std::vector<double>& f, int l) : features(f), label(l) {}
};

// Structure to represent a decision tree node
struct TreeNode {
    bool isLeaf;
    int prediction;  // For leaf nodes
    int featureIndex;  // For internal nodes
    double threshold;  // For internal nodes
    TreeNode* left;
    TreeNode* right;
    
    TreeNode() : isLeaf(false), prediction(0), featureIndex(-1), threshold(0.0), left(nullptr), right(nullptr) {}
};

class CARTDecisionTree {
private:
    TreeNode* root;
    int maxDepth;
    int minSamplesSplit;
    
    // Calculate Gini impurity
    double calculateGini(const std::vector<int>& labels) {
        if (labels.empty()) return 0.0;
        
        std::map<int, int> labelCount;
        for (int label : labels) {
            labelCount[label]++;
        }
        
        double gini = 1.0;
        int totalSamples = labels.size();
        
        for (const auto& pair : labelCount) {
            double probability = static_cast<double>(pair.second) / totalSamples;
            gini -= probability * probability;
        }
        
        return gini;
    }
    
    // Find best split for a feature
    std::pair<double, double> findBestSplit(const std::vector<DataPoint*>& data, int featureIndex) {
        std::vector<std::pair<double, int>> sortedData;
        for (const auto& point : data) {
            sortedData.push_back({point->features[featureIndex], point->label});
        }
        
        std::sort(sortedData.begin(), sortedData.end());
        
        double bestGini = INT_MAX;
        double bestThreshold = 0.0;
        
        int leftCount = 0;
        int rightCount = data.size();
        std::map<int, int> leftLabels;
        std::map<int, int> rightLabels;
        
        // Initialize right labels
        for (const auto& point : data) {
            rightLabels[point->label]++;
        }
        
        for (int i = 0; i < sortedData.size() - 1; i++) {
            double currentThreshold = (sortedData[i].first + sortedData[i+1].first) / 2.0;
            
            // Move one element from right to left
            int label = sortedData[i].second;
            leftLabels[label]++;
            rightLabels[label]--;
            if (rightLabels[label] == 0) {
                rightLabels.erase(label);
            }
            
            leftCount++;
            rightCount--;
            
            if (leftCount > 0 && rightCount > 0) {
                // Calculate weighted Gini impurity
                double leftGini = calculateGini(getLabelsFromMap(leftLabels));
                double rightGini = calculateGini(getLabelsFromMap(rightLabels));
                
                double weightedGini = (static_cast<double>(leftCount) / data.size()) * leftGini +
                                    (static_cast<double>(rightCount) / data.size()) * rightGini;
                
                if (weightedGini < bestGini) {
                    bestGini = weightedGini;
                    bestThreshold = currentThreshold;
                }
            }
        }
        
        return {bestGini, bestThreshold};
    }
    
    std::vector<int> getLabelsFromMap(const std::map<int, int>& labelMap) {
        std::vector<int> labels;
        for (const auto& pair : labelMap) {
            for (int i = 0; i < pair.second; i++) {
                labels.push_back(pair.first);
            }
        }
        return labels;
    }
    
    // Build the decision tree recursively
    TreeNode* buildTree(const std::vector<DataPoint*>& data, int depth) {
        if (data.empty()) {
            TreeNode* node = new TreeNode();
            node->isLeaf = true;
            node->prediction = 0;  // Default prediction
            return node;
        }
        
        // Check stopping criteria
        if (depth >= maxDepth || data.size() < minSamplesSplit) {
            TreeNode* node = new TreeNode();
            node->isLeaf = true;
            node->prediction = getMajorityClass(data);
            return node;
        }
        
        // Find best feature and threshold
        int numFeatures = data[0]->features.size();
        double bestGini = INT_MAX;
        int bestFeature = -1;
        double bestThreshold = 0.0;
        
        for (int featureIndex = 0; featureIndex < numFeatures; featureIndex++) {
            auto result = findBestSplit(data, featureIndex);
            if (result.first < bestGini) {
                bestGini = result.first;
                bestFeature = featureIndex;
                bestThreshold = result.second;
            }
        }
        
        // Split data
        std::vector<DataPoint*> leftData, rightData;
        for (const auto& point : data) {
            if (point->features[bestFeature] <= bestThreshold) {
                leftData.push_back(point);
            } else {
                rightData.push_back(point);
            }
        }
        
        // Create node
        TreeNode* node = new TreeNode();
        node->featureIndex = bestFeature;
        node->threshold = bestThreshold;
        
        // Recursively build subtrees
        node->left = buildTree(leftData, depth + 1);
        node->right = buildTree(rightData, depth + 1);
        
        return node;
    }
    
    int getMajorityClass(const std::vector<DataPoint*>& data) {
        std::map<int, int> classCount;
        for (const auto& point : data) {
            classCount[point->label]++;
        }
        
        int majorityClass = 0;
        int maxCount = 0;
        for (const auto& pair : classCount) {
            if (pair.second > maxCount) {
                maxCount = pair.second;
                majorityClass = pair.first;
            }
        }
        
        return majorityClass;
    }
    
    // Make prediction
    int predictSingle(const std::vector<double>& features, TreeNode* node) {
        if (node->isLeaf) {
            return node->prediction;
        }
        
        if (features[node->featureIndex] <= node->threshold) {
            return predictSingle(features, node->left);
        } else {
            return predictSingle(features, node->right);
        }
    }
    
public:
    CARTDecisionTree(int maxDepth = 10, int minSamplesSplit = 2) 
        : root(nullptr), maxDepth(maxDepth), minSamplesSplit(minSamplesSplit) {}
    
    // Train the decision tree
    void fit(const std::vector<DataPoint*>& data) {
        root = buildTree(data, 0);
    }
    
    // Make prediction
    int predict(const std::vector<double>& features) {
        return predictSingle(features, root);
    }
    
    // Predict multiple samples
    std::vector<int> predict(const std::vector<std::vector<double>>& featuresList) {
        std::vector<int> predictions;
        for (const auto& features : featuresList) {
            predictions.push_back(predict(features));
        }
        return predictions;
    }
    
    // Print tree structure (for debugging)
    void printTree() {
        printTreeHelper(root, 0);
    }
    
private:
    void printTreeHelper(TreeNode* node, int depth) {
        if (node == nullptr) return;
        
        for (int i = 0; i < depth; i++) {
            std::cout << "  ";
        }
        
        if (node->isLeaf) {
            std::cout << "Predict: " << node->prediction << std::endl;
        } else {
            std::cout << "Feature " << node->featureIndex << " <= " << node->threshold << std::endl;
            printTreeHelper(node->left, depth + 1);
            printTreeHelper(node->right, depth + 1);
        }
    }
};

// Example usage
int main() {
    // Create sample dataset (features: [sepal_length, sepal_width, petal_length, petal_width])
    std::vector<DataPoint*> dataset;
    
    // Sample data points (simplified iris-like dataset)
    dataset.push_back(new DataPoint({5.1, 3.5, 1.4, 0.2}, 0));  // Class 0
    dataset.push_back(new DataPoint({4.9, 3.0, 1.4, 0.2}, 0));  // Class 0
    dataset.push_back(new DataPoint({6.2, 2.8, 4.8, 1.8}, 1));  // Class 1
    dataset.push_back(new DataPoint({5.6, 2.7, 4.2, 1.3}, 1));  // Class 1
    dataset.push_back(new DataPoint({6.3, 3.3, 6.0, 2.5}, 2));  // Class 2
    dataset.push_back(new DataPoint({5.8, 2.7, 5.1, 1.9}, 2));  // Class 2
    
    // Create and train CART decision tree
    CARTDecisionTree tree(3, 2);  // maxDepth=3, minSamplesSplit=2
    tree.fit(dataset);
    
    std::cout << "Decision Tree Structure:" << std::endl;
    tree.printTree();
    
    // Make predictions
    std::vector<std::vector<double>> testSamples = {
        {5.0, 3.4, 1.5, 0.2},
        {6.0, 2.9, 4.5, 1.5},
        {6.5, 3.0, 5.2, 2.0}
    };
    
    std::cout << "\nPredictions:" << std::endl;
    auto predictions = tree.predict(testSamples);
    for (int i = 0; i < testSamples.size(); i++) {
        std::cout << "Sample " << i+1 << ": Class " << predictions[i] << std::endl;
    }
    
    // Clean up memory
    for (auto& point : dataset) {
        delete point;
    }
    
    return 0;
}
```

## Key Features of this CART Implementation:

### **Core Components:**
1. **DataPoint structure**: Represents individual data samples with features and labels
2. **TreeNode structure**: Represents nodes in the decision tree
3. **CARTDecisionTree class**: Main implementation with training and prediction methods

### **Algorithm Features:**
- **Gini Impurity**: Used for measuring node purity during splitting
- **Best Feature Selection**: Finds optimal feature to split on
- **Optimal Threshold**: Determines best threshold value for each feature
- **Stopping Criteria**: Depth limit and minimum samples required for splitting
- **Recursive Tree Building**: Builds tree by recursively splitting data

### **Key Methods:**
- `fit()`: Trains the decision tree on training data
- `predict()`: Makes predictions on new samples
- `buildTree()`: Recursively builds the decision tree
- `findBestSplit()`: Finds optimal split for a feature
- `calculateGini()`: Computes Gini impurity

### **Usage:**
The example demonstrates training on a small dataset and making predictions on new samples, showing how CART builds a binary decision tree using Gini impurity as the splitting criterion.