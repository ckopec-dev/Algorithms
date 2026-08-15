# Gradient Boosting Machines (GBM) Implementation in C++

Here's a simplified implementation of a Gradient Boosting Machine using C++:

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <random>
#include <cmath>

// Simple decision tree node structure
struct TreeNode {
    int feature_index;
    double threshold;
    double prediction;
    bool is_leaf;
    TreeNode* left;
    TreeNode* right;
    
    TreeNode() : feature_index(-1), threshold(0.0), prediction(0.0), 
                is_leaf(false), left(nullptr), right(nullptr) {}
};

// Simple decision tree for regression
class DecisionTree {
private:
    TreeNode* root;
    int max_depth;
    int min_samples_split;
    
    TreeNode* build_tree(const std::vector<std::vector<double>>& X,
                        const std::vector<double>& y,
                        int depth, int sample_size) {
        TreeNode* node = new TreeNode();
        
        // Stopping criteria
        if (depth >= max_depth || sample_size <= min_samples_split || 
            y.size() == 0) {
            node->is_leaf = true;
            node->prediction = calculate_mean(y);
            return node;
        }
        
        // Find best split
        int best_feature = -1;
        double best_threshold = 0.0;
        double best_loss = std::numeric_limits<double>::max();
        
        for (int feature = 0; feature < X[0].size(); feature++) {
            std::vector<std::pair<double, double>> data_pairs;
            for (int i = 0; i < X.size(); i++) {
                data_pairs.push_back({X[i][feature], y[i]});
            }
            
            std::sort(data_pairs.begin(), data_pairs.end());
            
            for (int i = 1; i < data_pairs.size(); i++) {
                double threshold = (data_pairs[i-1].first + data_pairs[i].first) / 2.0;
                double loss = calculate_split_loss(X, y, feature, threshold);
                
                if (loss < best_loss) {
                    best_loss = loss;
                    best_feature = feature;
                    best_threshold = threshold;
                }
            }
        }
        
        if (best_feature == -1) {
            node->is_leaf = true;
            node->prediction = calculate_mean(y);
            return node;
        }
        
        node->feature_index = best_feature;
        node->threshold = best_threshold;
        
        // Split data
        std::vector<std::vector<double>> left_X, right_X;
        std::vector<double> left_y, right_y;
        
        for (int i = 0; i < X.size(); i++) {
            if (X[i][best_feature] <= best_threshold) {
                left_X.push_back(X[i]);
                left_y.push_back(y[i]);
            } else {
                right_X.push_back(X[i]);
                right_y.push_back(y[i]);
            }
        }
        
        node->left = build_tree(left_X, left_y, depth + 1, left_X.size());
        node->right = build_tree(right_X, right_y, depth + 1, right_X.size());
        
        return node;
    }
    
    double calculate_mean(const std::vector<double>& values) {
        if (values.empty()) return 0.0;
        double sum = 0.0;
        for (double val : values) sum += val;
        return sum / values.size();
    }
    
    double calculate_split_loss(const std::vector<std::vector<double>>& X,
                               const std::vector<double>& y,
                               int feature, double threshold) {
        std::vector<double> left_y, right_y;
        
        for (int i = 0; i < X.size(); i++) {
            if (X[i][feature] <= threshold) {
                left_y.push_back(y[i]);
            } else {
                right_y.push_back(y[i]);
            }
        }
        
        double left_mean = calculate_mean(left_y);
        double right_mean = calculate_mean(right_y);
        
        double loss = 0.0;
        for (double val : left_y) loss += std::pow(val - left_mean, 2);
        for (double val : right_y) loss += std::pow(val - right_mean, 2);
        
        return loss;
    }
    
public:
    DecisionTree(int max_depth = 3, int min_samples_split = 2) 
        : root(nullptr), max_depth(max_depth), min_samples_split(min_samples_split) {}
    
    void fit(const std::vector<std::vector<double>>& X,
             const std::vector<double>& y) {
        root = build_tree(X, y, 0, X.size());
    }
    
    double predict(const std::vector<double>& x) {
        TreeNode* current = root;
        while (!current->is_leaf) {
            if (x[current->feature_index] <= current->threshold) {
                current = current->left;
            } else {
                current = current->right;
            }
        }
        return current->prediction;
    }
    
    void print_tree() {
        print_node(root, 0);
    }
    
private:
    void print_node(TreeNode* node, int depth) {
        if (node == nullptr) return;
        
        for (int i = 0; i < depth; i++) std::cout << "  ";
        
        if (node->is_leaf) {
            std::cout << "Prediction: " << node->prediction << std::endl;
        } else {
            std::cout << "Feature " << node->feature_index 
                      << " <= " << node->threshold << std::endl;
            print_node(node->left, depth + 1);
            print_node(node->right, depth + 1);
        }
    }
};

// Gradient Boosting Machine
class GradientBoostingRegressor {
private:
    std::vector<DecisionTree*> trees;
    double learning_rate;
    int n_estimators;
    double initial_prediction;
    
public:
    GradientBoostingRegressor(int n_estimators = 100, double learning_rate = 0.1) 
        : n_estimators(n_estimators), learning_rate(learning_rate) {}
    
    void fit(const std::vector<std::vector<double>>& X,
             const std::vector<double>& y) {
        
        // Initialize with mean of target values
        initial_prediction = 0.0;
        for (double val : y) initial_prediction += val;
        initial_prediction /= y.size();
        
        double current_prediction = initial_prediction;
        
        // Build trees sequentially
        for (int i = 0; i < n_estimators; i++) {
            std::vector<double> residuals;
            
            // Calculate residuals (actual - current prediction)
            for (int j = 0; j < X.size(); j++) {
                double pred = predict_single(X[j], i);
                residuals.push_back(y[j] - pred);
            }
            
            // Train a new tree on residuals
            DecisionTree* tree = new DecisionTree(3, 2);
            tree->fit(X, residuals);
            trees.push_back(tree);
            
            // Update prediction with learning rate
            current_prediction += learning_rate * predict_single(X[0], i + 1);
        }
    }
    
    double predict(const std::vector<double>& x) {
        double prediction = initial_prediction;
        
        for (int i = 0; i < trees.size(); i++) {
            prediction += learning_rate * trees[i]->predict(x);
        }
        
        return prediction;
    }
    
    double predict_single(const std::vector<double>& x, int tree_index) {
        if (tree_index >= trees.size()) return 0.0;
        return trees[tree_index]->predict(x);
    }
    
    void print_models() {
        std::cout << "GBM Model with " << trees.size() << " trees:" << std::endl;
        for (int i = 0; i < trees.size(); i++) {
            std::cout << "Tree " << i << ":" << std::endl;
            // Note: This is simplified - full implementation would require
            // more complex tree printing logic
        }
    }
};

// Example usage
int main() {
    // Sample data (X, y)
    std::vector<std::vector<double>> X = {
        {1.0, 2.0},
        {2.0, 3.0},
        {3.0, 4.0},
        {4.0, 5.0},
        {5.0, 6.0}
    };
    
    std::vector<double> y = {3.0, 5.0, 7.0, 9.0, 11.0};
    
    // Create and train GBM
    GradientBoostingRegressor gbm(5, 0.1);
    gbm.fit(X, y);
    
    // Make predictions
    std::vector<double> test_sample = {2.5, 3.5};
    double prediction = gbm.predict(test_sample);
    
    std::cout << "Prediction for [2.5, 3.5]: " << prediction << std::endl;
    
    return 0;
}
```

## Key Components Explained:

### 1. **DecisionTree Class**
- Implements a simple decision tree for regression
- Uses mean squared error for splitting criteria
- Recursive tree building with stopping conditions

### 2. **GradientBoostingRegressor Class**
- Sequentially builds decision trees
- Each tree learns to correct the residuals of previous trees
- Uses gradient descent principle with learning rate

### 3. **Core GBM Algorithm Steps**
1. Initialize predictions with mean of target values
2. For each iteration:
   - Calculate residuals (actual - current predictions)
   - Train new decision tree on residuals
   - Update predictions using learning rate
3. Final prediction = initial + sum(learning_rate × tree_predictions)

## Features Implemented:
- Gradient boosting with decision trees
- Residual calculation and correction
- Learning rate control
- Basic tree stopping criteria
- Sequential model building

This is a simplified implementation for educational purposes. A production-ready GBM would include features like:
- More sophisticated tree splitting criteria (information gain, Gini)
- Regularization terms
- Cross-validation for hyperparameter tuning
- Better memory management
- Parallel processing capabilities