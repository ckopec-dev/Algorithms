# Random Forest Algorithm in C++

Here's a complete implementation of a Random Forest algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <random>
#include <algorithm>
#include <cmath>
#include <memory>

// Structure to represent a data point
struct DataPoint {
    std::vector<double> features;
    int label;
    
    DataPoint(const std::vector<double>& f, int l) : features(f), label(l) {}
};

// Node structure for Decision Tree
struct TreeNode {
    bool is_leaf;
    int prediction;
    int feature_index;
    double threshold;
    std::unique_ptr<TreeNode> left;
    std::unique_ptr<TreeNode> right;
    
    TreeNode() : is_leaf(false), prediction(0), feature_index(-1), threshold(0.0) {}
};

// Decision Tree implementation
class DecisionTree {
private:
    int max_depth;
    int min_samples_split;
    int num_features;
    
    std::mt19937 rng;
    
    TreeNode* build_tree(const std::vector<DataPoint>& data, int depth) {
        TreeNode* node = new TreeNode();
        
        // Base cases
        if (depth >= max_depth || data.size() < min_samples_split) {
            node->is_leaf = true;
            node->prediction = majority_vote(data);
            return node;
        }
        
        // Find best split
        auto best_split = find_best_split(data);
        
        if (best_split.gain <= 0) {
            node->is_leaf = true;
            node->prediction = majority_vote(data);
            return node;
        }
        
        node->feature_index = best_split.feature_index;
        node->threshold = best_split.threshold;
        
        // Split data
        std::vector<DataPoint> left_data, right_data;
        for (const auto& point : data) {
            if (point.features[best_split.feature_index] <= best_split.threshold) {
                left_data.push_back(point);
            } else {
                right_data.push_back(point);
            }
        }
        
        // Recursively build left and right subtrees
        if (!left_data.empty()) {
            node->left = std::unique_ptr<TreeNode>(build_tree(left_data, depth + 1));
        }
        if (!right_data.empty()) {
            node->right = std::unique_ptr<TreeNode>(build_tree(right_data, depth + 1));
        }
        
        return node;
    }
    
    struct SplitResult {
        double gain;
        int feature_index;
        double threshold;
    };
    
    SplitResult find_best_split(const std::vector<DataPoint>& data) {
        SplitResult best_result = {0.0, -1, 0.0};
        double best_gini = 1.0;
        
        // Randomly select features
        std::vector<int> feature_indices(num_features);
        std::iota(feature_indices.begin(), feature_indices.end(), 0);
        std::shuffle(feature_indices.begin(), feature_indices.end(), rng);
        
        int num_features_to_consider = std::min(5, static_cast<int>(num_features));
        
        for (int i = 0; i < num_features_to_consider; i++) {
            int feature_idx = feature_indices[i];
            std::vector<double> values;
            for (const auto& point : data) {
                values.push_back(point.features[feature_idx]);
            }
            std::sort(values.begin(), values.end());
            
            // Try different thresholds
            for (size_t j = 1; j < values.size(); j++) {
                double threshold = (values[j-1] + values[j]) / 2.0;
                double gini = calculate_gini(data, feature_idx, threshold);
                
                if (gini < best_gini) {
                    best_gini = gini;
                    best_result.feature_index = feature_idx;
                    best_result.threshold = threshold;
                    best_result.gain = 1.0 - best_gini;
                }
            }
        }
        
        return best_result;
    }
    
    double calculate_gini(const std::vector<DataPoint>& data, int feature_idx, double threshold) {
        std::vector<int> left_labels, right_labels;
        
        for (const auto& point : data) {
            if (point.features[feature_idx] <= threshold) {
                left_labels.push_back(point.label);
            } else {
                right_labels.push_back(point.label);
            }
        }
        
        if (left_labels.empty() || right_labels.empty()) return 1.0;
        
        double left_gini = calculate_class_gini(left_labels);
        double right_gini = calculate_class_gini(right_labels);
        
        double left_weight = static_cast<double>(left_labels.size()) / data.size();
        double right_weight = static_cast<double>(right_labels.size()) / data.size();
        
        return left_weight * left_gini + right_weight * right_gini;
    }
    
    double calculate_class_gini(const std::vector<int>& labels) {
        if (labels.empty()) return 0.0;
        
        std::vector<int> counts(3, 0); // Assuming 3 classes
        for (int label : labels) {
            counts[label]++;
        }
        
        double gini = 1.0;
        for (int count : counts) {
            if (count > 0) {
                double prob = static_cast<double>(count) / labels.size();
                gini -= prob * prob;
            }
        }
        
        return gini;
    }
    
    int majority_vote(const std::vector<DataPoint>& data) {
        std::vector<int> counts(3, 0);
        for (const auto& point : data) {
            counts[point.label]++;
        }
        
        return std::distance(counts.begin(), std::max_element(counts.begin(), counts.end()));
    }
    
    int predict_single(const TreeNode* node, const std::vector<double>& features) const {
        if (node->is_leaf) {
            return node->prediction;
        }
        
        if (features[node->feature_index] <= node->threshold) {
            if (node->left) {
                return predict_single(node->left.get(), features);
            }
        } else {
            if (node->right) {
                return predict_single(node->right.get(), features);
            }
        }
        
        // Fallback
        return 0;
    }
    
public:
    std::unique_ptr<TreeNode> root;
    
    DecisionTree(int max_d = 10, int min_samples = 2, int n_features = 5) 
        : max_depth(max_d), min_samples_split(min_samples), num_features(n_features), 
          rng(std::random_device{}()) {}
    
    void train(const std::vector<DataPoint>& data) {
        root = std::unique_ptr<TreeNode>(build_tree(data, 0));
    }
    
    int predict(const std::vector<double>& features) const {
        if (!root) return 0;
        return predict_single(root.get(), features);
    }
};

// Random Forest implementation
class RandomForest {
private:
    std::vector<std::unique_ptr<DecisionTree>> trees;
    int num_trees;
    int num_features;
    
public:
    RandomForest(int n_trees = 10, int n_features = 5) 
        : num_trees(n_trees), num_features(n_features) {}
    
    void train(const std::vector<DataPoint>& data) {
        trees.clear();
        
        for (int i = 0; i < num_trees; i++) {
            trees.push_back(std::make_unique<DecisionTree>(10, 2, num_features));
            
            // Bootstrap sampling
            std::vector<DataPoint> bootstrap_data = bootstrap_sample(data);
            trees[i]->train(bootstrap_data);
        }
    }
    
    int predict(const std::vector<double>& features) const {
        std::vector<int> predictions(num_trees, 0);
        
        for (int i = 0; i < num_trees; i++) {
            predictions[i] = trees[i]->predict(features);
        }
        
        // Return majority vote
        std::vector<int> counts(3, 0);
        for (int pred : predictions) {
            counts[pred]++;
        }
        
        return std::distance(counts.begin(), std::max_element(counts.begin(), counts.end()));
    }
    
private:
    std::vector<DataPoint> bootstrap_sample(const std::vector<DataPoint>& data) {
        std::vector<DataPoint> sample;
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<> dis(0, data.size() - 1);
        
        for (size_t i = 0; i < data.size(); i++) {
            sample.push_back(data[dis(gen)]);
        }
        
        return sample;
    }
};

// Example usage
int main() {
    // Create sample dataset (simple 2D classification)
    std::vector<DataPoint> data = {
        {{1.0, 2.0}, 0},
        {{2.0, 3.0}, 0},
        {{3.0, 1.0}, 0},
        {{4.0, 2.0}, 1},
        {{5.0, 3.0}, 1},
        {{6.0, 1.0}, 1},
        {{7.0, 2.0}, 2},
        {{8.0, 3.0}, 2},
        {{9.0, 1.0}, 2}
    };
    
    // Create and train Random Forest
    RandomForest rf(5, 2); // 5 trees, 2 features
    rf.train(data);
    
    // Make predictions
    std::vector<std::vector<double>> test_points = {
        {1.5, 2.5},
        {5.5, 2.5},
        {8.5, 1.5}
    };
    
    std::cout << "Random Forest Predictions:\n";
    for (const auto& point : test_points) {
        int prediction = rf.predict(point);
        std::cout << "Point (" << point[0] << ", " << point[1] << ") -> Class: " << prediction << std::endl;
    }
    
    return 0;
}
```

## Key Features of this Implementation:

### 1. **Decision Tree Structure**
- Binary tree with feature splitting
- Gini impurity for measuring node purity
- Random feature selection for each split

### 2. **Random Forest Features**
- Bootstrap sampling for each tree
- Random feature selection (5 features in this example)
- Majority voting for final prediction

### 3. **Key Components**
- **DataPoint**: Represents training samples with features and labels
- **TreeNode**: Tree node structure with decision logic
- **DecisionTree**: Individual decision tree with training and prediction
- **RandomForest**: Ensemble of decision trees with voting mechanism

### 4. **Training Process**
- Bootstrap sampling for each tree
- Random feature selection at each split
- Tree depth limiting to prevent overfitting

### 5. **Prediction**
- Each tree makes a prediction
- Final prediction is determined by majority voting

This implementation provides a solid foundation for a Random Forest classifier that can be extended with additional features like cross-validation, different splitting criteria, or more sophisticated ensemble methods.

