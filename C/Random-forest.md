# Random Forest Implementation in C

Here's a simplified example of a Random Forest algorithm implemented in C. This example demonstrates the core concepts of ensemble learning with decision trees.

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

// Structure for a decision tree node
typedef struct TreeNode {
    int feature_index;
    float threshold;
    int prediction;
    struct TreeNode* left;
    struct TreeNode* right;
} TreeNode;

// Structure for a decision tree
typedef struct DecisionTree {
    TreeNode* root;
    int max_depth;
    int min_samples_split;
} DecisionTree;

// Structure for Random Forest
typedef struct RandomForest {
    DecisionTree** trees;
    int num_trees;
    int num_features;
} RandomForest;

// Function to create a new tree node
TreeNode* create_node() {
    TreeNode* node = (TreeNode*)malloc(sizeof(TreeNode));
    node->feature_index = -1;
    node->threshold = 0.0f;
    node->prediction = 0;
    node->left = NULL;
    node->right = NULL;
    return node;
}

// Simple function to calculate variance (for splitting)
float calculate_variance(float* values, int size) {
    if (size <= 1) return 0.0f;
    
    float mean = 0.0f;
    for (int i = 0; i < size; i++) {
        mean += values[i];
    }
    mean /= size;
    
    float variance = 0.0f;
    for (int i = 0; i < size; i++) {
        variance += (values[i] - mean) * (values[i] - mean);
    }
    variance /= size;
    
    return variance;
}

// Simple splitting function
int find_best_split(float** features, int* labels, int num_samples, int num_features, 
                   int* best_feature, float* best_threshold) {
    float best_variance = 1e9f;
    int best_feature_idx = -1;
    float best_threshold_val = 0.0f;
    
    for (int f = 0; f < num_features; f++) {
        // Sort samples by feature value
        int* indices = (int*)malloc(num_samples * sizeof(int));
        for (int i = 0; i < num_samples; i++) {
            indices[i] = i;
        }
        
        // Simple bubble sort for demonstration
        for (int i = 0; i < num_samples - 1; i++) {
            for (int j = 0; j < num_samples - i - 1; j++) {
                if (features[j][f] > features[j+1][f]) {
                    int temp = indices[j];
                    indices[j] = indices[j+1];
                    indices[j+1] = temp;
                }
            }
        }
        
        // Try different thresholds
        for (int i = 1; i < num_samples; i++) {
            float threshold = (features[indices[i-1]][f] + features[indices[i]][f]) / 2.0f;
            
            // Split data
            int left_count = 0, right_count = 0;
            float* left_values = (float*)malloc(num_samples * sizeof(float));
            float* right_values = (float*)malloc(num_samples * sizeof(float));
            
            for (int j = 0; j < num_samples; j++) {
                if (features[j][f] <= threshold) {
                    left_values[left_count++] = labels[j];
                } else {
                    right_values[right_count++] = labels[j];
                }
            }
            
            if (left_count > 0 && right_count > 0) {
                float left_var = calculate_variance(left_values, left_count);
                float right_var = calculate_variance(right_values, right_count);
                float weighted_var = (left_count * left_var + right_count * right_var) / num_samples;
                
                if (weighted_var < best_variance) {
                    best_variance = weighted_var;
                    best_feature_idx = f;
                    best_threshold_val = threshold;
                }
            }
            
            free(left_values);
            free(right_values);
        }
        
        free(indices);
    }
    
    *best_feature = best_feature_idx;
    *best_threshold = best_threshold_val;
    return (best_feature_idx != -1);
}

// Build a decision tree recursively
TreeNode* build_tree(float** features, int* labels, int num_samples, int num_features,
                    int depth, int max_depth, int min_samples_split) {
    TreeNode* node = create_node();
    
    // Check stopping criteria
    if (depth >= max_depth || num_samples < min_samples_split) {
        // Make a prediction (majority class)
        int class_count[2] = {0, 0};
        for (int i = 0; i < num_samples; i++) {
            class_count[labels[i]]++;
        }
        node->prediction = (class_count[1] > class_count[0]) ? 1 : 0;
        return node;
    }
    
    // Find best split
    int best_feature;
    float best_threshold;
    if (!find_best_split(features, labels, num_samples, num_features, &best_feature, &best_threshold)) {
        // Make a prediction (majority class)
        int class_count[2] = {0, 0};
        for (int i = 0; i < num_samples; i++) {
            class_count[labels[i]]++;
        }
        node->prediction = (class_count[1] > class_count[0]) ? 1 : 0;
        return node;
    }
    
    node->feature_index = best_feature;
    node->threshold = best_threshold;
    
    // Split data
    int left_count = 0, right_count = 0;
    int* left_indices = (int*)malloc(num_samples * sizeof(int));
    int* right_indices = (int*)malloc(num_samples * sizeof(int));
    
    for (int i = 0; i < num_samples; i++) {
        if (features[i][best_feature] <= best_threshold) {
            left_indices[left_count++] = i;
        } else {
            right_indices[right_count++] = i;
        }
    }
    
    // Recursively build left and right subtrees
    if (left_count > 0) {
        float** left_features = (float**)malloc(left_count * sizeof(float*));
        int* left_labels = (int*)malloc(left_count * sizeof(int));
        for (int i = 0; i < left_count; i++) {
            left_features[i] = features[left_indices[i]];
            left_labels[i] = labels[left_indices[i]];
        }
        node->left = build_tree(left_features, left_labels, left_count, num_features, 
                               depth + 1, max_depth, min_samples_split);
        free(left_features);
        free(left_labels);
    } else {
        node->left = create_node();
        node->left->prediction = 0;
    }
    
    if (right_count > 0) {
        float** right_features = (float**)malloc(right_count * sizeof(float*));
        int* right_labels = (int*)malloc(right_count * sizeof(int));
        for (int i = 0; i < right_count; i++) {
            right_features[i] = features[right_indices[i]];
            right_labels[i] = labels[right_indices[i]];
        }
        node->right = build_tree(right_features, right_labels, right_count, num_features, 
                                depth + 1, max_depth, min_samples_split);
        free(right_features);
        free(right_labels);
    } else {
        node->right = create_node();
        node->right->prediction = 0;
    }
    
    free(left_indices);
    free(right_indices);
    
    return node;
}

// Make prediction for a single sample
int predict_sample(TreeNode* root, float* sample) {
    if (root->feature_index == -1) {
        return root->prediction;
    }
    
    if (sample[root->feature_index] <= root->threshold) {
        return predict_sample(root->left, sample);
    } else {
        return predict_sample(root->right, sample);
    }
}

// Create a new decision tree
DecisionTree* create_decision_tree(int max_depth, int min_samples_split) {
    DecisionTree* tree = (DecisionTree*)malloc(sizeof(DecisionTree));
    tree->root = NULL;
    tree->max_depth = max_depth;
    tree->min_samples_split = min_samples_split;
    return tree;
}

// Train a decision tree
void train_tree(DecisionTree* tree, float** features, int* labels, int num_samples, int num_features) {
    tree->root = build_tree(features, labels, num_samples, num_features, 0, tree->max_depth, tree->min_samples_split);
}

// Make prediction using a single tree
int predict_tree(DecisionTree* tree, float* sample) {
    return predict_sample(tree->root, sample);
}

// Create a new random forest
RandomForest* create_random_forest(int num_trees, int max_depth, int min_samples_split) {
    RandomForest* rf = (RandomForest*)malloc(sizeof(RandomForest));
    rf->trees = (DecisionTree**)malloc(num_trees * sizeof(DecisionTree*));
    rf->num_trees = num_trees;
    rf->num_features = 0;
    
    for (int i = 0; i < num_trees; i++) {
        rf->trees[i] = create_decision_tree(max_depth, min_samples_split);
    }
    
    return rf;
}

// Train the random forest
void train_random_forest(RandomForest* rf, float** features, int* labels, 
                        int num_samples, int num_features) {
    rf->num_features = num_features;
    
    // In a real implementation, you would implement feature bagging here
    // For simplicity, we'll train each tree on the full dataset
    
    for (int i = 0; i < rf->num_trees; i++) {
        train_tree(rf->trees[i], features, labels, num_samples, num_features);
    }
}

// Make prediction using random forest
int predict_random_forest(RandomForest* rf, float* sample) {
    int votes[2] = {0, 0};
    
    for (int i = 0; i < rf->num_trees; i++) {
        int prediction = predict_tree(rf->trees[i], sample);
        votes[prediction]++;
    }
    
    return (votes[1] > votes[0]) ? 1 : 0;
}

// Free memory for decision tree
void free_tree(TreeNode* node) {
    if (node != NULL) {
        free_tree(node->left);
        free_tree(node->right);
        free(node);
    }
}

// Free memory for decision tree
void free_decision_tree(DecisionTree* tree) {
    if (tree != NULL) {
        free_tree(tree->root);
        free(tree);
    }
}

// Free memory for random forest
void free_random_forest(RandomForest* rf) {
    if (rf != NULL) {
        for (int i = 0; i < rf->num_trees; i++) {
            free_decision_tree(rf->trees[i]);
        }
        free(rf->trees);
        free(rf);
    }
}

// Example usage
int main() {
    // Sample data (2 features, 10 samples)
    float features[10][2] = {
        {2.0, 3.0}, {5.0, 4.0}, {9.0, 6.0}, {4.0, 7.0}, {8.0, 1.0},
        {7.0, 2.0}, {1.0, 8.0}, {3.0, 5.0}, {6.0, 9.0}, {10.0, 3.0}
    };
    
    int labels[10] = {0, 1, 1, 0, 1, 0, 1, 0, 1, 0};
    
    // Convert to appropriate format
    float** feature_array = (float**)malloc(10 * sizeof(float*));
    for (int i = 0; i < 10; i++) {
        feature_array[i] = features[i];
    }
    
    // Create and train random forest
    RandomForest* rf = create_random_forest(5, 5, 2);
    train_random_forest(rf, feature_array, labels, 10, 2);
    
    // Make predictions
    float test_sample[2] = {5.0, 5.0};
    int prediction = predict_random_forest(rf, test_sample);
    
    printf("Prediction for sample [5.0, 5.0]: %d\n", prediction);
    
    // Clean up
    free(feature_array);
    free_random_forest(rf);
    
    return 0;
}
```

## Key Components Explained:

### 1. **TreeNode Structure**
- Represents a node in the decision tree
- Contains feature index, threshold, prediction value, and left/right children

### 2. **DecisionTree Structure**
- Contains the root node and tree parameters
- Controls tree building process

### 3. **RandomForest Structure**
- Contains multiple decision trees
- Manages ensemble prediction

### 4. **Core Functions**
- `create_node()`: Creates new tree nodes
- `build_tree()`: Recursively builds decision tree
- `predict_sample()`: Makes prediction for a single sample
- `train_random_forest()`: Trains the ensemble of trees
- `predict_random_forest()`: Makes final prediction using voting

### 5. **Important Features**
- **Ensemble Learning**: Combines multiple decision trees
- **Voting Mechanism**: Aggregates predictions from all trees
- **Memory Management**: Proper allocation and deallocation of memory
- **Basic Splitting**: Uses variance-based splitting criterion

## Compilation and Execution:
```bash
gcc -o random_forest random_forest.c
./random_forest
```

This implementation provides a foundation for understanding how Random Forest works, though it's simplified for educational purposes. A production implementation would include more sophisticated features like:
- Feature bagging (random feature selection)
- Better splitting criteria
- Cross-validation
- More robust memory management
- Parallel processing capabilities

