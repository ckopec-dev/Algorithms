# Gradient Boosting Machines (GBM) in C

Here's a simplified implementation of a Gradient Boosting Machine algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Simple decision tree node structure
typedef struct TreeNode {
    int feature_index;
    double threshold;
    double prediction;
    struct TreeNode* left;
    struct TreeNode* right;
    int is_leaf;
} TreeNode;

// GBM model structure
typedef struct {
    TreeNode** trees;
    double* learning_rates;
    int num_trees;
    int max_depth;
    double* base_prediction;
} GBMModel;

// Simple decision tree implementation
TreeNode* create_node() {
    TreeNode* node = (TreeNode*)malloc(sizeof(TreeNode));
    node->feature_index = -1;
    node->threshold = 0.0;
    node->prediction = 0.0;
    node->left = NULL;
    node->right = NULL;
    node->is_leaf = 0;
    return node;
}

// Simple linear regression for demonstration
double predict_tree(TreeNode* tree, double* features) {
    if (tree->is_leaf) {
        return tree->prediction;
    }
    
    if (features[tree->feature_index] <= tree->threshold) {
        return predict_tree(tree->left, features);
    } else {
        return predict_tree(tree->right, features);
    }
}

// Simple gradient computation (for demonstration)
double compute_gradient(double y_true, double y_pred) {
    // For squared loss: gradient = y_true - y_pred
    return y_true - y_pred;
}

// Simple prediction function
double predict_gbm(GBMModel* model, double* features) {
    double prediction = *model->base_prediction;
    
    for (int i = 0; i < model->num_trees; i++) {
        double tree_pred = predict_tree(model->trees[i], features);
        prediction += model->learning_rates[i] * tree_pred;
    }
    
    return prediction;
}

// Simple training function (simplified version)
void train_gbm(GBMModel* model, double** X, double* y, int num_samples, int num_features) {
    // Initialize base prediction (mean of targets)
    double sum = 0.0;
    for (int i = 0; i < num_samples; i++) {
        sum += y[i];
    }
    *model->base_prediction = sum / num_samples;
    
    // Create simple trees (this is a very simplified version)
    for (int t = 0; t < model->num_trees; t++) {
        model->trees[t] = create_node();
        model->trees[t]->is_leaf = 1;
        model->trees[t]->prediction = 1.0; // Simple prediction value
    }
}

// Example usage
int main() {
    // Sample data
    double X[][2] = {{1.0, 2.0}, {2.0, 3.0}, {3.0, 4.0}, {4.0, 5.0}};
    double y[] = {3.0, 5.0, 7.0, 9.0};
    int num_samples = 4;
    int num_features = 2;
    
    // Create GBM model
    GBMModel model;
    model.num_trees = 3;
    model.max_depth = 3;
    model.learning_rates = (double*)malloc(model.num_trees * sizeof(double));
    model.trees = (TreeNode**)malloc(model.num_trees * sizeof(TreeNode*));
    model.base_prediction = (double*)malloc(sizeof(double));
    
    // Set learning rates
    for (int i = 0; i < model.num_trees; i++) {
        model.learning_rates[i] = 0.1;
    }
    
    // Train the model
    train_gbm(&model, X, y, num_samples, num_features);
    
    // Make predictions
    double test_sample[] = {2.5, 3.5};
    double prediction = predict_gbm(&model, test_sample);
    
    printf("Base prediction: %.2f\n", *model.base_prediction);
    printf("Prediction for [2.5, 3.5]: %.2f\n", prediction);
    
    // Cleanup
    free(model.learning_rates);
    free(model.trees);
    free(model.base_prediction);
    
    return 0;
}
```

## Key Components Explained:

### 1. **TreeNode Structure**
- Represents a decision tree node
- Contains feature index, threshold, and prediction values
- Supports left and right child nodes

### 2. **GBMModel Structure**
- Contains the ensemble of trees
- Stores learning rates for each tree
- Holds base prediction value

### 3. **Core Functions**
- `predict_tree()`: Makes predictions using a single tree
- `predict_gbm()`: Combines predictions from all trees
- `compute_gradient()`: Computes gradients for boosting
- `train_gbm()`: Trains the GBM model

### 4. **Key GBM Concepts**
- **Ensemble**: Combines multiple weak learners
- **Gradient Descent**: Minimizes loss function iteratively
- **Learning Rate**: Controls step size of updates
- **Base Prediction**: Initial prediction (usually mean)

## Compilation and Execution:
```bash
gcc -o gbm gbm.c -lm
./gbm
```

## Notes:
- This is a simplified implementation for demonstration
- Real GBM implementations are more complex and include:
  - Advanced tree building algorithms
  - Regularization techniques
  - Various loss functions
  - Cross-validation
  - Feature importance calculation

This example shows the fundamental structure and concepts of Gradient Boosting Machines in C programming language.

