# CART Decision Tree Implementation in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Structure for a node in the decision tree
typedef struct TreeNode {
    int feature_index;      // Feature index used for splitting
    float threshold;        // Threshold value for splitting
    float prediction;       // Prediction value (for leaf nodes)
    struct TreeNode* left;  // Left child node
    struct TreeNode* right; // Right child node
    int is_leaf;            // Flag indicating if node is leaf
} TreeNode;

// Structure for dataset
typedef struct {
    float** data;
    int* labels;
    int num_samples;
    int num_features;
} Dataset;

// Function to calculate gini impurity
float calculate_gini(int* labels, int start, int end) {
    if (start >= end) return 0.0;
    
    int total = end - start;
    int class_counts[2] = {0, 0}; // Assuming binary classification
    
    for (int i = start; i < end; i++) {
        if (labels[i] >= 0 && labels[i] < 2) {
            class_counts[labels[i]]++;
        }
    }
    
    float gini = 1.0;
    for (int i = 0; i < 2; i++) {
        if (total > 0) {
            float probability = (float)class_counts[i] / total;
            gini -= probability * probability;
        }
    }
    
    return gini;
}

// Function to find best split
void find_best_split(Dataset* dataset, int* sample_indices, int num_samples, 
                     int* best_feature, float* best_threshold, float* best_gini) {
    *best_gini = 1.0;
    *best_feature = -1;
    *best_threshold = 0.0;
    
    // Try each feature
    for (int feature = 0; feature < dataset->num_features; feature++) {
        // Sort samples by feature value
        float* values = (float*)malloc(num_samples * sizeof(float));
        for (int i = 0; i < num_samples; i++) {
            values[i] = dataset->data[sample_indices[i]][feature];
        }
        
        // Simple bubble sort
        for (int i = 0; i < num_samples - 1; i++) {
            for (int j = 0; j < num_samples - i - 1; j++) {
                if (values[j] > values[j + 1]) {
                    float temp = values[j];
                    values[j] = values[j + 1];
                    values[j + 1] = temp;
                }
            }
        }
        
        // Try each possible threshold
        for (int i = 0; i < num_samples - 1; i++) {
            float threshold = (values[i] + values[i + 1]) / 2.0;
            
            // Split samples
            int left_count = 0;
            int right_count = 0;
            int left_labels[1000] = {0}; // Assuming max 1000 samples
            int right_labels[1000] = {0};
            
            for (int j = 0; j < num_samples; j++) {
                if (dataset->data[sample_indices[j]][feature] <= threshold) {
                    left_labels[left_count++] = dataset->labels[sample_indices[j]];
                } else {
                    right_labels[right_count++] = dataset->labels[sample_indices[j]];
                }
            }
            
            // Calculate weighted gini
            float left_gini = calculate_gini(left_labels, 0, left_count);
            float right_gini = calculate_gini(right_labels, 0, right_count);
            
            float weighted_gini = (float)left_count / num_samples * left_gini + 
                                 (float)right_count / num_samples * right_gini;
            
            if (weighted_gini < *best_gini) {
                *best_gini = weighted_gini;
                *best_feature = feature;
                *best_threshold = threshold;
            }
        }
        
        free(values);
    }
}

// Function to create a new tree node
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

// Function to get majority class
int get_majority_class(int* labels, int start, int end) {
    int count[2] = {0, 0};
    
    for (int i = start; i < end; i++) {
        if (labels[i] >= 0 && labels[i] < 2) {
            count[labels[i]]++;
        }
    }
    
    return (count[0] > count[1]) ? 0 : 1;
}

// Recursive function to build the decision tree
TreeNode* build_tree(Dataset* dataset, int* sample_indices, int num_samples, int depth) {
    TreeNode* node = create_node();
    
    // Base cases
    if (num_samples == 0) {
        node->is_leaf = 1;
        node->prediction = 0;
        return node;
    }
    
    // If all samples belong to same class
    int first_label = dataset->labels[sample_indices[0]];
    int all_same = 1;
    for (int i = 1; i < num_samples; i++) {
        if (dataset->labels[sample_indices[i]] != first_label) {
            all_same = 0;
            break;
        }
    }
    
    if (all_same || depth > 10) { // Limit depth to prevent overfitting
        node->is_leaf = 1;
        node->prediction = first_label;
        return node;
    }
    
    // Find best split
    int best_feature;
    float best_threshold;
    float best_gini;
    
    find_best_split(dataset, sample_indices, num_samples, &best_feature, &best_threshold, &best_gini);
    
    // If no good split found, make leaf
    if (best_feature == -1) {
        node->is_leaf = 1;
        node->prediction = get_majority_class(dataset->labels, 0, dataset->num_samples);
        return node;
    }
    
    // Split samples
    int* left_indices = (int*)malloc(num_samples * sizeof(int));
    int* right_indices = (int*)malloc(num_samples * sizeof(int));
    int left_count = 0;
    int right_count = 0;
    
    for (int i = 0; i < num_samples; i++) {
        if (dataset->data[sample_indices[i]][best_feature] <= best_threshold) {
            left_indices[left_count++] = sample_indices[i];
        } else {
            right_indices[right_count++] = sample_indices[i];
        }
    }
    
    // Store split information
    node->feature_index = best_feature;
    node->threshold = best_threshold;
    
    // Recursively build left and right subtrees
    node->left = build_tree(dataset, left_indices, left_count, depth + 1);
    node->right = build_tree(dataset, right_indices, right_count, depth + 1);
    
    free(left_indices);
    free(right_indices);
    
    return node;
}

// Function to make prediction
int predict(TreeNode* root, float* sample) {
    if (root->is_leaf) {
        return (int)root->prediction;
    }
    
    if (sample[root->feature_index] <= root->threshold) {
        return predict(root->left, sample);
    } else {
        return predict(root->right, sample);
    }
}

// Function to print the tree (for visualization)
void print_tree(TreeNode* node, int depth) {
    if (node == NULL) return;
    
    for (int i = 0; i < depth; i++) {
        printf("  ");
    }
    
    if (node->is_leaf) {
        printf("Predict: %d\n", (int)node->prediction);
    } else {
        printf("Feature %d <= %.2f?\n", node->feature_index, node->threshold);
        print_tree(node->left, depth + 1);
        for (int i = 0; i < depth; i++) {
            printf("  ");
        }
        printf("Feature %d > %.2f?\n", node->feature_index, node->threshold);
        print_tree(node->right, depth + 1);
    }
}

// Main function demonstrating the CART algorithm
int main() {
    // Sample dataset (4 samples, 2 features, binary classification)
    float data[4][2] = {{2.0, 3.0}, {5.0, 4.0}, {1.0, 2.0}, {4.0, 5.0}};
    int labels[4] = {0, 1, 0, 1};
    
    // Create dataset structure
    Dataset dataset;
    dataset.num_samples = 4;
    dataset.num_features = 2;
    dataset.data = (float**)malloc(4 * sizeof(float*));
    dataset.labels = labels;
    
    for (int i = 0; i < 4; i++) {
        dataset.data[i] = data[i];
    }
    
    // Create sample indices
    int* sample_indices = (int*)malloc(4 * sizeof(int));
    for (int i = 0; i < 4; i++) {
        sample_indices[i] = i;
    }
    
    // Build the decision tree
    TreeNode* root = build_tree(&dataset, sample_indices, 4, 0);
    
    // Print the tree structure
    printf("Decision Tree Structure:\n");
    print_tree(root, 0);
    
    // Make predictions
    printf("\nPredictions:\n");
    float test_sample[2] = {3.0, 3.5};
    int prediction = predict(root, test_sample);
    printf("Sample [%0.1f, %0.1f] -> Prediction: %d\n", test_sample[0], test_sample[1], prediction);
    
    // Clean up memory
    free(sample_indices);
    free(dataset.data);
    
    return 0;
}
```

## Key Features of this CART Implementation:

1. **Binary Classification**: Supports binary classification tasks
2. **Gini Impurity**: Uses Gini impurity for measuring node purity
3. **Recursive Splitting**: Recursively splits data based on best feature and threshold
4. **Pruning**: Includes depth limiting to prevent overfitting
5. **Prediction**: Makes predictions using the trained tree
6. **Tree Visualization**: Includes function to print tree structure

## How it Works:

1. **Data Preparation**: Stores dataset with features and labels
2. **Split Finding**: For each feature, tries different thresholds to find the best split
3. **Gini Calculation**: Calculates weighted Gini impurity for each potential split
4. **Tree Building**: Recursively builds the tree by splitting on the best features
5. **Prediction**: Traverses the tree to make predictions for new samples

This implementation demonstrates the core CART (Classification and Regression Trees) algorithm in C, showing how decision trees are constructed using the Gini impurity criterion for splitting.

