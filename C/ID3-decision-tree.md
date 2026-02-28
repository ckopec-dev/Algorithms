# ID3 Decision Tree Algorithm in C

Here's a complete implementation of the ID3 algorithm for building decision trees in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAX_ATTRIBUTES 100
#define MAX_VALUES 100
#define MAX_DATA 1000

// Data structure for a single data instance
typedef struct {
    char *values[MAX_ATTRIBUTES];
    char *class_label;
} Instance;

// Data structure for a decision tree node
typedef struct TreeNode {
    char *attribute;           // Attribute name (NULL for leaf nodes)
    char *value;               // Value that leads to this node
    int is_leaf;               // 1 if leaf node, 0 otherwise
    char *prediction;          // Class prediction for leaf nodes
    struct TreeNode *children[MAX_VALUES];  // Children nodes
    int num_children;          // Number of children
} TreeNode;

// Global variables
int num_attributes;
int num_instances;
int num_classes;
char *attribute_names[MAX_ATTRIBUTES];
char *class_names[MAX_ATTRIBUTES];
Instance dataset[MAX_DATA];

// Function prototypes
TreeNode* id3(Instance *data, int num_data, char **attributes, int num_attributes, char **classes, int num_classes);
double calculate_entropy(Instance *data, int num_data, char **classes, int num_classes);
double calculate_information_gain(Instance *data, int num_data, char *attribute, char **classes, int num_classes);
int find_attribute_index(char *attribute_name);
char* find_most_common_class(Instance *data, int num_data);
TreeNode* create_leaf_node(char *class_label);
TreeNode* create_internal_node(char *attribute_name);
void free_tree(TreeNode *node);
void print_tree(TreeNode *node, int depth);

// Calculate entropy of a dataset
double calculate_entropy(Instance *data, int num_data, char **classes, int num_classes) {
    if (num_data == 0) return 0.0;
    
    // Count occurrences of each class
    int class_counts[MAX_ATTRIBUTES] = {0};
    for (int i = 0; i < num_data; i++) {
        for (int j = 0; j < num_classes; j++) {
            if (strcmp(data[i].class_label, classes[j]) == 0) {
                class_counts[j]++;
                break;
            }
        }
    }
    
    double entropy = 0.0;
    for (int i = 0; i < num_classes; i++) {
        if (class_counts[i] > 0) {
            double probability = (double)class_counts[i] / num_data;
            entropy -= probability * log2(probability);
        }
    }
    
    return entropy;
}

// Calculate information gain for an attribute
double calculate_information_gain(Instance *data, int num_data, char *attribute, char **classes, int num_classes) {
    double original_entropy = calculate_entropy(data, num_data, classes, num_classes);
    double weighted_entropy = 0.0;
    
    // Find attribute index
    int attr_index = find_attribute_index(attribute);
    if (attr_index == -1) return 0.0;
    
    // Group data by attribute values
    char *unique_values[MAX_VALUES];
    int value_count = 0;
    
    // Find unique values for this attribute
    for (int i = 0; i < num_data; i++) {
        int found = 0;
        for (int j = 0; j < value_count; j++) {
            if (strcmp(data[i].values[attr_index], unique_values[j]) == 0) {
                found = 1;
                break;
            }
        }
        if (!found && value_count < MAX_VALUES) {
            unique_values[value_count] = data[i].values[attr_index];
            value_count++;
        }
    }
    
    // Calculate weighted entropy
    for (int i = 0; i < value_count; i++) {
        // Create subset for this attribute value
        Instance subset[MAX_DATA];
        int subset_count = 0;
        
        for (int j = 0; j < num_data; j++) {
            if (strcmp(data[j].values[attr_index], unique_values[i]) == 0) {
                subset[subset_count] = data[j];
                subset_count++;
            }
        }
        
        // Calculate weighted entropy
        double subset_entropy = calculate_entropy(subset, subset_count, classes, num_classes);
        weighted_entropy += ((double)subset_count / num_data) * subset_entropy;
    }
    
    return original_entropy - weighted_entropy;
}

// Find index of attribute in attribute_names array
int find_attribute_index(char *attribute_name) {
    for (int i = 0; i < num_attributes; i++) {
        if (strcmp(attribute_names[i], attribute_name) == 0) {
            return i;
        }
    }
    return -1;
}

// Find most common class in dataset
char* find_most_common_class(Instance *data, int num_data) {
    // Count class occurrences
    int class_counts[MAX_ATTRIBUTES] = {0};
    char *class_labels[MAX_ATTRIBUTES];
    int class_count = 0;
    
    for (int i = 0; i < num_data; i++) {
        int found = 0;
        for (int j = 0; j < class_count; j++) {
            if (strcmp(data[i].class_label, class_labels[j]) == 0) {
                class_counts[j]++;
                found = 1;
                break;
            }
        }
        if (!found && class_count < MAX_ATTRIBUTES) {
            class_labels[class_count] = data[i].class_label;
            class_counts[class_count] = 1;
            class_count++;
        }
    }
    
    // Find class with maximum count
    int max_count = 0;
    int max_index = 0;
    for (int i = 0; i < class_count; i++) {
        if (class_counts[i] > max_count) {
            max_count = class_counts[i];
            max_index = i;
        }
    }
    
    return class_labels[max_index];
}

// Create a leaf node
TreeNode* create_leaf_node(char *class_label) {
    TreeNode *node = (TreeNode*)malloc(sizeof(TreeNode));
    node->attribute = NULL;
    node->value = NULL;
    node->is_leaf = 1;
    node->prediction = class_label;
    node->num_children = 0;
    return node;
}

// Create an internal node
TreeNode* create_internal_node(char *attribute_name) {
    TreeNode *node = (TreeNode*)malloc(sizeof(TreeNode));
    node->attribute = attribute_name;
    node->value = NULL;
    node->is_leaf = 0;
    node->prediction = NULL;
    node->num_children = 0;
    return node;
}

// Main ID3 algorithm
TreeNode* id3(Instance *data, int num_data, char **attributes, int num_attributes, char **classes, int num_classes) {
    // Base case: if all instances have the same class
    char *first_class = data[0].class_label;
    int all_same = 1;
    for (int i = 1; i < num_data; i++) {
        if (strcmp(data[i].class_label, first_class) != 0) {
            all_same = 0;
            break;
        }
    }
    
    if (all_same) {
        return create_leaf_node(first_class);
    }
    
    // Base case: if no attributes left
    if (num_attributes == 0) {
        char *most_common = find_most_common_class(data, num_data);
        return create_leaf_node(most_common);
    }
    
    // Find best attribute to split on
    double max_gain = -1.0;
    char *best_attribute = NULL;
    
    for (int i = 0; i < num_attributes; i++) {
        double gain = calculate_information_gain(data, num_data, attributes[i], classes, num_classes);
        if (gain > max_gain) {
            max_gain = gain;
            best_attribute = attributes[i];
        }
    }
    
    // Create internal node
    TreeNode *node = create_internal_node(best_attribute);
    
    // Find attribute index
    int attr_index = find_attribute_index(best_attribute);
    
    // Group data by attribute values
    char *unique_values[MAX_VALUES];
    int value_count = 0;
    
    for (int i = 0; i < num_data; i++) {
        int found = 0;
        for (int j = 0; j < value_count; j++) {
            if (strcmp(data[i].values[attr_index], unique_values[j]) == 0) {
                found = 1;
                break;
            }
        }
        if (!found && value_count < MAX_VALUES) {
            unique_values[value_count] = data[i].values[attr_index];
            value_count++;
        }
    }
    
    // Create children for each value
    for (int i = 0; i < value_count; i++) {
        // Create subset for this value
        Instance subset[MAX_DATA];
        int subset_count = 0;
        
        for (int j = 0; j < num_data; j++) {
            if (strcmp(data[j].values[attr_index], unique_values[i]) == 0) {
                subset[subset_count] = data[j];
                subset_count++;
            }
        }
        
        // Remove current attribute from remaining attributes
        char *remaining_attributes[MAX_ATTRIBUTES];
        int remaining_count = 0;
        
        for (int j = 0; j < num_attributes; j++) {
            if (strcmp(attributes[j], best_attribute) != 0) {
                remaining_attributes[remaining_count] = attributes[j];
                remaining_count++;
            }
        }
        
        // Recursively build subtree
        TreeNode *child = id3(subset, subset_count, remaining_attributes, remaining_count, classes, num_classes);
        child->value = unique_values[i];
        node->children[node->num_children] = child;
        node->num_children++;
    }
    
    return node;
}

// Print the decision tree
void print_tree(TreeNode *node, int depth) {
    if (node == NULL) return;
    
    for (int i = 0; i < depth; i++) {
        printf("  ");
    }
    
    if (node->is_leaf) {
        printf("Predict: %s\n", node->prediction);
    } else {
        printf("Attribute: %s\n", node->attribute);
        for (int i = 0; i < node->num_children; i++) {
            for (int j = 0; j < depth + 1; j++) {
                printf("  ");
            }
            printf("%s -> ", node->children[i]->value);
            print_tree(node->children[i], depth + 2);
        }
    }
}

// Free memory allocated for tree
void free_tree(TreeNode *node) {
    if (node == NULL) return;
    
    if (!node->is_leaf) {
        for (int i = 0; i < node->num_children; i++) {
            free_tree(node->children[i]);
        }
    }
    
    free(node);
}

// Example usage
int main() {
    // Initialize dataset (simple example)
    num_instances = 14;
    num_attributes = 4;
    num_classes = 2;
    
    // Attribute names
    attribute_names[0] = "Outlook";
    attribute_names[1] = "Temperature";
    attribute_names[2] = "Humidity";
    attribute_names[3] = "Wind";
    
    // Class names
    class_names[0] = "Yes";
    class_names[1] = "No";
    
    // Sample dataset
    dataset[0].values[0] = "Sunny"; dataset[0].values[1] = "Hot"; dataset[0].values[2] = "High"; dataset[0].values[3] = "Weak"; dataset[0].class_label = "No";
    dataset[1].values[0] = "Sunny"; dataset[1].values[1] = "Hot"; dataset[1].values[2] = "High"; dataset[1].values[3] = "Strong"; dataset[1].class_label = "No";
    dataset[2].values[0] = "Overcast"; dataset[2].values[1] = "Hot"; dataset[2].values[2] = "High"; dataset[2].values[3] = "Weak"; dataset[2].class_label = "Yes";
    dataset[3].values[0] = "Rain"; dataset[3].values[1] = "Mild"; dataset[3].values[2] = "High"; dataset[3].values[3] = "Weak"; dataset[3].class_label = "Yes";
    dataset[4].values[0] = "Rain"; dataset[4].values[1] = "Cool"; dataset[4].values[2] = "Normal"; dataset[4].values[3] = "Weak"; dataset[4].class_label = "Yes";
    dataset[5].values[0] = "Rain"; dataset[5].values[1] = "Cool"; dataset[5].values[2] = "Normal"; dataset[5].values[3] = "Strong"; dataset[5].class_label = "No";
    dataset[6].values[0] = "Overcast"; dataset[6].values[1] = "Cool"; dataset[6].values[2] = "Normal"; dataset[6].values[3] = "Strong"; dataset[6].class_label = "Yes";
    dataset[7].values[0] = "Sunny"; dataset[7].values[1] = "Mild"; dataset[7].values[2] = "High"; dataset[7].values[3] = "Weak"; dataset[7].class_label = "No";
    dataset[8].values[0] = "Sunny"; dataset[8].values[1] = "Cool"; dataset[8].values[2] = "Normal"; dataset[8].values[3] = "Weak"; dataset[8].class_label = "Yes";
    dataset[9].values[0] = "Rain"; dataset[9].values[1] = "Mild"; dataset[9].values[2] = "Normal"; dataset[9].values[3] = "Weak"; dataset[9].class_label = "Yes";
    dataset[10].values[0] = "Sunny"; dataset[10].values[1] = "Mild"; dataset[10].values[2] = "Normal"; dataset[10].values[3] = "Strong"; dataset[10].class_label = "Yes";
    dataset[11].values[0] = "Overcast"; dataset[11].values[1] = "Mild"; dataset[11].values[2] = "High"; dataset[11].values[3] = "Strong"; dataset[11].class_label = "Yes";
    dataset[12].values[0] = "Overcast"; dataset[12].values[1] = "Hot"; dataset[12].values[2] = "Normal"; dataset[12].values[3] = "Weak"; dataset[12].class_label = "Yes";
    dataset[13].values[0] = "Rain"; dataset[13].values[1] = "Mild"; dataset[13].values[2] = "High"; dataset[13].values[3] = "Strong"; dataset[13].class_label = "No";
    
    // Build decision tree
    TreeNode *tree = id3(dataset, num_instances, attribute_names, num_attributes, class_names, num_classes);
    
    // Print the tree
    printf("Decision Tree:\n");
    print_tree(tree, 0);
    
    // Free memory
    free_tree(tree);
    
    return 0;
}
```

## Key Features of this Implementation:

1. **Complete ID3 Algorithm**: Implements the full ID3 algorithm including entropy calculation and information gain
2. **Memory Management**: Proper allocation and deallocation of tree nodes
3. **Recursive Tree Building**: Builds the decision tree recursively by selecting the best attribute at each step
4. **Proper Data Structures**: Uses structs for instances and tree nodes
5. **Example Dataset**: Includes a sample tennis dataset for demonstration
6. **Tree Visualization**: Prints the decision tree in a readable format

## How to Compile and Run:

```bash
gcc -o id3_decision_tree id3_decision_tree.c -lm
./id3_decision_tree
```

## Algorithm Steps:

1. **Calculate Entropy**: Measures the impurity of the dataset
2. **Calculate Information Gain**: For each attribute, calculates how much information is gained by splitting on that attribute
3. **Select Best Attribute**: Choose the attribute with maximum information gain
4. **Create Tree Nodes**: Build internal nodes for attributes and leaf nodes for class predictions
5. **Recursive Building**: Continue building subtrees until all instances have the same class or no attributes remain

This implementation demonstrates the core concepts of the ID3 algorithm while maintaining clean, readable C code structure.

