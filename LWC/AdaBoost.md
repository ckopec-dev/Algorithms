# AdaBoost Algorithm in LWC

AdaBoost (Adaptive Boosting) is a machine learning algorithm that combines multiple weak learners to create a strong learner. Here's an implementation in LWC (Lightweight C):

```lwc
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Structure for a weak learner (decision stump)
typedef struct {
    int feature_index;
    double threshold;
    int polarity;  // 1 or -1
} DecisionStump;

// Structure for AdaBoost model
typedef struct {
    DecisionStump* classifiers;
    double* alphas;
    int num_classifiers;
} AdaBoostModel;

// Calculate weighted error for a weak learner
double calculate_weighted_error(double* weights, int* predictions, int* actual, int size) {
    double error = 0.0;
    for (int i = 0; i < size; i++) {
        if (predictions[i] != actual[i]) {
            error += weights[i];
        }
    }
    return error;
}

// Create a decision stump (weak learner)
DecisionStump* create_decision_stump(double** features, int* labels, double* weights, 
                                   int num_samples, int num_features) {
    DecisionStump* stump = malloc(sizeof(DecisionStump));
    double best_error = 1.0;
    int best_feature = 0;
    double best_threshold = 0.0;
    int best_polarity = 1;
    
    // Try different features and thresholds
    for (int f = 0; f < num_features; f++) {
        double min_val = features[0][f];
        double max_val = features[0][f];
        
        // Find min and max values for this feature
        for (int i = 1; i < num_samples; i++) {
            if (features[i][f] < min_val) min_val = features[i][f];
            if (features[i][f] > max_val) max_val = features[i][f];
        }
        
        // Try different thresholds
        for (int t = 0; t < 10; t++) {
            double threshold = min_val + (max_val - min_val) * t / 9.0;
            
            // Try both polarities
            for (int polarity = -1; polarity <= 1; polarity += 2) {
                int* predictions = malloc(num_samples * sizeof(int));
                
                // Make predictions
                for (int i = 0; i < num_samples; i++) {
                    if (polarity * features[i][f] < polarity * threshold) {
                        predictions[i] = -1;
                    } else {
                        predictions[i] = 1;
                    }
                }
                
                // Calculate error
                double error = calculate_weighted_error(weights, predictions, labels, num_samples);
                
                if (error < best_error) {
                    best_error = error;
                    best_feature = f;
                    best_threshold = threshold;
                    best_polarity = polarity;
                }
                
                free(predictions);
            }
        }
    }
    
    stump->feature_index = best_feature;
    stump->threshold = best_threshold;
    stump->polarity = best_polarity;
    
    return stump;
}

// Train AdaBoost model
AdaBoostModel* train_adaboost(double** features, int* labels, double* weights,
                            int num_samples, int num_features, int num_iterations) {
    AdaBoostModel* model = malloc(sizeof(AdaBoostModel));
    model->num_classifiers = num_iterations;
    model->classifiers = malloc(num_iterations * sizeof(DecisionStump));
    model->alphas = malloc(num_iterations * sizeof(double));
    
    // Initialize weights
    for (int i = 0; i < num_samples; i++) {
        weights[i] = 1.0 / num_samples;
    }
    
    for (int t = 0; t < num_iterations; t++) {
        // Create weak learner
        DecisionStump* stump = create_decision_stump(features, labels, weights, 
                                                   num_samples, num_features);
        model->classifiers[t] = *stump;
        
        // Make predictions with current weak learner
        int* predictions = malloc(num_samples * sizeof(int));
        for (int i = 0; i < num_samples; i++) {
            if (model->classifiers[t].polarity * features[i][model->classifiers[t].feature_index] 
                < model->classifiers[t].polarity * model->classifiers[t].threshold) {
                predictions[i] = -1;
            } else {
                predictions[i] = 1;
            }
        }
        
        // Calculate error
        double error = calculate_weighted_error(weights, predictions, labels, num_samples);
        
        // Calculate alpha (learner weight)
        if (error > 0.0 && error < 0.5) {
            model->alphas[t] = 0.5 * log((1.0 - error) / error);
        } else {
            model->alphas[t] = 0.0;
        }
        
        // Update weights
        double Z = 0.0;
        for (int i = 0; i < num_samples; i++) {
            if (predictions[i] == labels[i]) {
                weights[i] *= exp(-model->alphas[t]);
            } else {
                weights[i] *= exp(model->alphas[t]);
            }
            Z += weights[i];
        }
        
        // Normalize weights
        for (int i = 0; i < num_samples; i++) {
            weights[i] /= Z;
        }
        
        free(predictions);
        free(stump);
    }
    
    return model;
}

// Predict using AdaBoost model
int predict(AdaBoostModel* model, double* features) {
    double sum = 0.0;
    
    for (int i = 0; i < model->num_classifiers; i++) {
        int prediction = 1;
        if (model->classifiers[i].polarity * features[model->classifiers[i].feature_index] 
            < model->classifiers[i].polarity * model->classifiers[i].threshold) {
            prediction = -1;
        }
        sum += model->alphas[i] * prediction;
    }
    
    return (sum >= 0) ? 1 : -1;
}

// Example usage
int main() {
    // Sample dataset: 4 samples, 2 features each
    double features[4][2] = {{1.0, 2.0}, {2.0, 3.0}, {3.0, 1.0}, {4.0, 4.0}};
    int labels[4] = {1, 1, -1, -1};
    double weights[4] = {0.25, 0.25, 0.25, 0.25};
    
    // Train AdaBoost
    AdaBoostModel* model = train_adaboost(features, labels, weights, 4, 2, 3);
    
    // Make predictions
    printf("Predictions:\n");
    for (int i = 0; i < 4; i++) {
        int prediction = predict(model, features[i]);
        printf("Sample %d: predicted %d\n", i+1, prediction);
    }
    
    // Clean up
    free(model->classifiers);
    free(model->alphas);
    free(model);
    
    return 0;
}
```

## Key Components Explained:

### 1. **DecisionStump Structure**
- Represents a weak learner (decision stump)
- Contains feature index, threshold value, and polarity

### 2. **Training Process**
- Iteratively creates weak learners
- Updates sample weights based on previous learner performance
- Calculates alpha (learner importance) using exponential weighting

### 3. **Prediction Function**
- Combines predictions from all weak learners
- Uses weighted voting based on alpha values

### 4. **Key Features**
- Uses decision stumps as weak learners
- Implements weighted sampling
- Supports binary classification
- Adaptive learning with exponential weights

This implementation demonstrates the core AdaBoost algorithm principles in LWC, showing how multiple weak learners are combined to create a strong classifier through iterative boosting.

